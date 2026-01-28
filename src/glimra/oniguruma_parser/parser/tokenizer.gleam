/// Tokenizer for Oniguruma regex patterns.
/// Converts a pattern string into a list of tokens.
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import glimra/oniguruma_parser/parser/ast_types.{
  type FlagGroupModifiers, type NamedCalloutKind, Cmp, Count, Custom,
  ErrorCallout, Fail, FlagGroupModifiers, FlagGroupSwitches, Max, Mismatch, Skip,
  TotalCount,
}
import glimra/oniguruma_parser/parser/token_types.{
  type FlagProperties, type Token, type TokenCharacterSetKind,
  type TokenOrIntermediate, type TokenizeResult, AbsenceRepeater,
  AlternatorToken, AssertionToken, Atomic, BackreferenceToken, Capturing,
  CharacterClassCloseToken, CharacterClassHyphenToken,
  CharacterClassIntersectorToken, CharacterClassOpenToken, CharacterSetToken,
  CharacterToken, DirectiveToken, EscapedNumberToken, FlagProperties,
  GroupCloseToken, GroupKind, GroupOpenToken, IntermediateToken, LookaheadKind,
  LookbehindKind, NamedCalloutToken, QuantifierToken, RegularToken,
  SubroutineToken, TokAny, TokDigit, TokDot, TokFlags, TokGreedy, TokHex,
  TokKeep, TokLazy, TokNewline, TokPosix, TokPossessive, TokProperty, TokSpace,
  TokTextSegment, TokWord, TokenizeResult,
}

// ============================================================================
// Tokenizer Options
// ============================================================================

/// Options for tokenizing
pub type TokenizeOptions {
  TokenizeOptions(flags: String, capture_group: Bool, singleline: Bool)
}

/// Default tokenize options
pub fn default_options() -> TokenizeOptions {
  TokenizeOptions(flags: "", capture_group: False, singleline: False)
}

// ============================================================================
// Tokenizer Context
// ============================================================================

/// Internal context for tokenization
type Context {
  Context(
    pattern: String,
    graphemes: List(String),
    pos: Int,
    capture_group: Bool,
    singleline: Bool,
    x_stack: List(Bool),
    num_open_groups: Int,
  )
}

// ============================================================================
// Loop State Types (for stack-safe iterative tokenization)
// ============================================================================

/// Result of a single loop step - either continue looping or finish
type TokLoopStep(state, result) {
  TokLoopContinue(state)
  TokLoopDone(result)
}

/// State for main tokenization loop
type TokLoopState {
  TokLoopState(ctx: Context, acc: List(TokenOrIntermediate))
}

/// State for character class contents tokenization loop
type CharClassTokState {
  CharClassTokState(ctx: Context, depth: Int, acc: List(TokenOrIntermediate))
}

/// Get current mod x (extended) flag value
fn get_current_mod_x(ctx: Context) -> Bool {
  case ctx.x_stack {
    [x, ..] -> x
    [] -> False
  }
}

/// Push a mod x value
fn push_mod_x(ctx: Context, is_x_on: Bool) -> Context {
  Context(..ctx, x_stack: [is_x_on, ..ctx.x_stack])
}

/// Pop a mod x value
fn pop_mod_x(ctx: Context) -> Context {
  Context(..ctx, x_stack: case ctx.x_stack {
    [_, ..rest] -> rest
    [] -> []
  })
}

/// Replace current mod x value
fn replace_current_mod_x(ctx: Context, is_x_on: Bool) -> Context {
  Context(..ctx, x_stack: case ctx.x_stack {
    [_, ..rest] -> [is_x_on, ..rest]
    [] -> [is_x_on]
  })
}

// ============================================================================
// Main Tokenize Function
// ============================================================================

/// Tokenize a pattern string into tokens
pub fn tokenize(
  pattern: String,
  options: TokenizeOptions,
) -> Result(TokenizeResult, String) {
  let flag_props = get_flag_properties(options.flags)

  let ctx =
    Context(
      pattern: pattern,
      graphemes: string.to_graphemes(pattern),
      pos: 0,
      capture_group: options.capture_group,
      singleline: options.singleline,
      x_stack: [flag_props.extended],
      num_open_groups: 0,
    )

  use #(tokens_intermediate, _final_ctx) <- result.try(tokenize_loop(ctx, []))

  // Second pass: assign capture group numbers and convert EscapedNumber tokens
  let #(final_tokens, num_captures) =
    process_capture_groups(tokens_intermediate, options.capture_group)

  // Convert escaped number tokens to proper backrefs or characters
  use final_tokens <- result.try(convert_escaped_numbers(
    final_tokens,
    num_captures,
  ))

  Ok(TokenizeResult(tokens: final_tokens, flags: flag_props))
}

/// Main tokenization loop - iterative version
fn tokenize_loop(
  ctx: Context,
  acc: List(TokenOrIntermediate),
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  do_tokenize_loop(TokLoopState(ctx: ctx, acc: acc))
}

/// Single step of tokenization
fn tokenize_loop_step(
  state: TokLoopState,
) -> TokLoopStep(
  TokLoopState,
  Result(#(List(TokenOrIntermediate), Context), String),
) {
  let TokLoopState(ctx: ctx, acc: acc) = state
  case peek_char(ctx) {
    None -> TokLoopDone(Ok(#(list.reverse(acc), ctx)))
    Some(char) -> {
      case tokenize_one(ctx, char) {
        Error(e) -> TokLoopDone(Error(e))
        Ok(#(result_tokens, new_ctx)) -> {
          let new_acc = list.append(list.reverse(result_tokens), acc)
          TokLoopContinue(TokLoopState(ctx: new_ctx, acc: new_acc))
        }
      }
    }
  }
}

/// Tail-recursive loop driver for tokenization
fn do_tokenize_loop(
  state: TokLoopState,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  case tokenize_loop_step(state) {
    TokLoopDone(result) -> result
    TokLoopContinue(new_state) -> do_tokenize_loop(new_state)
  }
}

/// Tokenize a single element at current position
fn tokenize_one(
  ctx: Context,
  char: String,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  case char {
    // Character class opening
    "[" -> tokenize_char_class(ctx)

    // Escape sequence
    "\\" -> tokenize_escape(ctx, False)

    // Group opening
    "(" -> tokenize_group_open(ctx)

    // Group closing
    ")" -> tokenize_group_close(ctx)

    // In extended mode, handle comments and whitespace
    "#" -> {
      case get_current_mod_x(ctx) {
        True -> tokenize_comment(ctx)
        False -> {
          let cp = string_to_codepoint("#")
          let new_ctx = advance(ctx, 1)
          Ok(#([RegularToken(CharacterToken(value: cp, raw: "#"))], new_ctx))
        }
      }
    }

    // Whitespace in extended mode
    " " | "\t" | "\n" | "\r" -> {
      case get_current_mod_x(ctx) {
        True -> tokenize_whitespace(ctx)
        False -> {
          let cp = string_to_codepoint(char)
          let new_ctx = advance(ctx, 1)
          Ok(#([RegularToken(CharacterToken(value: cp, raw: char))], new_ctx))
        }
      }
    }

    // Dot (any character)
    "." -> {
      let new_ctx = advance(ctx, 1)
      Ok(#(
        [
          RegularToken(CharacterSetToken(
            kind: TokDot,
            value: None,
            negate: None,
            raw: ".",
          )),
        ],
        new_ctx,
      ))
    }

    // Assertions
    "^" -> {
      let kind = case ctx.singleline {
        True -> "\\A"
        False -> "^"
      }
      let new_ctx = advance(ctx, 1)
      Ok(#([RegularToken(AssertionToken(kind: kind, raw: "^"))], new_ctx))
    }

    "$" -> {
      let kind = case ctx.singleline {
        True -> "\\Z"
        False -> "$"
      }
      let new_ctx = advance(ctx, 1)
      Ok(#([RegularToken(AssertionToken(kind: kind, raw: "$"))], new_ctx))
    }

    // Alternation
    "|" -> {
      let new_ctx = advance(ctx, 1)
      Ok(#([RegularToken(AlternatorToken(raw: "|"))], new_ctx))
    }

    // Quantifiers
    "?" | "*" | "+" -> tokenize_quantifier(ctx, char)

    // Interval quantifier
    "{" -> tokenize_interval_quantifier(ctx)

    // Regular character
    _ -> {
      let cp = string_to_codepoint(char)
      let new_ctx = advance(ctx, 1)
      Ok(#([RegularToken(CharacterToken(value: cp, raw: char))], new_ctx))
    }
  }
}

// ============================================================================
// Character Class Tokenization
// ============================================================================

/// Tokenize a character class [...]
fn tokenize_char_class(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let #(negate, raw, ctx2) = case peek_char_at(ctx, 1) {
    Some("^") -> #(True, "[^", advance(ctx, 2))
    _ -> #(False, "[", advance(ctx, 1))
  }

  let open_token = CharacterClassOpenToken(negate: negate, raw: raw)

  // Tokenize contents of character class
  use #(inner_tokens, ctx3) <- result.try(
    tokenize_char_class_contents(ctx2, 1, []),
  )

  Ok(#(list.flatten([[RegularToken(open_token)], inner_tokens]), ctx3))
}

/// Tokenize contents of a character class - iterative version
fn tokenize_char_class_contents(
  ctx: Context,
  depth: Int,
  acc: List(TokenOrIntermediate),
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  do_char_class_tok_loop(CharClassTokState(ctx: ctx, depth: depth, acc: acc))
}

/// Single step of character class tokenization
fn char_class_tok_step(
  state: CharClassTokState,
) -> TokLoopStep(
  CharClassTokState,
  Result(#(List(TokenOrIntermediate), Context), String),
) {
  let CharClassTokState(ctx: ctx, depth: depth, acc: acc) = state

  case peek_char(ctx) {
    None -> TokLoopDone(Error("Unclosed character class"))

    Some("]") -> {
      // Check if this is the first character (literal ])
      let is_first = case acc {
        [] -> True
        [RegularToken(CharacterClassOpenToken(..))] -> True
        _ -> False
      }

      case is_first {
        True -> {
          // Literal ] at start
          let cp = string_to_codepoint("]")
          let new_ctx = advance(ctx, 1)
          TokLoopContinue(
            CharClassTokState(ctx: new_ctx, depth: depth, acc: [
              RegularToken(CharacterToken(value: cp, raw: "]")),
              ..acc
            ]),
          )
        }
        False -> {
          // Closing bracket
          let new_depth = depth - 1
          let new_ctx = advance(ctx, 1)
          case new_depth {
            0 ->
              TokLoopDone(
                Ok(#(
                  list.reverse([
                    RegularToken(CharacterClassCloseToken(raw: "]")),
                    ..acc
                  ]),
                  new_ctx,
                )),
              )
            _ ->
              TokLoopContinue(
                CharClassTokState(ctx: new_ctx, depth: new_depth, acc: [
                  RegularToken(CharacterClassCloseToken(raw: "]")),
                  ..acc
                ]),
              )
          }
        }
      }
    }

    Some("[") -> {
      // Check for nested char class or POSIX class
      case peek_char_at(ctx, 1) {
        Some(":") -> {
          // POSIX class
          case tokenize_posix_class(ctx) {
            Error(e) -> TokLoopDone(Error(e))
            Ok(#(token, new_ctx)) ->
              TokLoopContinue(
                CharClassTokState(ctx: new_ctx, depth: depth, acc: [
                  RegularToken(token),
                  ..acc
                ]),
              )
          }
        }
        Some("^") -> {
          // Nested negated char class
          let new_ctx = advance(ctx, 2)
          TokLoopContinue(
            CharClassTokState(ctx: new_ctx, depth: depth + 1, acc: [
              RegularToken(CharacterClassOpenToken(negate: True, raw: "[^")),
              ..acc
            ]),
          )
        }
        _ -> {
          // Nested char class
          let new_ctx = advance(ctx, 1)
          TokLoopContinue(
            CharClassTokState(ctx: new_ctx, depth: depth + 1, acc: [
              RegularToken(CharacterClassOpenToken(negate: False, raw: "[")),
              ..acc
            ]),
          )
        }
      }
    }

    Some("\\") -> {
      case tokenize_escape(ctx, True) {
        Error(e) -> TokLoopDone(Error(e))
        Ok(#(tokens, new_ctx)) ->
          TokLoopContinue(CharClassTokState(
            ctx: new_ctx,
            depth: depth,
            acc: list.append(list.reverse(tokens), acc),
          ))
      }
    }

    Some("-") -> {
      let new_ctx = advance(ctx, 1)
      TokLoopContinue(
        CharClassTokState(ctx: new_ctx, depth: depth, acc: [
          RegularToken(CharacterClassHyphenToken(raw: "-")),
          ..acc
        ]),
      )
    }

    Some("&") -> {
      case peek_char_at(ctx, 1) {
        Some("&") -> {
          let new_ctx = advance(ctx, 2)
          TokLoopContinue(
            CharClassTokState(ctx: new_ctx, depth: depth, acc: [
              RegularToken(CharacterClassIntersectorToken(raw: "&&")),
              ..acc
            ]),
          )
        }
        _ -> {
          let cp = string_to_codepoint("&")
          let new_ctx = advance(ctx, 1)
          TokLoopContinue(
            CharClassTokState(ctx: new_ctx, depth: depth, acc: [
              RegularToken(CharacterToken(value: cp, raw: "&")),
              ..acc
            ]),
          )
        }
      }
    }

    Some(char) -> {
      let cp = string_to_codepoint(char)
      let new_ctx = advance(ctx, 1)
      TokLoopContinue(
        CharClassTokState(ctx: new_ctx, depth: depth, acc: [
          RegularToken(CharacterToken(value: cp, raw: char)),
          ..acc
        ]),
      )
    }
  }
}

/// Tail-recursive loop driver for character class tokenization
fn do_char_class_tok_loop(
  state: CharClassTokState,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  case char_class_tok_step(state) {
    TokLoopDone(result) -> result
    TokLoopContinue(new_state) -> do_char_class_tok_loop(new_state)
  }
}

/// Tokenize a POSIX class like [:alpha:]
fn tokenize_posix_class(ctx: Context) -> Result(#(Token, Context), String) {
  // Skip the opening "[:"
  let ctx2 = advance(ctx, 2)

  // Check for negation
  let #(negate, ctx3) = case peek_char(ctx2) {
    Some("^") -> #(True, advance(ctx2, 1))
    _ -> #(False, ctx2)
  }

  // Read the class name
  let #(name, ctx4) = read_while(ctx3, is_alpha)

  // Expect closing ":]"
  case peek_chars(ctx4, 2) {
    ":]" -> {
      let raw =
        "[:"
        <> case negate {
          True -> "^"
          False -> ""
        }
        <> name
        <> ":]"
      let new_ctx = advance(ctx4, 2)
      Ok(#(
        CharacterSetToken(
          kind: TokPosix,
          value: Some(name),
          negate: Some(negate),
          raw: raw,
        ),
        new_ctx,
      ))
    }
    _ -> Error("Invalid POSIX class")
  }
}

// ============================================================================
// Escape Sequence Tokenization
// ============================================================================

/// Tokenize an escape sequence
fn tokenize_escape(
  ctx: Context,
  in_char_class: Bool,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  case peek_char_at(ctx, 1) {
    None -> Error("Incomplete escape \"\\\"")

    Some(char2) -> {
      case char2 {
        // Assertions (not in char class)
        "A" | "b" | "B" | "G" | "y" | "Y" | "z" | "Z" if !in_char_class -> {
          let raw = "\\" <> char2
          let new_ctx = advance(ctx, 2)
          Ok(#([RegularToken(AssertionToken(kind: raw, raw: raw))], new_ctx))
        }

        // Subroutine
        "g" if !in_char_class -> tokenize_subroutine_or_backref(ctx, "g")

        // Backreference with k wrapper
        "k" if !in_char_class -> tokenize_subroutine_or_backref(ctx, "k")

        // Keep directive
        "K" if !in_char_class -> {
          let new_ctx = advance(ctx, 2)
          Ok(#(
            [
              RegularToken(DirectiveToken(
                kind: TokKeep,
                flags: None,
                raw: "\\K",
              )),
            ],
            new_ctx,
          ))
        }

        // Newline sets
        "N" | "R" if !in_char_class -> {
          let raw = "\\" <> char2
          let negate = char2 == "N"
          let new_ctx = advance(ctx, 2)
          Ok(#(
            [
              RegularToken(CharacterSetToken(
                kind: TokNewline,
                value: None,
                negate: Some(negate),
                raw: raw,
              )),
            ],
            new_ctx,
          ))
        }

        // Any character \O
        "O" if !in_char_class -> {
          let new_ctx = advance(ctx, 2)
          Ok(#(
            [
              RegularToken(CharacterSetToken(
                kind: TokAny,
                value: None,
                negate: None,
                raw: "\\O",
              )),
            ],
            new_ctx,
          ))
        }

        // Text segment \X
        "X" if !in_char_class -> {
          let new_ctx = advance(ctx, 2)
          Ok(#(
            [
              RegularToken(CharacterSetToken(
                kind: TokTextSegment,
                value: None,
                negate: None,
                raw: "\\X",
              )),
            ],
            new_ctx,
          ))
        }

        // Character shorthands (both in and out of char class)
        "d" | "D" | "h" | "H" | "s" | "S" | "w" | "W" -> {
          let #(kind, negate) = shorthand_to_kind(char2)
          let raw = "\\" <> char2
          let new_ctx = advance(ctx, 2)
          Ok(#(
            [
              RegularToken(CharacterSetToken(
                kind: kind,
                value: None,
                negate: Some(negate),
                raw: raw,
              )),
            ],
            new_ctx,
          ))
        }

        // Unicode property
        "p" | "P" -> tokenize_unicode_property(ctx)

        // Hex character
        "x" -> tokenize_hex_escape(ctx)

        // Unicode escape
        "u" -> tokenize_unicode_escape(ctx)

        // Octal escape
        "o" -> tokenize_octal_escape(ctx)

        // Control character
        "c" | "C" -> tokenize_control_char(ctx)

        // Escaped number (backref or octal or literal)
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
          tokenize_escaped_number(ctx, in_char_class)

        // Named escape characters
        "a" -> {
          let new_ctx = advance(ctx, 2)
          Ok(#([RegularToken(CharacterToken(value: 7, raw: "\\a"))], new_ctx))
        }
        "e" -> {
          let new_ctx = advance(ctx, 2)
          Ok(#([RegularToken(CharacterToken(value: 27, raw: "\\e"))], new_ctx))
        }
        "f" -> {
          let new_ctx = advance(ctx, 2)
          Ok(#([RegularToken(CharacterToken(value: 12, raw: "\\f"))], new_ctx))
        }
        "n" -> {
          let new_ctx = advance(ctx, 2)
          Ok(#([RegularToken(CharacterToken(value: 10, raw: "\\n"))], new_ctx))
        }
        "r" -> {
          let new_ctx = advance(ctx, 2)
          Ok(#([RegularToken(CharacterToken(value: 13, raw: "\\r"))], new_ctx))
        }
        "t" -> {
          let new_ctx = advance(ctx, 2)
          Ok(#([RegularToken(CharacterToken(value: 9, raw: "\\t"))], new_ctx))
        }
        "v" -> {
          let new_ctx = advance(ctx, 2)
          Ok(#([RegularToken(CharacterToken(value: 11, raw: "\\v"))], new_ctx))
        }

        // Identity escape (any other character)
        _ -> {
          let cp = string_to_codepoint(char2)
          let raw = "\\" <> char2
          let new_ctx = advance(ctx, 2)
          Ok(#([RegularToken(CharacterToken(value: cp, raw: raw))], new_ctx))
        }
      }
    }
  }
}

/// Convert shorthand character to kind and negate flag
fn shorthand_to_kind(char: String) -> #(TokenCharacterSetKind, Bool) {
  case char {
    "d" -> #(TokDigit, False)
    "D" -> #(TokDigit, True)
    "h" -> #(TokHex, False)
    "H" -> #(TokHex, True)
    "s" -> #(TokSpace, False)
    "S" -> #(TokSpace, True)
    "w" -> #(TokWord, False)
    "W" -> #(TokWord, True)
    _ -> #(TokWord, False)
  }
}

/// Tokenize subroutine \g<...> or backreference \k<...>
fn tokenize_subroutine_or_backref(
  ctx: Context,
  kind: String,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 2)
  // Expecting < or '
  case peek_char(ctx2) {
    Some("<") -> {
      let ctx3 = advance(ctx2, 1)
      let #(name, ctx4) = read_until(ctx3, ">")
      case peek_char(ctx4) {
        Some(">") -> {
          let raw = "\\" <> kind <> "<" <> name <> ">"
          let new_ctx = advance(ctx4, 1)
          let token = case kind {
            "g" -> SubroutineToken(raw: raw)
            _ -> BackreferenceToken(raw: raw)
          }
          Ok(#([RegularToken(token)], new_ctx))
        }
        _ ->
          Error("Invalid group name \"" <> "\\" <> kind <> "<" <> name <> "\"")
      }
    }
    Some("'") -> {
      let ctx3 = advance(ctx2, 1)
      let #(name, ctx4) = read_until(ctx3, "'")
      case peek_char(ctx4) {
        Some("'") -> {
          let raw = "\\" <> kind <> "'" <> name <> "'"
          let new_ctx = advance(ctx4, 1)
          let token = case kind {
            "g" -> SubroutineToken(raw: raw)
            _ -> BackreferenceToken(raw: raw)
          }
          Ok(#([RegularToken(token)], new_ctx))
        }
        _ ->
          Error("Invalid group name \"" <> "\\" <> kind <> "'" <> name <> "\"")
      }
    }
    _ -> Error("Invalid group name \"" <> "\\" <> kind <> "\"")
  }
}

/// Tokenize Unicode property \p{...} or \P{...}
fn tokenize_unicode_property(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let char2 = case peek_char_at(ctx, 1) {
    Some(c) -> c
    None -> ""
  }
  let ctx2 = advance(ctx, 2)

  case peek_char(ctx2) {
    Some("{") -> {
      let ctx3 = advance(ctx2, 1)

      // Check for negation with ^
      let #(inner_negate, ctx4) = case peek_char(ctx3) {
        Some("^") -> #(True, advance(ctx3, 1))
        _ -> #(False, ctx3)
      }

      let #(value, ctx5) = read_until(ctx4, "}")
      case peek_char(ctx5) {
        Some("}") -> {
          let negate = case char2 {
            "P" -> !inner_negate
            _ -> inner_negate
          }
          let neg_str = case inner_negate {
            True -> "^"
            False -> ""
          }
          let raw = "\\" <> char2 <> "{" <> neg_str <> value <> "}"
          let new_ctx = advance(ctx5, 1)
          Ok(#(
            [
              RegularToken(CharacterSetToken(
                kind: TokProperty,
                value: Some(value),
                negate: Some(negate),
                raw: raw,
              )),
            ],
            new_ctx,
          ))
        }
        _ ->
          Error(
            "Incomplete or invalid Unicode property \"\\p{" <> value <> "\"",
          )
      }
    }
    _ -> Error("Incomplete or invalid Unicode property \"\\p\"")
  }
}

/// Tokenize hex escape \xNN or \x{NNNN}
fn tokenize_hex_escape(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 2)
  // \x followed by
  case peek_char(ctx2) {
    Some("{") -> {
      // \x{NNNN...}
      let ctx3 = advance(ctx2, 1)
      let #(hex, ctx4) = read_while(ctx3, is_hex_digit)
      case peek_char(ctx4) {
        Some("}") -> {
          let raw = "\\x{" <> hex <> "}"
          let new_ctx = advance(ctx4, 1)
          use cp <- result.try(
            int.base_parse(hex, 16)
            |> result.replace_error("Invalid hex escape \"" <> raw <> "\""),
          )
          Ok(#([RegularToken(CharacterToken(value: cp, raw: raw))], new_ctx))
        }
        _ -> Error("Incomplete or invalid escape \"\\x{" <> hex <> "\"")
      }
    }
    Some(c1) -> {
      case is_hex_digit(c1) {
        True -> {
          // \xNN - one or two hex digits
          let #(hex, ctx3) = read_hex_digits(ctx2, 2)
          let raw = "\\x" <> hex
          use cp <- result.try(
            int.base_parse(hex, 16)
            |> result.replace_error("Invalid hex escape \"" <> raw <> "\""),
          )
          Ok(#([RegularToken(CharacterToken(value: cp, raw: raw))], ctx3))
        }
        False -> {
          // Bare \x - treat as literal x in some contexts, error in others
          // For compatibility, treat as null character like Onig does
          Ok(#([RegularToken(CharacterToken(value: 0, raw: "\\x"))], ctx2))
        }
      }
    }
    None -> {
      // Bare \x - treat as literal x in some contexts, error in others
      // For compatibility, treat as null character like Onig does
      Ok(#([RegularToken(CharacterToken(value: 0, raw: "\\x"))], ctx2))
    }
  }
}

/// Tokenize unicode escape \uNNNN
fn tokenize_unicode_escape(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 2)
  let #(hex, ctx3) = read_hex_digits(ctx2, 4)
  case string.length(hex) {
    4 -> {
      let raw = "\\u" <> hex
      use cp <- result.try(
        int.base_parse(hex, 16)
        |> result.replace_error("Invalid unicode escape \"" <> raw <> "\""),
      )
      Ok(#([RegularToken(CharacterToken(value: cp, raw: raw))], ctx3))
    }
    _ -> Error("Incomplete or invalid escape \"\\u" <> hex <> "\"")
  }
}

/// Tokenize octal escape \o{NNN}
fn tokenize_octal_escape(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 2)
  case peek_char(ctx2) {
    Some("{") -> {
      let ctx3 = advance(ctx2, 1)
      let #(oct, ctx4) = read_while(ctx3, is_octal_digit)
      case peek_char(ctx4) {
        Some("}") -> {
          let raw = "\\o{" <> oct <> "}"
          let new_ctx = advance(ctx4, 1)
          use cp <- result.try(
            int.base_parse(oct, 8)
            |> result.replace_error("Invalid octal escape \"" <> raw <> "\""),
          )
          Ok(#([RegularToken(CharacterToken(value: cp, raw: raw))], new_ctx))
        }
        _ ->
          Error(
            "Incomplete, invalid, or unsupported octal code point \"\\o{"
            <> oct
            <> "\"",
          )
      }
    }
    _ -> Error("Incomplete, invalid, or unsupported octal code point \"\\o\"")
  }
}

/// Tokenize control character \cX or \C-X
fn tokenize_control_char(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let char2 = case peek_char_at(ctx, 1) {
    Some(c) -> c
    None -> ""
  }

  case char2 {
    "c" -> {
      // \cX format
      case peek_char_at(ctx, 2) {
        Some(char3) -> {
          case is_alpha(char3) {
            True -> {
              let cp = string_to_codepoint(string.uppercase(char3)) - 64
              let raw = "\\c" <> char3
              let new_ctx = advance(ctx, 3)
              Ok(#([RegularToken(CharacterToken(value: cp, raw: raw))], new_ctx))
            }
            False -> Error("Unsupported control character \"\\c\"")
          }
        }
        None -> Error("Unsupported control character \"\\c\"")
      }
    }
    "C" -> {
      // \C-X format
      case peek_char_at(ctx, 2) {
        Some("-") -> {
          case peek_char_at(ctx, 3) {
            Some(char4) -> {
              case is_alpha(char4) {
                True -> {
                  let cp = string_to_codepoint(string.uppercase(char4)) - 64
                  let raw = "\\C-" <> char4
                  let new_ctx = advance(ctx, 4)
                  Ok(#(
                    [RegularToken(CharacterToken(value: cp, raw: raw))],
                    new_ctx,
                  ))
                }
                False -> Error("Unsupported control character \"\\C-\"")
              }
            }
            None -> Error("Unsupported control character \"\\C-\"")
          }
        }
        _ -> Error("Unsupported control character \"\\C\"")
      }
    }
    _ -> Error("Unexpected control character escape")
  }
}

/// Tokenize escaped number (could be backref, octal, or literal)
fn tokenize_escaped_number(
  ctx: Context,
  in_char_class: Bool,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 1)
  // \0 through \9
  let #(digits, ctx3) = read_while(ctx2, is_digit)
  let raw = "\\" <> digits
  // Create intermediate token to be resolved later
  Ok(#(
    [
      IntermediateToken(EscapedNumberToken(
        in_char_class: in_char_class,
        raw: raw,
      )),
    ],
    ctx3,
  ))
}

// ============================================================================
// Group Tokenization
// ============================================================================

/// Tokenize a group opening
fn tokenize_group_open(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  case peek_char_at(ctx, 1) {
    // Named callout (*NAME...)
    Some("*") -> tokenize_named_callout(ctx)

    // Comment group (?#...)
    Some("?") -> {
      let ctx2 = advance(ctx, 2)
      case peek_char(ctx2) {
        // Comment
        Some("#") -> tokenize_comment_group(ctx)

        // Various group types
        Some(":") -> {
          // Non-capturing group (?:...)
          let mod_x = get_current_mod_x(ctx)
          let new_ctx =
            ctx
            |> advance(3)
            |> push_mod_x(mod_x)
            |> increment_open_groups
          Ok(#(
            [
              RegularToken(GroupOpenToken(
                kind: GroupKind,
                flags: None,
                name: None,
                number: None,
                negate: None,
                raw: "(?:",
              )),
            ],
            new_ctx,
          ))
        }

        Some("=") -> {
          // Positive lookahead (?=...)
          let mod_x = get_current_mod_x(ctx)
          let new_ctx =
            ctx
            |> advance(3)
            |> push_mod_x(mod_x)
            |> increment_open_groups
          Ok(#(
            [
              RegularToken(GroupOpenToken(
                kind: LookaheadKind,
                flags: None,
                name: None,
                number: None,
                negate: Some(False),
                raw: "(?=",
              )),
            ],
            new_ctx,
          ))
        }

        Some("!") -> {
          // Negative lookahead (?!...)
          let mod_x = get_current_mod_x(ctx)
          let new_ctx =
            ctx
            |> advance(3)
            |> push_mod_x(mod_x)
            |> increment_open_groups
          Ok(#(
            [
              RegularToken(GroupOpenToken(
                kind: LookaheadKind,
                flags: None,
                name: None,
                number: None,
                negate: Some(True),
                raw: "(?!",
              )),
            ],
            new_ctx,
          ))
        }

        Some(">") -> {
          // Atomic group (?>...)
          let mod_x = get_current_mod_x(ctx)
          let new_ctx =
            ctx
            |> advance(3)
            |> push_mod_x(mod_x)
            |> increment_open_groups
          Ok(#(
            [
              RegularToken(GroupOpenToken(
                kind: Atomic,
                flags: None,
                name: None,
                number: None,
                negate: None,
                raw: "(?>",
              )),
            ],
            new_ctx,
          ))
        }

        Some("<") -> tokenize_lookbehind_or_named_group(ctx)

        Some("'") -> tokenize_named_group_single_quote(ctx)

        Some("~") -> tokenize_absence_function(ctx)

        Some("(") -> Error("Unsupported conditional \"(?(\"")

        Some("{") -> Error("Unsupported callout \"(?{\"")

        Some(")") -> {
          // Empty group (?)
          let mod_x = get_current_mod_x(ctx)
          let new_ctx =
            ctx
            |> advance(2)
            |> push_mod_x(mod_x)
            |> increment_open_groups
          Ok(#(
            [
              RegularToken(GroupOpenToken(
                kind: GroupKind,
                flags: None,
                name: None,
                number: None,
                negate: None,
                raw: "(?",
              )),
            ],
            new_ctx,
          ))
        }

        // Flag modifier (?imx...) or (?imx-xyz:) or (?imx-xyz)
        Some(c) -> {
          case is_flag_char(c) || c == "-" {
            True -> tokenize_flag_modifier(ctx)
            False -> Error("Invalid or unsupported group option")
          }
        }

        None -> Error("Invalid or unsupported group option")
      }
    }

    // Plain capturing group (...)
    _ -> {
      let mod_x = get_current_mod_x(ctx)
      let new_ctx =
        ctx
        |> advance(1)
        |> push_mod_x(mod_x)
        |> increment_open_groups

      case ctx.capture_group {
        True ->
          // When captureGroup is enabled, ( is always capturing
          Ok(#(
            [
              RegularToken(GroupOpenToken(
                kind: Capturing,
                flags: None,
                name: None,
                number: None,
                negate: None,
                raw: "(",
              )),
            ],
            new_ctx,
          ))
        False ->
          // Otherwise, may be converted to capturing later
          Ok(#(
            [
              RegularToken(GroupOpenToken(
                kind: GroupKind,
                flags: None,
                name: None,
                number: None,
                negate: None,
                raw: "(",
              )),
            ],
            new_ctx,
          ))
      }
    }
  }
}

/// Tokenize lookbehind or named group starting with (?<
fn tokenize_lookbehind_or_named_group(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 3)
  // (?<
  case peek_char(ctx2) {
    Some("=") -> {
      // Positive lookbehind (?<=...)
      let mod_x = get_current_mod_x(ctx)
      let new_ctx =
        ctx
        |> advance(4)
        |> push_mod_x(mod_x)
        |> increment_open_groups
      Ok(#(
        [
          RegularToken(GroupOpenToken(
            kind: LookbehindKind,
            flags: None,
            name: None,
            number: None,
            negate: Some(False),
            raw: "(?<=",
          )),
        ],
        new_ctx,
      ))
    }
    Some("!") -> {
      // Negative lookbehind (?<!...)
      let mod_x = get_current_mod_x(ctx)
      let new_ctx =
        ctx
        |> advance(4)
        |> push_mod_x(mod_x)
        |> increment_open_groups
      Ok(#(
        [
          RegularToken(GroupOpenToken(
            kind: LookbehindKind,
            flags: None,
            name: None,
            number: None,
            negate: Some(True),
            raw: "(?<!",
          )),
        ],
        new_ctx,
      ))
    }
    _ -> {
      // Named capturing group (?<name>...)
      let #(name, ctx3) = read_until(ctx2, ">")
      case peek_char(ctx3) {
        Some(">") -> {
          let raw = "(?<" <> name <> ">"
          let mod_x = get_current_mod_x(ctx)
          let new_ctx =
            ctx3
            |> advance(1)
            |> push_mod_x(mod_x)
            |> increment_open_groups
          Ok(#(
            [
              RegularToken(GroupOpenToken(
                kind: Capturing,
                flags: None,
                name: Some(name),
                number: None,
                negate: None,
                raw: raw,
              )),
            ],
            new_ctx,
          ))
        }
        _ -> Error("Invalid group name")
      }
    }
  }
}

/// Tokenize named group with single quote (?'name'...)
fn tokenize_named_group_single_quote(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 3)
  // (?'
  let #(name, ctx3) = read_until(ctx2, "'")
  case peek_char(ctx3) {
    Some("'") -> {
      let raw = "(?'" <> name <> "'"
      let mod_x = get_current_mod_x(ctx)
      let new_ctx =
        ctx3
        |> advance(1)
        |> push_mod_x(mod_x)
        |> increment_open_groups
      Ok(#(
        [
          RegularToken(GroupOpenToken(
            kind: Capturing,
            flags: None,
            name: Some(name),
            number: None,
            negate: None,
            raw: raw,
          )),
        ],
        new_ctx,
      ))
    }
    _ -> Error("Invalid group name")
  }
}

/// Tokenize absence function (?~...)
fn tokenize_absence_function(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 3)
  // (?~
  case peek_char(ctx2) {
    Some("|") -> Error("Unsupported absence function kind \"(?~|\"")
    _ -> {
      let mod_x = get_current_mod_x(ctx)
      let new_ctx =
        ctx2
        |> push_mod_x(mod_x)
        |> increment_open_groups
      Ok(#(
        [
          RegularToken(GroupOpenToken(
            kind: AbsenceRepeater,
            flags: None,
            name: None,
            number: None,
            negate: None,
            raw: "(?~",
          )),
        ],
        new_ctx,
      ))
    }
  }
}

/// Tokenize flag modifier (?imx-xyz:) or (?imx-xyz)
fn tokenize_flag_modifier(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 2)
  // (?
  // Read on flags
  let #(on_flags, ctx3) = read_while(ctx2, is_flag_char)

  // Check for off flags
  let #(off_flags, ctx4) = case peek_char(ctx3) {
    Some("-") -> {
      let ctx_after_minus = advance(ctx3, 1)
      read_while(ctx_after_minus, fn(c) { is_flag_char(c) || c == "-" })
    }
    _ -> #("", ctx3)
  }

  // Determine if directive or group opener
  case peek_char(ctx4) {
    Some(")") -> {
      // Flag directive (?imx-xyz)
      let raw =
        "(?"
        <> on_flags
        <> case off_flags {
          "" -> ""
          _ -> "-" <> off_flags
        }
        <> ")"
      let new_ctx = advance(ctx4, 1)

      let is_x_on =
        { get_current_mod_x(ctx) || string.contains(on_flags, "x") }
        && !string.contains(off_flags, "x")
      let new_ctx2 = replace_current_mod_x(new_ctx, is_x_on)

      let flags = make_flag_modifiers(on_flags, off_flags)
      Ok(#(
        [
          RegularToken(DirectiveToken(
            kind: TokFlags,
            flags: Some(flags),
            raw: raw,
          )),
        ],
        new_ctx2,
      ))
    }
    Some(":") -> {
      // Flag group (?imx-xyz:...)
      let raw =
        "(?"
        <> on_flags
        <> case off_flags {
          "" -> ""
          _ -> "-" <> off_flags
        }
        <> ":"
      let new_ctx = advance(ctx4, 1)

      let is_x_on =
        { get_current_mod_x(ctx) || string.contains(on_flags, "x") }
        && !string.contains(off_flags, "x")
      let new_ctx2 =
        new_ctx
        |> push_mod_x(is_x_on)
        |> increment_open_groups

      let flags = make_flag_modifiers(on_flags, off_flags)
      Ok(#(
        [
          RegularToken(GroupOpenToken(
            kind: GroupKind,
            flags: case on_flags, off_flags {
              "", "" -> None
              _, _ -> Some(flags)
            },
            name: None,
            number: None,
            negate: None,
            raw: raw,
          )),
        ],
        new_ctx2,
      ))
    }
    _ -> Error("Unexpected flag modifier")
  }
}

/// Create flag modifiers from on and off flag strings
fn make_flag_modifiers(on: String, off: String) -> FlagGroupModifiers {
  let enable = case on {
    "" -> None
    _ ->
      Some(
        FlagGroupSwitches(
          ignore_case: case string.contains(on, "i") {
            True -> Some(True)
            False -> None
          },
          dot_all: case string.contains(on, "m") {
            True -> Some(True)
            False -> None
          },
          extended: case string.contains(on, "x") {
            True -> Some(True)
            False -> None
          },
        ),
      )
  }

  let disable = case off {
    "" -> None
    _ ->
      Some(
        FlagGroupSwitches(
          ignore_case: case string.contains(off, "i") {
            True -> Some(True)
            False -> None
          },
          dot_all: case string.contains(off, "m") {
            True -> Some(True)
            False -> None
          },
          extended: case string.contains(off, "x") {
            True -> Some(True)
            False -> None
          },
        ),
      )
  }

  FlagGroupModifiers(enable: enable, disable: disable)
}

/// Tokenize comment group (?#...)
fn tokenize_comment_group(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 3)
  // (?#
  // Read until )
  let #(_, ctx3) = read_until(ctx2, ")")
  case peek_char(ctx3) {
    Some(")") -> {
      let new_ctx = advance(ctx3, 1)
      Ok(#([], new_ctx))
    }
    _ -> Error("Unclosed comment group \"(?#\"")
  }
}

/// Tokenize named callout (*NAME...)
fn tokenize_named_callout(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 2)
  // (*
  // Read the name
  let #(name, ctx3) = read_while(ctx2, is_word_char)

  // Check for tag [tag]
  let #(tag, ctx4) = case peek_char(ctx3) {
    Some("[") -> {
      let ctx_after_bracket = advance(ctx3, 1)
      let #(tag_value, ctx_after_tag) = read_until(ctx_after_bracket, "]")
      case peek_char(ctx_after_tag) {
        Some("]") -> #(Some(tag_value), advance(ctx_after_tag, 1))
        _ -> #(None, ctx3)
      }
    }
    _ -> #(None, ctx3)
  }

  // Check for arguments {args}
  let #(args, ctx5) = case peek_char(ctx4) {
    Some("{") -> {
      let ctx_after_brace = advance(ctx4, 1)
      let #(args_value, ctx_after_args) = read_until(ctx_after_brace, "}")
      case peek_char(ctx_after_args) {
        Some("}") -> {
          let args_list =
            string.split(args_value, ",")
            |> list.filter(fn(s) { s != "" })
          #(Some(args_list), advance(ctx_after_args, 1))
        }
        _ -> #(None, ctx4)
      }
    }
    _ -> #(None, ctx4)
  }

  case peek_char(ctx5) {
    Some(")") -> {
      let kind = callout_name_to_kind(name)
      let tag_str = case tag {
        Some(t) -> "[" <> t <> "]"
        None -> ""
      }
      let args_str = case args {
        Some(a) -> "{" <> string.join(a, ",") <> "}"
        None -> ""
      }
      let raw = "(*" <> name <> tag_str <> args_str <> ")"
      let new_ctx = advance(ctx5, 1)
      Ok(#(
        [
          RegularToken(NamedCalloutToken(
            kind: kind,
            tag: tag,
            arguments: args,
            raw: raw,
          )),
        ],
        new_ctx,
      ))
    }
    _ -> Error("Incomplete or invalid named callout")
  }
}

/// Convert callout name to kind
fn callout_name_to_kind(name: String) -> NamedCalloutKind {
  case string.uppercase(name) {
    "COUNT" -> Count
    "CMP" -> Cmp
    "ERROR" -> ErrorCallout
    "FAIL" -> Fail
    "MAX" -> Max
    "MISMATCH" -> Mismatch
    "SKIP" -> Skip
    "TOTAL_COUNT" -> TotalCount
    _ -> Custom
  }
}

/// Tokenize group close )
fn tokenize_group_close(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let new_ctx =
    ctx
    |> advance(1)
    |> pop_mod_x
    |> decrement_open_groups

  case new_ctx.num_open_groups < 0 {
    True -> Error("Unmatched \")\"")
    False -> Ok(#([RegularToken(GroupCloseToken(raw: ")"))], new_ctx))
  }
}

// ============================================================================
// Quantifier Tokenization
// ============================================================================

/// Tokenize a quantifier ?, *, +
fn tokenize_quantifier(
  ctx: Context,
  char: String,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let #(min, max) = case char {
    "?" -> #(0, 1)
    "*" -> #(0, ast_types.quantifier_max_infinity)
    "+" -> #(1, ast_types.quantifier_max_infinity)
    _ -> #(0, 0)
  }

  let ctx2 = advance(ctx, 1)

  // Check for lazy or possessive suffix
  let #(kind, raw, ctx3) = case peek_char(ctx2) {
    Some("?") -> #(TokLazy, char <> "?", advance(ctx2, 1))
    Some("+") -> #(TokPossessive, char <> "+", advance(ctx2, 1))
    _ -> #(TokGreedy, char, ctx2)
  }

  Ok(#(
    [RegularToken(QuantifierToken(kind: kind, min: min, max: max, raw: raw))],
    ctx3,
  ))
}

/// Tokenize interval quantifier {n,m}
fn tokenize_interval_quantifier(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 1)
  // {
  // Read min
  let #(min_str, ctx3) = read_while(ctx2, is_digit)

  case peek_char(ctx3) {
    Some("}") -> {
      // Fixed quantifier {n}
      let raw = "{" <> min_str <> "}"
      let new_ctx = advance(ctx3, 1)
      use min <- result.try(
        int.parse(min_str)
        |> result.replace_error("Invalid quantifier \"" <> raw <> "\""),
      )

      // Check for lazy suffix
      let #(kind, raw2, ctx4) = case peek_char(new_ctx) {
        Some("?") -> #(TokLazy, raw <> "?", advance(new_ctx, 1))
        _ -> #(TokGreedy, raw, new_ctx)
      }

      Ok(#(
        [
          RegularToken(QuantifierToken(
            kind: kind,
            min: min,
            max: min,
            raw: raw2,
          )),
        ],
        ctx4,
      ))
    }

    Some(",") -> {
      let ctx4 = advance(ctx3, 1)
      let #(max_str, ctx5) = read_while(ctx4, is_digit)

      case peek_char(ctx5) {
        Some("}") -> {
          let raw = "{" <> min_str <> "," <> max_str <> "}"
          let new_ctx = advance(ctx5, 1)

          let min = case min_str {
            "" -> 0
            _ ->
              int.parse(min_str)
              |> result.unwrap(0)
          }

          let max = case max_str {
            "" -> ast_types.quantifier_max_infinity
            _ ->
              int.parse(max_str)
              |> result.unwrap(ast_types.quantifier_max_infinity)
          }

          // Check for reversed range (possessive)
          let #(kind, actual_min, actual_max, raw2, ctx6) = case min > max {
            True -> {
              // Reversed = possessive
              let #(lazy_suffix, ctx_after) = case peek_char(new_ctx) {
                Some("?") -> #("?", advance(new_ctx, 1))
                _ -> #("", new_ctx)
              }
              case lazy_suffix {
                "?" -> #(TokLazy, max, min, raw <> "?", ctx_after)
                _ -> #(TokPossessive, max, min, raw, ctx_after)
              }
            }
            False -> {
              let #(suffix, suffix_ctx) = case peek_char(new_ctx) {
                Some("?") -> #(TokLazy, advance(new_ctx, 1))
                _ -> #(TokGreedy, new_ctx)
              }
              let suffix_raw = case suffix {
                TokLazy -> raw <> "?"
                _ -> raw
              }
              #(suffix, min, max, suffix_raw, suffix_ctx)
            }
          }

          Ok(#(
            [
              RegularToken(QuantifierToken(
                kind: kind,
                min: actual_min,
                max: actual_max,
                raw: raw2,
              )),
            ],
            ctx6,
          ))
        }
        _ -> {
          // Invalid quantifier, treat { as literal
          let cp = string_to_codepoint("{")
          Ok(#([RegularToken(CharacterToken(value: cp, raw: "{"))], ctx2))
        }
      }
    }

    _ -> {
      // Not a valid quantifier, treat { as literal
      let cp = string_to_codepoint("{")
      Ok(#([RegularToken(CharacterToken(value: cp, raw: "{"))], ctx2))
    }
  }
}

// ============================================================================
// Comment and Whitespace (Extended Mode)
// ============================================================================

/// Tokenize a comment in extended mode
fn tokenize_comment(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let ctx2 = advance(ctx, 1)
  // Skip #
  // Read until newline
  let #(_, ctx3) = read_until(ctx2, "\n")
  // Don't consume the newline itself
  Ok(#([], ctx3))
}

/// Tokenize whitespace in extended mode
fn tokenize_whitespace(
  ctx: Context,
) -> Result(#(List(TokenOrIntermediate), Context), String) {
  let #(_, new_ctx) = read_while(ctx, is_whitespace)
  Ok(#([], new_ctx))
}

// ============================================================================
// Second Pass: Capture Group Processing
// ============================================================================

/// Process capture groups - assign numbers and handle unnamed captures
fn process_capture_groups(
  tokens: List(TokenOrIntermediate),
  capture_group_enabled: Bool,
) -> #(List(TokenOrIntermediate), Int) {
  // First pass: count named/opt-in captures and collect potential unnamed
  let #(num_named, potential_unnamed) =
    list.fold(tokens, #(0, []), fn(acc, tok) {
      let #(count, unnamed) = acc
      case tok {
        RegularToken(GroupOpenToken(kind: Capturing, name: Some(_), ..)) -> #(
          count + 1,
          unnamed,
        )
        RegularToken(GroupOpenToken(kind: Capturing, name: None, raw: "(", ..)) -> #(
          count + 1,
          [tok, ..unnamed],
        )
        RegularToken(GroupOpenToken(kind: GroupKind, raw: "(", ..)) -> #(count, [
          tok,
          ..unnamed
        ])
        _ -> acc
      }
    })

  // If no named captures and captureGroup not enabled, convert unnamed to capturing
  let should_convert_unnamed = num_named == 0 && !capture_group_enabled

  // Second pass: assign numbers
  let #(final_count, numbered_tokens) =
    list.map_fold(tokens, 1, fn(num, tok) {
      case tok {
        RegularToken(GroupOpenToken(
          kind: Capturing,
          flags: flags,
          name: name,
          number: _,
          negate: negate,
          raw: raw,
        )) -> {
          let new_tok =
            RegularToken(GroupOpenToken(
              kind: Capturing,
              flags: flags,
              name: name,
              number: Some(num),
              negate: negate,
              raw: raw,
            ))
          #(num + 1, new_tok)
        }
        RegularToken(GroupOpenToken(
          kind: GroupKind,
          flags: flags,
          name: name,
          number: _,
          negate: negate,
          raw: "(",
        )) -> {
          case should_convert_unnamed {
            True -> {
              let new_tok =
                RegularToken(GroupOpenToken(
                  kind: Capturing,
                  flags: flags,
                  name: name,
                  number: Some(num),
                  negate: negate,
                  raw: "(",
                ))
              #(num + 1, new_tok)
            }
            False -> #(num, tok)
          }
        }
        _ -> #(num, tok)
      }
    })

  let total_captures = case should_convert_unnamed {
    True -> list.length(potential_unnamed)
    False -> num_named
  }

  #(numbered_tokens, int.max(final_count - 1, total_captures))
}

/// Convert escaped number tokens to proper backrefs or characters
fn convert_escaped_numbers(
  tokens: List(TokenOrIntermediate),
  num_captures: Int,
) -> Result(List(Token), String) {
  list.try_map(tokens, fn(tok) {
    case tok {
      RegularToken(t) -> Ok([t])
      IntermediateToken(EscapedNumberToken(in_char_class: in_cc, raw: raw)) ->
        split_escaped_number(raw, in_cc, num_captures)
    }
  })
  |> result.map(list.flatten)
}

/// Split an escaped number into backref or character tokens
fn split_escaped_number(
  raw: String,
  in_char_class: Bool,
  num_captures: Int,
) -> Result(List(Token), String) {
  let value = string.drop_start(raw, 1)

  // Determine if this is a backref
  let is_backref =
    !in_char_class
    && {
      // Single digit 1-9 outside char class is always backref
      { value != "0" && string.length(value) == 1 }
      // Or if it's <= num_captures and doesn't start with 0
      || {
        case string.first(value) {
          Ok("0") -> False
          _ -> {
            case int.parse(value) {
              Ok(n) -> n <= num_captures
              Error(_) -> False
            }
          }
        }
      }
    }

  case is_backref {
    True -> Ok([BackreferenceToken(raw: raw)])
    False -> {
      // Parse as octal or literal digits
      let chars = string.to_graphemes(value)
      parse_octal_or_digits(chars, raw)
    }
  }
}

/// Parse escaped number as octal and/or literal digits
fn parse_octal_or_digits(
  chars: List(String),
  raw: String,
) -> Result(List(Token), String) {
  case chars {
    [] -> Ok([])
    [first, ..rest] -> {
      // Check if first char sequence is octal
      let #(octal_chars, remaining) = take_octal_prefix(chars)

      case octal_chars {
        [] -> {
          // First char is 8 or 9, literal digit
          let cp = string_to_codepoint(first)
          use rest_tokens <- result.try(parse_octal_or_digits(
            rest,
            string.drop_start(raw, 1),
          ))
          Ok([CharacterToken(value: cp, raw: "\\" <> first), ..rest_tokens])
        }
        _ -> {
          let octal_str = string.join(octal_chars, "")
          use octal_value <- result.try(
            int.base_parse(octal_str, 8)
            |> result.replace_error("Invalid octal \"" <> raw <> "\""),
          )

          // Check if octal value > 0o177 (unsupported)
          case octal_value > 127 {
            True ->
              Error(
                "Octal encoded byte above 177 unsupported \"" <> raw <> "\"",
              )
            False -> {
              let octal_raw = "\\" <> octal_str
              use rest_tokens <- result.try(parse_literal_digits(remaining))
              Ok([
                CharacterToken(value: octal_value, raw: octal_raw),
                ..rest_tokens
              ])
            }
          }
        }
      }
    }
  }
}

/// Take octal prefix (0-7 digits)
fn take_octal_prefix(chars: List(String)) -> #(List(String), List(String)) {
  take_while(chars, is_octal_digit)
}

/// Parse remaining literal digits
fn parse_literal_digits(chars: List(String)) -> Result(List(Token), String) {
  list.try_map(chars, fn(char) {
    let cp = string_to_codepoint(char)
    Ok(CharacterToken(value: cp, raw: char))
  })
}

// ============================================================================
// Flag Properties
// ============================================================================

/// Get flag properties from a flags string
fn get_flag_properties(flags: String) -> FlagProperties {
  let chars = string.to_graphemes(flags)
  parse_flag_chars(chars, token_types.default_flag_properties())
}

fn parse_flag_chars(
  chars: List(String),
  props: FlagProperties,
) -> FlagProperties {
  case chars {
    [] -> props
    ["i", ..rest] ->
      parse_flag_chars(rest, FlagProperties(..props, ignore_case: True))
    ["m", ..rest] ->
      parse_flag_chars(rest, FlagProperties(..props, dot_all: True))
    ["x", ..rest] ->
      parse_flag_chars(rest, FlagProperties(..props, extended: True))
    ["D", ..rest] ->
      parse_flag_chars(rest, FlagProperties(..props, digit_is_ascii: True))
    ["P", ..rest] ->
      parse_flag_chars(rest, FlagProperties(..props, posix_is_ascii: True))
    ["S", ..rest] ->
      parse_flag_chars(rest, FlagProperties(..props, space_is_ascii: True))
    ["W", ..rest] ->
      parse_flag_chars(rest, FlagProperties(..props, word_is_ascii: True))
    ["y", "{", "g", "}", ..rest] ->
      parse_flag_chars(
        rest,
        FlagProperties(
          ..props,
          text_segment_mode: Some(token_types.GraphemeMode),
        ),
      )
    ["y", "{", "w", "}", ..rest] ->
      parse_flag_chars(
        rest,
        FlagProperties(
          ..props,
          text_segment_mode: Some(token_types.WordSegmentMode),
        ),
      )
    [_, ..rest] -> parse_flag_chars(rest, props)
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Peek at current character
fn peek_char(ctx: Context) -> Option(String) {
  ctx.graphemes
  |> list.drop(ctx.pos)
  |> list.first
  |> option.from_result
}

/// Peek at character at offset from current position
fn peek_char_at(ctx: Context, offset: Int) -> Option(String) {
  ctx.graphemes
  |> list.drop(ctx.pos + offset)
  |> list.first
  |> option.from_result
}

/// Peek at n characters from current position
fn peek_chars(ctx: Context, n: Int) -> String {
  ctx.graphemes
  |> list.drop(ctx.pos)
  |> list.take(n)
  |> string.join("")
}

/// Advance position by n
fn advance(ctx: Context, n: Int) -> Context {
  Context(..ctx, pos: ctx.pos + n)
}

/// Increment open groups count
fn increment_open_groups(ctx: Context) -> Context {
  Context(..ctx, num_open_groups: ctx.num_open_groups + 1)
}

/// Decrement open groups count
fn decrement_open_groups(ctx: Context) -> Context {
  Context(..ctx, num_open_groups: ctx.num_open_groups - 1)
}

/// Read characters while predicate is true
fn read_while(ctx: Context, pred: fn(String) -> Bool) -> #(String, Context) {
  do_read_while(ctx, pred, [])
}

fn do_read_while(
  ctx: Context,
  pred: fn(String) -> Bool,
  acc: List(String),
) -> #(String, Context) {
  case peek_char(ctx) {
    Some(char) -> {
      case pred(char) {
        True -> do_read_while(advance(ctx, 1), pred, [char, ..acc])
        False -> #(string.join(list.reverse(acc), ""), ctx)
      }
    }
    None -> #(string.join(list.reverse(acc), ""), ctx)
  }
}

/// Read characters until a specific character
fn read_until(ctx: Context, stop: String) -> #(String, Context) {
  do_read_until(ctx, stop, [])
}

fn do_read_until(
  ctx: Context,
  stop: String,
  acc: List(String),
) -> #(String, Context) {
  case peek_char(ctx) {
    Some(char) -> {
      case char == stop {
        True -> #(string.join(list.reverse(acc), ""), ctx)
        False -> do_read_until(advance(ctx, 1), stop, [char, ..acc])
      }
    }
    None -> #(string.join(list.reverse(acc), ""), ctx)
  }
}

/// Read up to n hex digits
fn read_hex_digits(ctx: Context, max: Int) -> #(String, Context) {
  do_read_hex_digits(ctx, max, [])
}

fn do_read_hex_digits(
  ctx: Context,
  remaining: Int,
  acc: List(String),
) -> #(String, Context) {
  case remaining > 0 {
    False -> #(string.join(list.reverse(acc), ""), ctx)
    True ->
      case peek_char(ctx) {
        Some(char) -> {
          case is_hex_digit(char) {
            True ->
              do_read_hex_digits(advance(ctx, 1), remaining - 1, [char, ..acc])
            False -> #(string.join(list.reverse(acc), ""), ctx)
          }
        }
        _ -> #(string.join(list.reverse(acc), ""), ctx)
      }
  }
}

/// Take while predicate is true
fn take_while(
  items: List(String),
  pred: fn(String) -> Bool,
) -> #(List(String), List(String)) {
  do_take_while(items, pred, [])
}

fn do_take_while(
  items: List(String),
  pred: fn(String) -> Bool,
  acc: List(String),
) -> #(List(String), List(String)) {
  case items {
    [first, ..rest] -> {
      case pred(first) {
        True -> do_take_while(rest, pred, [first, ..acc])
        False -> #(list.reverse(acc), items)
      }
    }
    [] -> #(list.reverse(acc), items)
  }
}

/// Check if character is a hex digit
fn is_hex_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    "a" | "b" | "c" | "d" | "e" | "f" -> True
    "A" | "B" | "C" | "D" | "E" | "F" -> True
    _ -> False
  }
}

/// Check if character is a digit
fn is_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

/// Check if character is an octal digit
fn is_octal_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" -> True
    _ -> False
  }
}

/// Check if character is alphabetic
fn is_alpha(char: String) -> Bool {
  let cp = string_to_codepoint(char)
  { cp >= 65 && cp <= 90 } || { cp >= 97 && cp <= 122 }
}

/// Check if character is a word character
fn is_word_char(char: String) -> Bool {
  is_alpha(char) || is_digit(char) || char == "_"
}

/// Check if character is a flag character
fn is_flag_char(char: String) -> Bool {
  case char {
    "i" | "m" | "x" -> True
    _ -> False
  }
}

/// Check if character is whitespace
fn is_whitespace(char: String) -> Bool {
  case char {
    " " | "\t" | "\n" | "\r" -> True
    _ -> False
  }
}

/// Convert a single-character string to its code point
fn string_to_codepoint(s: String) -> Int {
  case string.to_utf_codepoints(s) {
    [cp] -> string.utf_codepoint_to_int(cp)
    _ -> 0
  }
}
