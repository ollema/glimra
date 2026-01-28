/// Main parser module for Oniguruma regex patterns.
/// Builds an AST from tokenized input.
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import glimra/oniguruma_parser/parser/ast_types.{
  type AbsenceFunctionNode, type AlternativeElement, type AlternativeNode,
  type AssertionKind, type AssertionNode, type BackreferenceNode,
  type BackreferenceRef, type CalloutArg, type CapturingGroupNode,
  type CharacterClassElement, type CharacterClassKind, type CharacterClassNode,
  type CharacterClassRangeNode, type CharacterNode, type CharacterSetNode,
  type DirectiveNode, type FlagGroupModifiers, type FlagsNode, type GroupNode,
  type LookaroundAssertionKind, type LookaroundAssertionNode,
  type NamedCalloutKind, type NamedCalloutNode, type OnigurumaAst,
  type QuantifiableNode, type QuantifierKind, type QuantifierNode,
  type RegexNode, type SubroutineNode, type SubroutineRef, type TextSegmentMode,
  AbsenceFunctionE, AbsenceFunctionNode, AlternativeNode, AssertionE,
  AssertionNode, BackreferenceE, BackreferenceNode, CapturingGroupE,
  CapturingGroupNode, CharacterCCE, CharacterClassCCE, CharacterClassE,
  CharacterClassNode, CharacterClassRangeCCE, CharacterClassRangeNode,
  CharacterE, CharacterNode, CharacterSetCCE, CharacterSetE, CharacterSetNode,
  DirectiveE, DirectiveNode, FlagsNode, Grapheme, GroupE, GroupNode,
  Intersection, LineEnd, LineStart, Lookahead, LookaroundAssertionE,
  LookaroundAssertionNode, Lookbehind, NamedCalloutE, NamedCalloutNode, NamedRef,
  NamedSubroutineRef, NumberArg, NumberedRef, NumberedSubroutineRef, QuantifierE,
  QuantifierNode, RegexNode, Repeater, SearchStart, StringArg, StringEnd,
  StringEndNewline, StringStart, SubroutineE, SubroutineNode,
  TextSegmentBoundary, Union, WordBoundary, WordMode,
}
import glimra/oniguruma_parser/parser/token_types.{
  type FlagProperties, type Token, type TokenCharacterSetKind,
  type TokenGroupOpenKind, type TokenQuantifierKind, AbsenceRepeater,
  AlternatorToken, AssertionToken, Atomic, BackreferenceToken, Capturing,
  CharacterClassCloseToken, CharacterClassHyphenToken,
  CharacterClassIntersectorToken, CharacterClassOpenToken, CharacterSetToken,
  CharacterToken, DirectiveToken, GroupCloseToken, GroupKind, GroupOpenToken,
  LookaheadKind, LookbehindKind, NamedCalloutToken, QuantifierToken,
  SubroutineToken, TokFlags, TokGreedy, TokKeep, TokLazy, TokNewline, TokPosix,
  TokPossessive, TokProperty, TokTextSegment,
}
import glimra/oniguruma_parser/parser/tokenizer.{TokenizeOptions, tokenize}
import glimra/oniguruma_parser/unicode.{type UnicodePropertyMap}

// ============================================================================
// Parse Options
// ============================================================================

/// Options for parsing
pub type ParseOptions {
  ParseOptions(
    flags: String,
    normalize_unknown_property_names: Bool,
    skip_backref_validation: Bool,
    skip_lookbehind_validation: Bool,
    skip_property_name_validation: Bool,
    unicode_property_map: Option(UnicodePropertyMap),
    capture_group: Bool,
    singleline: Bool,
  )
}

/// Default parse options
pub fn default_options() -> ParseOptions {
  ParseOptions(
    flags: "",
    normalize_unknown_property_names: False,
    skip_backref_validation: False,
    skip_lookbehind_validation: False,
    skip_property_name_validation: False,
    unicode_property_map: None,
    capture_group: False,
    singleline: False,
  )
}

// ============================================================================
// Parser Context
// ============================================================================

/// Internal context for parsing
type Context {
  Context(
    tokens: List(Token),
    pos: Int,
    capturing_groups: List(CapturingGroupNode),
    has_numbered_ref: Bool,
    named_groups_by_name: Dict(String, List(CapturingGroupNode)),
    subroutines: List(SubroutineNode),
    skip_backref_validation: Bool,
    skip_lookbehind_validation: Bool,
    skip_property_name_validation: Bool,
    normalize_unknown_property_names: Bool,
    unicode_property_map: Option(UnicodePropertyMap),
  )
}

// ============================================================================
// Loop State Types (for stack-safe iterative parsing)
// ============================================================================

/// Result of a single parsing step - either continue looping or finish
type LoopStep(state, result) {
  LoopContinue(state)
  LoopDone(result)
}

/// State for top-level/group body parsing loop
type AltLoopState {
  AltLoopState(ctx: Context, alts: List(AlternativeNode))
}

/// State for character class contents parsing loop
type CharClassLoopState {
  CharClassLoopState(
    ctx: Context,
    current_elements: List(CharacterClassElement),
    intersections: List(List(CharacterClassElement)),
  )
}

// ============================================================================
// Main Parse Function
// ============================================================================

/// Parse an Oniguruma pattern into an AST
pub fn parse(
  pattern: String,
  options: ParseOptions,
) -> Result(OnigurumaAst, String) {
  // Tokenize first
  let tokenize_opts =
    TokenizeOptions(
      flags: options.flags,
      capture_group: options.capture_group,
      singleline: options.singleline,
    )

  use tokenize_result <- result.try(tokenize(pattern, tokenize_opts))

  let ctx =
    Context(
      tokens: tokenize_result.tokens,
      pos: 0,
      capturing_groups: [],
      has_numbered_ref: False,
      named_groups_by_name: dict.new(),
      subroutines: [],
      skip_backref_validation: options.skip_backref_validation,
      skip_lookbehind_validation: options.skip_lookbehind_validation,
      skip_property_name_validation: options.skip_property_name_validation,
      normalize_unknown_property_names: options.normalize_unknown_property_names,
      unicode_property_map: options.unicode_property_map,
    )

  // Build flags node from flag properties
  let flags = token_flags_to_ast_flags(tokenize_result.flags)

  // Parse the body
  use #(body, final_ctx) <- result.try(parse_top_level(ctx))

  // Build the regex node
  let ast = RegexNode(body: body, flags: flags)

  // Validate backrefs and subroutines
  use _ <- result.try(validate_refs(final_ctx, options.capture_group))

  // Mark capturing groups that are referenced by subroutines
  let final_ast = mark_subroutined_groups(ast, final_ctx.subroutines)

  Ok(final_ast)
}

/// Convert token flag properties to AST flags node
fn token_flags_to_ast_flags(props: FlagProperties) -> FlagsNode {
  let text_mode: Option(TextSegmentMode) = case props.text_segment_mode {
    Some(token_types.GraphemeMode) -> Some(Grapheme)
    Some(token_types.WordSegmentMode) -> Some(WordMode)
    None -> None
  }

  FlagsNode(
    ignore_case: props.ignore_case,
    dot_all: props.dot_all,
    extended: props.extended,
    digit_is_ascii: props.digit_is_ascii,
    posix_is_ascii: props.posix_is_ascii,
    space_is_ascii: props.space_is_ascii,
    word_is_ascii: props.word_is_ascii,
    text_segment_mode: text_mode,
  )
}

// ============================================================================
// Top-level Parsing
// ============================================================================

/// Parse the top level of a pattern (alternations) - iterative version
fn parse_top_level(
  ctx: Context,
) -> Result(#(List(AlternativeNode), Context), String) {
  let first_alt = AlternativeNode(body: [])
  parse_top_level_loop(AltLoopState(ctx: ctx, alts: [first_alt]))
}

/// Single step of top-level parsing
fn parse_top_level_step(
  state: AltLoopState,
) -> LoopStep(AltLoopState, Result(#(List(AlternativeNode), Context), String)) {
  let AltLoopState(ctx: ctx, alts: alts) = state
  case current_token(ctx) {
    None -> LoopDone(Ok(#(list.reverse(alts), ctx)))
    Some(token) -> {
      case token {
        AlternatorToken(..) -> {
          // Start new alternative
          let new_alt = AlternativeNode(body: [])
          LoopContinue(AltLoopState(ctx: advance(ctx), alts: [new_alt, ..alts]))
        }
        QuantifierToken(kind: kind, min: min, max: max, ..) -> {
          // Handle quantifier by modifying the previous element
          case attach_quantifier_to_alt(alts, kind, min, max) {
            Error(e) -> LoopDone(Error(e))
            Ok(updated_alts) ->
              LoopContinue(AltLoopState(ctx: advance(ctx), alts: updated_alts))
          }
        }
        _ -> {
          // Parse element and add to current alternative
          case parse_element(ctx) {
            Error(e) -> LoopDone(Error(e))
            Ok(#(element, new_ctx)) -> {
              let updated_alts = add_to_current_alt(alts, element)
              LoopContinue(AltLoopState(ctx: new_ctx, alts: updated_alts))
            }
          }
        }
      }
    }
  }
}

/// Tail-recursive loop for top-level parsing
fn parse_top_level_loop(
  state: AltLoopState,
) -> Result(#(List(AlternativeNode), Context), String) {
  case parse_top_level_step(state) {
    LoopDone(result) -> result
    LoopContinue(new_state) -> parse_top_level_loop(new_state)
  }
}

/// Add an element to the current (first) alternative
fn add_to_current_alt(
  alts: List(AlternativeNode),
  element: AlternativeElement,
) -> List(AlternativeNode) {
  case alts {
    [AlternativeNode(body: body), ..rest] -> [
      AlternativeNode(body: list.append(body, [element])),
      ..rest
    ]
    [] -> [AlternativeNode(body: [element])]
  }
}

/// Attach a quantifier to the last element of the current alternative
fn attach_quantifier_to_alt(
  alts: List(AlternativeNode),
  kind: TokenQuantifierKind,
  min: Int,
  max: Int,
) -> Result(List(AlternativeNode), String) {
  case alts {
    [AlternativeNode(body: body), ..rest] -> {
      case list.reverse(body) {
        [] -> Error("Quantifier requires a repeatable token")
        [last, ..rest_body] -> {
          // Check if the last element is quantifiable and wrap it
          use quantifiable <- result.try(element_to_quantifiable(last))
          let quantifier_kind = token_quantifier_kind_to_ast(kind)
          let quantifier_node =
            QuantifierNode(
              kind: quantifier_kind,
              min: min,
              max: max,
              body: quantifiable,
            )
          // Replace last element with quantified version
          let new_body =
            list.append(list.reverse(rest_body), [QuantifierE(quantifier_node)])
          Ok([AlternativeNode(body: new_body), ..rest])
        }
      }
    }
    [] -> Error("Quantifier requires a repeatable token")
  }
}

/// Convert a token quantifier kind to AST quantifier kind
fn token_quantifier_kind_to_ast(
  kind: TokenQuantifierKind,
) -> ast_types.QuantifierKind {
  case kind {
    TokGreedy -> ast_types.Greedy
    TokLazy -> ast_types.Lazy
    TokPossessive -> ast_types.Possessive
  }
}

/// Convert an alternative element to a quantifiable node
fn element_to_quantifiable(
  element: AlternativeElement,
) -> Result(QuantifiableNode, String) {
  case element {
    AbsenceFunctionE(node) -> Ok(ast_types.AbsenceFunctionQ(node))
    BackreferenceE(node) -> Ok(ast_types.BackreferenceQ(node))
    CapturingGroupE(node) -> Ok(ast_types.CapturingGroupQ(node))
    CharacterE(node) -> Ok(ast_types.CharacterQ(node))
    CharacterClassE(node) -> Ok(ast_types.CharacterClassQ(node))
    CharacterSetE(node) -> Ok(ast_types.CharacterSetQ(node))
    GroupE(node) -> Ok(ast_types.GroupQ(node))
    SubroutineE(node) -> Ok(ast_types.SubroutineQ(node))
    QuantifierE(node) -> Ok(ast_types.QuantifierQ(node))
    // Not quantifiable
    AssertionE(_) -> Error("Quantifier requires a repeatable token")
    DirectiveE(_) -> Error("Quantifier requires a repeatable token")
    LookaroundAssertionE(_) -> Error("Quantifier requires a repeatable token")
    NamedCalloutE(_) -> Error("Quantifier requires a repeatable token")
  }
}

// ============================================================================
// Element Parsing
// ============================================================================

/// Parse a single element
fn parse_element(ctx: Context) -> Result(#(AlternativeElement, Context), String) {
  case current_token(ctx) {
    None -> Error("Unexpected end of pattern")

    Some(AssertionToken(kind: kind, ..)) -> {
      use assertion_kind <- result.try(parse_assertion_kind(kind))
      // For word_boundary and text_segment_boundary, always include negate
      // For \b and \y, negate is false; for \B and \Y, negate is true
      let negate = case kind {
        "\\b" | "\\y" -> Some(False)
        "\\B" | "\\Y" -> Some(True)
        _ -> None
      }
      let node = AssertionNode(kind: assertion_kind, negate: negate)
      Ok(#(AssertionE(node), advance(ctx)))
    }

    Some(BackreferenceToken(raw: raw)) -> parse_backreference(ctx, raw)

    Some(CharacterToken(value: value, ..)) -> {
      let node = CharacterNode(value: value)
      Ok(#(CharacterE(node), advance(ctx)))
    }

    Some(CharacterSetToken(kind: kind, value: value, negate: negate, ..)) -> {
      use node <- result.try(parse_character_set(ctx, kind, value, negate))
      Ok(#(CharacterSetE(node), advance(ctx)))
    }

    Some(CharacterClassOpenToken(negate: negate, ..)) ->
      parse_character_class(ctx, negate)

    Some(DirectiveToken(kind: kind, flags: flags, ..)) -> {
      let dir_kind = case kind {
        TokKeep -> ast_types.Keep
        TokFlags -> ast_types.Flags
      }
      let node = DirectiveNode(kind: dir_kind, flags: flags)
      Ok(#(DirectiveE(node), advance(ctx)))
    }

    Some(GroupOpenToken(
      kind: kind,
      name: name,
      number: number,
      negate: negate,
      flags: flags,
      ..,
    )) -> parse_group(ctx, kind, name, number, negate, flags)

    Some(NamedCalloutToken(kind: kind, tag: tag, arguments: args, ..)) -> {
      let parsed_args = case args {
        None -> None
        Some(arg_list) ->
          Some(
            list.map(arg_list, fn(arg) {
              case int.parse(arg) {
                Ok(n) -> NumberArg(n)
                Error(_) -> StringArg(arg)
              }
            }),
          )
      }
      let node = NamedCalloutNode(kind: kind, tag: tag, arguments: parsed_args)
      Ok(#(NamedCalloutE(node), advance(ctx)))
    }

    Some(QuantifierToken(kind: kind, min: min, max: max, ..)) ->
      parse_quantifier(ctx, kind, min, max)

    Some(SubroutineToken(raw: raw)) -> parse_subroutine(ctx, raw)

    Some(token) -> Error("Unexpected token: " <> token_types.token_raw(token))
  }
}

/// Parse an assertion kind from its string representation
fn parse_assertion_kind(kind: String) -> Result(AssertionKind, String) {
  case kind {
    "^" -> Ok(LineStart)
    "$" -> Ok(LineEnd)
    "\\A" -> Ok(StringStart)
    "\\b" -> Ok(WordBoundary)
    "\\B" -> Ok(WordBoundary)
    "\\G" -> Ok(SearchStart)
    "\\y" -> Ok(TextSegmentBoundary)
    "\\Y" -> Ok(TextSegmentBoundary)
    "\\z" -> Ok(StringEnd)
    "\\Z" -> Ok(StringEndNewline)
    _ -> Error("Unknown assertion kind: " <> kind)
  }
}

/// Parse a backreference
fn parse_backreference(
  ctx: Context,
  raw: String,
) -> Result(#(AlternativeElement, Context), String) {
  let has_k_wrapper =
    string.starts_with(raw, "\\k<") || string.starts_with(raw, "\\k'")

  let ref_str = case has_k_wrapper {
    True -> string.slice(raw, 3, string.length(raw) - 4)
    False -> string.drop_start(raw, 1)
  }

  // Try to parse as number
  case has_k_wrapper {
    True -> {
      case parse_numbered_ref(ref_str) {
        Ok(#(num, is_relative)) -> {
          let actual_num = case is_relative {
            True -> list.length(ctx.capturing_groups) + 1 - num
            False -> num
          }
          // Check if valid
          let #(orphan, new_ctx) = case
            actual_num > list.length(ctx.capturing_groups)
          {
            True ->
              case ctx.skip_backref_validation {
                True -> #(Some(True), ctx)
                False -> #(None, ctx)
              }
            False -> #(None, Context(..ctx, has_numbered_ref: True))
          }
          let node =
            BackreferenceNode(ref: NumberedRef(actual_num), orphan: orphan)
          Ok(#(BackreferenceE(node), advance(new_ctx)))
        }
        Error(_) -> {
          // Named backreference
          case dict.get(ctx.named_groups_by_name, ref_str) {
            Ok(_) -> {
              let node = BackreferenceNode(ref: NamedRef(ref_str), orphan: None)
              Ok(#(BackreferenceE(node), advance(ctx)))
            }
            Error(_) ->
              case ctx.skip_backref_validation {
                True -> {
                  let node =
                    BackreferenceNode(
                      ref: NamedRef(ref_str),
                      orphan: Some(True),
                    )
                  Ok(#(BackreferenceE(node), advance(ctx)))
                }
                False ->
                  Error("Group name not defined to the left \"" <> raw <> "\"")
              }
          }
        }
      }
    }
    False -> {
      // Simple numbered backref \1, \2, etc.
      use num <- result.try(
        int.parse(ref_str)
        |> result.replace_error("Invalid backreference \"" <> raw <> "\""),
      )
      let num_captures = list.length(ctx.capturing_groups)
      let #(orphan, new_ctx) = case num > num_captures {
        True ->
          case ctx.skip_backref_validation {
            True -> #(Some(True), ctx)
            False -> #(None, ctx)
          }
        False -> #(None, Context(..ctx, has_numbered_ref: True))
      }
      let node = BackreferenceNode(ref: NumberedRef(num), orphan: orphan)
      Ok(#(BackreferenceE(node), advance(new_ctx)))
    }
  }
}

/// Parse a numbered reference (possibly with sign)
fn parse_numbered_ref(s: String) -> Result(#(Int, Bool), Nil) {
  let trimmed = string.trim(s)
  case string.first(trimmed) {
    Ok("-") -> {
      let num_str = string.drop_start(trimmed, 1)
      case int.parse(num_str) {
        Ok(n) -> Ok(#(n, True))
        Error(_) -> Error(Nil)
      }
    }
    Ok("+") -> {
      let num_str = string.drop_start(trimmed, 1)
      case int.parse(num_str) {
        Ok(n) -> Ok(#(n, True))
        Error(_) -> Error(Nil)
      }
    }
    _ ->
      case int.parse(trimmed) {
        Ok(n) -> Ok(#(n, False))
        Error(_) -> Error(Nil)
      }
  }
}

/// Parse a character set
fn parse_character_set(
  ctx: Context,
  kind: TokenCharacterSetKind,
  value: Option(String),
  negate: Option(Bool),
) -> Result(CharacterSetNode, String) {
  let ast_kind = token_types.token_cs_kind_to_ast(kind)

  // Handle property and posix specially
  case kind {
    TokProperty -> {
      let val = option.unwrap(value, "")
      // Check if it's actually a POSIX class
      let normalized = unicode.slug(val)
      let is_posix = unicode.is_posix_class_name(normalized)
      let in_map = case ctx.unicode_property_map {
        Some(map) -> dict.has_key(map, normalized)
        None -> False
      }

      case is_posix && !in_map {
        True -> {
          // Treat as POSIX
          Ok(CharacterSetNode(
            kind: ast_types.Posix,
            value: Some(normalized),
            negate: negate,
            variable_length: None,
          ))
        }
        False -> {
          // Unicode property
          let final_value = case ctx.unicode_property_map {
            Some(map) ->
              case dict.get(map, normalized) {
                Ok(v) -> v
                Error(_) ->
                  case ctx.normalize_unknown_property_names {
                    True -> unicode.normalize_property_name(val)
                    False -> val
                  }
              }
            None -> val
          }
          Ok(CharacterSetNode(
            kind: ast_types.Property,
            value: Some(final_value),
            negate: negate,
            variable_length: None,
          ))
        }
      }
    }

    TokPosix -> {
      Ok(CharacterSetNode(
        kind: ast_types.Posix,
        value: value,
        negate: negate,
        variable_length: None,
      ))
    }

    TokTextSegment -> {
      Ok(CharacterSetNode(
        kind: ast_kind,
        value: None,
        negate: None,
        variable_length: Some(True),
      ))
    }

    TokNewline -> {
      let is_neg = option.unwrap(negate, False)
      Ok(
        CharacterSetNode(
          kind: ast_kind,
          value: None,
          negate: negate,
          variable_length: case is_neg {
            True -> None
            False -> Some(True)
          },
        ),
      )
    }

    _ -> {
      Ok(CharacterSetNode(
        kind: ast_kind,
        value: None,
        negate: negate,
        variable_length: None,
      ))
    }
  }
}

// ============================================================================
// Character Class Parsing
// ============================================================================

/// Parse a character class [...]
fn parse_character_class(
  ctx: Context,
  negate: Bool,
) -> Result(#(AlternativeElement, Context), String) {
  let ctx2 = advance(ctx)
  // Skip opening bracket
  use #(elements, intersections, ctx3) <- result.try(
    parse_char_class_contents(ctx2, [], []),
  )

  // Build the character class node
  let node = case intersections {
    [] ->
      CharacterClassNode(
        kind: Union,
        negate: negate,
        body: list.reverse(elements),
      )
    _ -> {
      // Has intersections - reverse to get correct order since we prepend during parsing
      let all_parts = list.reverse([elements, ..intersections])
      let body =
        list.map(all_parts, fn(part) {
          let reversed = list.reverse(part)
          case reversed {
            [single] -> single
            _ ->
              CharacterClassCCE(CharacterClassNode(
                kind: Union,
                negate: False,
                body: reversed,
              ))
          }
        })
      CharacterClassNode(kind: Intersection, negate: negate, body: body)
    }
  }

  Ok(#(CharacterClassE(node), ctx3))
}

/// Parse character class contents - iterative version
fn parse_char_class_contents(
  ctx: Context,
  current_elements: List(CharacterClassElement),
  intersections: List(List(CharacterClassElement)),
) -> Result(
  #(List(CharacterClassElement), List(List(CharacterClassElement)), Context),
  String,
) {
  parse_char_class_loop(CharClassLoopState(
    ctx: ctx,
    current_elements: current_elements,
    intersections: intersections,
  ))
}

/// Result type for character class parsing
type CharClassResult =
  Result(
    #(List(CharacterClassElement), List(List(CharacterClassElement)), Context),
    String,
  )

/// Single step of character class contents parsing
fn parse_char_class_step(
  state: CharClassLoopState,
) -> LoopStep(CharClassLoopState, CharClassResult) {
  let CharClassLoopState(
    ctx: ctx,
    current_elements: current_elements,
    intersections: intersections,
  ) = state

  case current_token(ctx) {
    None -> LoopDone(Error("Unclosed character class"))

    Some(CharacterClassCloseToken(..)) ->
      LoopDone(Ok(#(current_elements, intersections, advance(ctx))))

    Some(CharacterClassIntersectorToken(..)) -> {
      // Start new intersection part
      LoopContinue(
        CharClassLoopState(
          ctx: advance(ctx),
          current_elements: [],
          intersections: [current_elements, ..intersections],
        ),
      )
    }

    Some(CharacterClassHyphenToken(..)) -> {
      // Might be a range or literal hyphen
      case parse_char_class_hyphen(ctx, current_elements) {
        Error(e) -> LoopDone(Error(e))
        Ok(#(element, is_range, new_ctx)) -> {
          // If it's a range, remove the previous min character from elements
          let updated_elements = case is_range {
            True ->
              case current_elements {
                [_, ..rest] -> rest
                [] -> []
              }
            False -> current_elements
          }
          LoopContinue(CharClassLoopState(
            ctx: new_ctx,
            current_elements: [element, ..updated_elements],
            intersections: intersections,
          ))
        }
      }
    }

    Some(CharacterClassOpenToken(negate: negate, ..)) -> {
      // Nested character class - this recurses but depth is bounded by nesting level
      let ctx2 = advance(ctx)
      case parse_char_class_contents(ctx2, [], []) {
        Error(e) -> LoopDone(Error(e))
        Ok(#(elements, inner_ints, ctx3)) -> {
          let inner_node = case inner_ints {
            [] ->
              CharacterClassNode(
                kind: Union,
                negate: negate,
                body: list.reverse(elements),
              )
            _ -> {
              // Reverse to get correct order since we prepend during parsing
              let all_parts = list.reverse([elements, ..inner_ints])
              let body =
                list.map(all_parts, fn(part) {
                  let reversed = list.reverse(part)
                  case reversed {
                    [single] -> single
                    _ ->
                      CharacterClassCCE(CharacterClassNode(
                        kind: Union,
                        negate: False,
                        body: reversed,
                      ))
                  }
                })
              CharacterClassNode(kind: Intersection, negate: negate, body: body)
            }
          }
          LoopContinue(CharClassLoopState(
            ctx: ctx3,
            current_elements: [
              CharacterClassCCE(inner_node),
              ..current_elements
            ],
            intersections: intersections,
          ))
        }
      }
    }

    Some(CharacterToken(value: value, ..)) -> {
      let element = CharacterCCE(CharacterNode(value: value))
      LoopContinue(CharClassLoopState(
        ctx: advance(ctx),
        current_elements: [element, ..current_elements],
        intersections: intersections,
      ))
    }

    Some(CharacterSetToken(kind: kind, value: value, negate: negate, ..)) -> {
      case parse_character_set(ctx, kind, value, negate) {
        Error(e) -> LoopDone(Error(e))
        Ok(node) ->
          LoopContinue(CharClassLoopState(
            ctx: advance(ctx),
            current_elements: [CharacterSetCCE(node), ..current_elements],
            intersections: intersections,
          ))
      }
    }

    Some(token) ->
      LoopDone(Error(
        "Unexpected token in character class: " <> token_types.token_raw(token),
      ))
  }
}

/// Tail-recursive loop for character class contents parsing
fn parse_char_class_loop(state: CharClassLoopState) -> CharClassResult {
  case parse_char_class_step(state) {
    LoopDone(result) -> result
    LoopContinue(new_state) -> parse_char_class_loop(new_state)
  }
}

/// Parse a hyphen in character class (range or literal)
/// Returns (#element, is_range, context) - is_range indicates if prev element should be removed
fn parse_char_class_hyphen(
  ctx: Context,
  prev_elements: List(CharacterClassElement),
) -> Result(#(CharacterClassElement, Bool, Context), String) {
  let ctx2 = advance(ctx)
  // Skip the hyphen

  // Check if we can form a range
  case prev_elements {
    [CharacterCCE(CharacterNode(value: min_val)), ..] -> {
      // Check if next token can be end of range
      case current_token(ctx2) {
        Some(CharacterToken(value: max_val, ..)) -> {
          // Valid range
          case max_val >= min_val {
            True -> {
              let range_node =
                CharacterClassRangeNode(
                  min: CharacterNode(value: min_val),
                  max: CharacterNode(value: max_val),
                )
              // Return is_range=True to indicate prev element should be removed
              Ok(#(CharacterClassRangeCCE(range_node), True, advance(ctx2)))
            }
            False -> Error("Character class range out of order")
          }
        }
        Some(CharacterClassCloseToken(..))
        | Some(CharacterClassIntersectorToken(..))
        | None -> {
          // Hyphen at end is literal
          let hyphen_cp = 45
          // '-' codepoint
          Ok(#(CharacterCCE(CharacterNode(value: hyphen_cp)), False, ctx2))
        }
        _ -> {
          // Hyphen is literal
          let hyphen_cp = 45
          Ok(#(CharacterCCE(CharacterNode(value: hyphen_cp)), False, ctx2))
        }
      }
    }
    _ -> {
      // No previous character to form range, hyphen is literal
      let hyphen_cp = 45
      Ok(#(CharacterCCE(CharacterNode(value: hyphen_cp)), False, ctx2))
    }
  }
}

// ============================================================================
// Group Parsing
// ============================================================================

/// Parse a group
fn parse_group(
  ctx: Context,
  kind: TokenGroupOpenKind,
  name: Option(String),
  number: Option(Int),
  negate: Option(Bool),
  flags: Option(FlagGroupModifiers),
) -> Result(#(AlternativeElement, Context), String) {
  let ctx2 = advance(ctx)
  // Skip group open token

  // For capturing groups, register them first
  let ctx3 = case kind {
    Capturing -> {
      let cap_group =
        CapturingGroupNode(
          number: option.unwrap(number, 0),
          name: name,
          is_subroutined: None,
          body: [],
        )
      let new_ctx =
        Context(
          ..ctx2,
          capturing_groups: list.append(ctx2.capturing_groups, [cap_group]),
        )
      case name {
        Some(n) -> {
          let existing =
            dict.get(new_ctx.named_groups_by_name, n) |> result.unwrap([])
          Context(
            ..new_ctx,
            named_groups_by_name: dict.insert(
              new_ctx.named_groups_by_name,
              n,
              list.append(existing, [cap_group]),
            ),
          )
        }
        None -> new_ctx
      }
    }
    _ -> ctx2
  }

  // Parse group contents
  use #(body, ctx4) <- result.try(
    parse_group_body(ctx3, [AlternativeNode(body: [])]),
  )

  // Build the appropriate node
  case kind {
    AbsenceRepeater -> {
      let node = AbsenceFunctionNode(kind: Repeater, body: body)
      Ok(#(AbsenceFunctionE(node), ctx4))
    }
    Atomic -> {
      let node = GroupNode(atomic: Some(True), flags: None, body: body)
      Ok(#(GroupE(node), ctx4))
    }
    Capturing -> {
      let node =
        CapturingGroupNode(
          number: option.unwrap(number, 0),
          name: name,
          is_subroutined: None,
          body: body,
        )
      Ok(#(CapturingGroupE(node), ctx4))
    }
    GroupKind -> {
      let node = GroupNode(atomic: None, flags: flags, body: body)
      Ok(#(GroupE(node), ctx4))
    }
    LookaheadKind -> {
      let node =
        LookaroundAssertionNode(
          kind: Lookahead,
          negate: option.unwrap(negate, False),
          body: body,
        )
      Ok(#(LookaroundAssertionE(node), ctx4))
    }
    LookbehindKind -> {
      let node =
        LookaroundAssertionNode(
          kind: Lookbehind,
          negate: option.unwrap(negate, False),
          body: body,
        )
      Ok(#(LookaroundAssertionE(node), ctx4))
    }
  }
}

/// Parse group body (handles alternation) - iterative version
fn parse_group_body(
  ctx: Context,
  alts: List(AlternativeNode),
) -> Result(#(List(AlternativeNode), Context), String) {
  parse_group_body_loop(AltLoopState(ctx: ctx, alts: alts))
}

/// Single step of group body parsing
fn parse_group_body_step(
  state: AltLoopState,
) -> LoopStep(AltLoopState, Result(#(List(AlternativeNode), Context), String)) {
  let AltLoopState(ctx: ctx, alts: alts) = state
  case current_token(ctx) {
    None -> LoopDone(Error("Unclosed group"))

    Some(GroupCloseToken(..)) ->
      LoopDone(Ok(#(list.reverse(alts), advance(ctx))))

    Some(AlternatorToken(..)) -> {
      let new_alt = AlternativeNode(body: [])
      LoopContinue(AltLoopState(ctx: advance(ctx), alts: [new_alt, ..alts]))
    }

    Some(QuantifierToken(kind: kind, min: min, max: max, ..)) -> {
      // Handle quantifier by modifying the previous element
      case attach_quantifier_to_alt(alts, kind, min, max) {
        Error(e) -> LoopDone(Error(e))
        Ok(updated_alts) ->
          LoopContinue(AltLoopState(ctx: advance(ctx), alts: updated_alts))
      }
    }

    Some(_) -> {
      case parse_element(ctx) {
        Error(e) -> LoopDone(Error(e))
        Ok(#(element, new_ctx)) -> {
          let updated_alts = add_to_current_alt(alts, element)
          LoopContinue(AltLoopState(ctx: new_ctx, alts: updated_alts))
        }
      }
    }
  }
}

/// Tail-recursive loop for group body parsing
fn parse_group_body_loop(
  state: AltLoopState,
) -> Result(#(List(AlternativeNode), Context), String) {
  case parse_group_body_step(state) {
    LoopDone(result) -> result
    LoopContinue(new_state) -> parse_group_body_loop(new_state)
  }
}

// ============================================================================
// Quantifier Parsing
// ============================================================================

/// Parse a quantifier and attach it to the previous element
fn parse_quantifier(
  _ctx: Context,
  _kind: TokenQuantifierKind,
  _min: Int,
  _max: Int,
) -> Result(#(AlternativeElement, Context), String) {
  // The quantifier must attach to a previous element
  // This is handled differently - we need to modify the previous element
  // For now, return an error as this should be handled at the alt level
  Error("Quantifier requires a repeatable token")
}

// ============================================================================
// Subroutine Parsing
// ============================================================================

/// Parse a subroutine
fn parse_subroutine(
  ctx: Context,
  raw: String,
) -> Result(#(AlternativeElement, Context), String) {
  // Extract the reference from \g<...> or \g'...'
  let ref_str = string.slice(raw, 3, string.length(raw) - 4)

  // Try to parse as number
  let #(ref, new_ctx) = case parse_subroutine_ref(ref_str, ctx) {
    Ok(#(r, c)) -> #(r, c)
    Error(_) -> #(NamedSubroutineRef(ref_str), ctx)
  }

  let node = SubroutineNode(ref: ref, is_recursive: None)
  let ctx2 =
    Context(..new_ctx, subroutines: list.append(new_ctx.subroutines, [node]))

  Ok(#(SubroutineE(node), advance(ctx2)))
}

/// Parse a subroutine reference
fn parse_subroutine_ref(
  ref_str: String,
  ctx: Context,
) -> Result(#(SubroutineRef, Context), String) {
  case ref_str {
    "0" -> Ok(#(NumberedSubroutineRef(0), ctx))
    _ -> {
      case parse_numbered_ref(ref_str) {
        Ok(#(num, is_relative)) -> {
          let num_captures = list.length(ctx.capturing_groups)
          let actual_num = case is_relative, string.first(ref_str) {
            True, Ok("+") -> num_captures + num
            True, Ok("-") -> num_captures + 1 - num
            _, _ -> num
          }
          case actual_num < 1 {
            True -> Error("Invalid subroutine number")
            False -> {
              let new_ctx = Context(..ctx, has_numbered_ref: True)
              Ok(#(NumberedSubroutineRef(actual_num), new_ctx))
            }
          }
        }
        Error(_) -> Error("Not a number")
      }
    }
  }
}

// ============================================================================
// Validation
// ============================================================================

/// Validate backreferences and subroutines
fn validate_refs(
  ctx: Context,
  capture_group_enabled: Bool,
) -> Result(Nil, String) {
  // Check numbered ref with named captures
  case
    ctx.has_numbered_ref
    && dict.size(ctx.named_groups_by_name) > 0
    && !capture_group_enabled
  {
    True ->
      Error("Numbered backref/subroutine not allowed when using named capture")
    False -> {
      // Validate subroutines
      list.try_each(ctx.subroutines, fn(sub) {
        case sub.ref {
          NumberedSubroutineRef(n) -> {
            case n > list.length(ctx.capturing_groups) {
              True -> Error("Subroutine uses a group number that's not defined")
              False -> Ok(Nil)
            }
          }
          NamedSubroutineRef(name) -> {
            case dict.get(ctx.named_groups_by_name, name) {
              Error(_) ->
                Error(
                  "Subroutine uses a group name that's not defined \"\\g<"
                  <> name
                  <> ">\"",
                )
              Ok(groups) ->
                case list.length(groups) > 1 {
                  True ->
                    Error(
                      "Subroutine uses a duplicate group name \"\\g<"
                      <> name
                      <> ">\"",
                    )
                  False -> Ok(Nil)
                }
            }
          }
        }
      })
    }
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Get the current token
fn current_token(ctx: Context) -> Option(Token) {
  ctx.tokens |> list.drop(ctx.pos) |> list.first |> option.from_result
}

/// Advance to the next token
fn advance(ctx: Context) -> Context {
  Context(..ctx, pos: ctx.pos + 1)
}

// ============================================================================
// Node Creation Functions (exported for use by transformer)
// ============================================================================

/// Create an alternative node
pub fn create_alternative(body: List(AlternativeElement)) -> AlternativeNode {
  AlternativeNode(body: body)
}

/// Create an assertion node
pub fn create_assertion(
  kind: AssertionKind,
  negate: Option(Bool),
) -> AssertionNode {
  AssertionNode(kind: kind, negate: negate)
}

/// Create a backreference node
pub fn create_backreference(
  ref: BackreferenceRef,
  orphan: Option(Bool),
) -> BackreferenceNode {
  BackreferenceNode(ref: ref, orphan: orphan)
}

/// Create a capturing group node
pub fn create_capturing_group(
  number: Int,
  name: Option(String),
  body: List(AlternativeNode),
) -> CapturingGroupNode {
  CapturingGroupNode(
    number: number,
    name: name,
    is_subroutined: None,
    body: body,
  )
}

/// Create a character node
pub fn create_character(value: Int) -> CharacterNode {
  CharacterNode(value: value)
}

/// Create a character class node
pub fn create_character_class(
  kind: CharacterClassKind,
  negate: Bool,
  body: List(CharacterClassElement),
) -> CharacterClassNode {
  CharacterClassNode(kind: kind, negate: negate, body: body)
}

/// Create a character class range node
pub fn create_character_class_range(
  min: CharacterNode,
  max: CharacterNode,
) -> Result(CharacterClassRangeNode, String) {
  case max.value < min.value {
    True -> Error("Character class range out of order")
    False -> Ok(CharacterClassRangeNode(min: min, max: max))
  }
}

/// Create a character set node
pub fn create_character_set(
  kind: ast_types.CharacterSetKind,
  value: Option(String),
  negate: Option(Bool),
) -> CharacterSetNode {
  let variable_length = case kind {
    ast_types.TextSegment -> Some(True)
    ast_types.Newline ->
      case negate {
        Some(True) -> None
        _ -> Some(True)
      }
    _ -> None
  }
  CharacterSetNode(
    kind: kind,
    value: value,
    negate: negate,
    variable_length: variable_length,
  )
}

/// Create a directive node
pub fn create_directive(
  kind: ast_types.DirectiveKind,
  flags: Option(FlagGroupModifiers),
) -> DirectiveNode {
  DirectiveNode(kind: kind, flags: flags)
}

/// Create a flags node
pub fn create_flags(
  ignore_case: Bool,
  dot_all: Bool,
  extended: Bool,
) -> FlagsNode {
  FlagsNode(
    ignore_case: ignore_case,
    dot_all: dot_all,
    extended: extended,
    digit_is_ascii: False,
    posix_is_ascii: False,
    space_is_ascii: False,
    word_is_ascii: False,
    text_segment_mode: None,
  )
}

/// Create a group node
pub fn create_group(
  atomic: Option(Bool),
  flags: Option(FlagGroupModifiers),
  body: List(AlternativeNode),
) -> GroupNode {
  GroupNode(atomic: atomic, flags: flags, body: body)
}

/// Create a lookaround assertion node
pub fn create_lookaround_assertion(
  kind: LookaroundAssertionKind,
  negate: Bool,
  body: List(AlternativeNode),
) -> LookaroundAssertionNode {
  LookaroundAssertionNode(kind: kind, negate: negate, body: body)
}

/// Create a named callout node
pub fn create_named_callout(
  kind: NamedCalloutKind,
  tag: Option(String),
  arguments: Option(List(CalloutArg)),
) -> NamedCalloutNode {
  NamedCalloutNode(kind: kind, tag: tag, arguments: arguments)
}

/// Create a quantifier node
pub fn create_quantifier(
  kind: QuantifierKind,
  min: Int,
  max: Int,
  body: QuantifiableNode,
) -> Result(QuantifierNode, String) {
  case min > max {
    True -> Error("Invalid reversed quantifier range")
    False -> Ok(QuantifierNode(kind: kind, min: min, max: max, body: body))
  }
}

/// Create a regex node
pub fn create_regex(flags: FlagsNode, body: List(AlternativeNode)) -> RegexNode {
  RegexNode(body: body, flags: flags)
}

/// Create a subroutine node
pub fn create_subroutine(ref: SubroutineRef) -> SubroutineNode {
  SubroutineNode(ref: ref, is_recursive: None)
}

/// Create an absence function node
pub fn create_absence_function(
  kind: ast_types.AbsenceFunctionKind,
  body: List(AlternativeNode),
) -> AbsenceFunctionNode {
  AbsenceFunctionNode(kind: kind, body: body)
}

// ============================================================================
// Post-processing: Mark Subroutined Groups
// ============================================================================

/// Mark capturing groups that are referenced by subroutines
fn mark_subroutined_groups(
  ast: OnigurumaAst,
  subroutines: List(SubroutineNode),
) -> OnigurumaAst {
  // Collect all referenced group numbers and names
  let #(numbered_refs, named_refs) = collect_subroutine_refs(subroutines)

  // Traverse and update the AST
  let updated_body =
    list.map(ast.body, fn(alt) {
      mark_alternative(alt, numbered_refs, named_refs)
    })

  RegexNode(..ast, body: updated_body)
}

/// Collect all numbered and named group references from subroutines
fn collect_subroutine_refs(
  subroutines: List(SubroutineNode),
) -> #(List(Int), List(String)) {
  list.fold(subroutines, #([], []), fn(acc, sub) {
    let #(numbers, names) = acc
    case sub.ref {
      NumberedSubroutineRef(n) -> #([n, ..numbers], names)
      NamedSubroutineRef(name) -> #(numbers, [name, ..names])
    }
  })
}

/// Mark subroutined groups in an alternative
fn mark_alternative(
  alt: AlternativeNode,
  numbered_refs: List(Int),
  named_refs: List(String),
) -> AlternativeNode {
  AlternativeNode(
    body: list.map(alt.body, fn(elem) {
      mark_element(elem, numbered_refs, named_refs)
    }),
  )
}

/// Mark subroutined groups in an element
fn mark_element(
  elem: AlternativeElement,
  numbered_refs: List(Int),
  named_refs: List(String),
) -> AlternativeElement {
  case elem {
    CapturingGroupE(node) -> {
      let is_by_number = list.contains(numbered_refs, node.number)
      let is_by_name = case node.name {
        Some(name) -> list.contains(named_refs, name)
        None -> False
      }
      case is_by_number || is_by_name {
        True ->
          CapturingGroupE(
            CapturingGroupNode(
              ..node,
              is_subroutined: Some(True),
              body: list.map(node.body, fn(a) {
                mark_alternative(a, numbered_refs, named_refs)
              }),
            ),
          )
        False ->
          CapturingGroupE(
            CapturingGroupNode(
              ..node,
              body: list.map(node.body, fn(a) {
                mark_alternative(a, numbered_refs, named_refs)
              }),
            ),
          )
      }
    }
    GroupE(node) ->
      GroupE(
        GroupNode(
          ..node,
          body: list.map(node.body, fn(a) {
            mark_alternative(a, numbered_refs, named_refs)
          }),
        ),
      )
    LookaroundAssertionE(node) ->
      LookaroundAssertionE(
        LookaroundAssertionNode(
          ..node,
          body: list.map(node.body, fn(a) {
            mark_alternative(a, numbered_refs, named_refs)
          }),
        ),
      )
    QuantifierE(node) ->
      QuantifierE(
        QuantifierNode(
          ..node,
          body: mark_quantifiable(node.body, numbered_refs, named_refs),
        ),
      )
    AbsenceFunctionE(node) ->
      AbsenceFunctionE(
        AbsenceFunctionNode(
          ..node,
          body: list.map(node.body, fn(a) {
            mark_alternative(a, numbered_refs, named_refs)
          }),
        ),
      )
    // Other elements don't contain nested groups
    _ -> elem
  }
}

/// Mark subroutined groups in a quantifiable node
fn mark_quantifiable(
  node: QuantifiableNode,
  numbered_refs: List(Int),
  named_refs: List(String),
) -> QuantifiableNode {
  case node {
    ast_types.CapturingGroupQ(cg) -> {
      let is_by_number = list.contains(numbered_refs, cg.number)
      let is_by_name = case cg.name {
        Some(name) -> list.contains(named_refs, name)
        None -> False
      }
      case is_by_number || is_by_name {
        True ->
          ast_types.CapturingGroupQ(
            CapturingGroupNode(
              ..cg,
              is_subroutined: Some(True),
              body: list.map(cg.body, fn(a) {
                mark_alternative(a, numbered_refs, named_refs)
              }),
            ),
          )
        False ->
          ast_types.CapturingGroupQ(
            CapturingGroupNode(
              ..cg,
              body: list.map(cg.body, fn(a) {
                mark_alternative(a, numbered_refs, named_refs)
              }),
            ),
          )
      }
    }
    ast_types.GroupQ(g) ->
      ast_types.GroupQ(
        GroupNode(
          ..g,
          body: list.map(g.body, fn(a) {
            mark_alternative(a, numbered_refs, named_refs)
          }),
        ),
      )
    ast_types.AbsenceFunctionQ(af) ->
      ast_types.AbsenceFunctionQ(
        AbsenceFunctionNode(
          ..af,
          body: list.map(af.body, fn(a) {
            mark_alternative(a, numbered_refs, named_refs)
          }),
        ),
      )
    // Other quantifiables don't contain nested groups
    _ -> node
  }
}
