//// Scope Selector Parser and Matcher
////
//// Implements TextMate scope selector parsing according to the grammar:
//// - Identifiers: scope names like "meta.style.astro"
//// - Negation: "-" prefix negates the following operand
//// - Grouping: parentheses for grouping
//// - OR: "|" or "," between expressions
//// - AND: space-separated operands (implicit conjunction)
//// - Priority: "L:" prefix = low priority, "R:" prefix = high priority

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// A compiled matcher that can test scope stacks
pub type ScopeMatcher {
  ScopeMatcher(matcher: fn(List(String)) -> Bool, priority: Int)
}

/// Token types for the selector parser
type Token {
  TokenIdentifier(String)
  TokenNegation
  TokenOpenParen
  TokenCloseParen
  TokenOr
  TokenPriorityL
  TokenPriorityR
  TokenEof
}

/// Tokenizer state
type Tokenizer {
  Tokenizer(tokens: List(Token), pos: Int)
}

/// Create matchers from a scope selector string
pub fn create_matchers(selector: String) -> List(ScopeMatcher) {
  let tokenizer = tokenize(selector)
  parse_selector_list(tokenizer, [])
}

/// Tokenize the selector string
fn tokenize(input: String) -> Tokenizer {
  let tokens = tokenize_impl(input, [])
  Tokenizer(tokens: list.reverse(tokens), pos: 0)
}

fn tokenize_impl(input: String, acc: List(Token)) -> List(Token) {
  let input = string.trim_start(input)
  case input {
    "" -> acc
    _ -> {
      case string.first(input) {
        Ok("-") ->
          tokenize_impl(string.drop_start(input, 1), [TokenNegation, ..acc])
        Ok("(") ->
          tokenize_impl(string.drop_start(input, 1), [TokenOpenParen, ..acc])
        Ok(")") ->
          tokenize_impl(string.drop_start(input, 1), [TokenCloseParen, ..acc])
        Ok("|") -> tokenize_impl(string.drop_start(input, 1), [TokenOr, ..acc])
        Ok(",") -> tokenize_impl(string.drop_start(input, 1), [TokenOr, ..acc])
        Ok("L") -> {
          case string.starts_with(input, "L:") {
            True ->
              tokenize_impl(string.drop_start(input, 2), [TokenPriorityL, ..acc])
            False -> tokenize_identifier(input, acc)
          }
        }
        Ok("R") -> {
          case string.starts_with(input, "R:") {
            True ->
              tokenize_impl(string.drop_start(input, 2), [TokenPriorityR, ..acc])
            False -> tokenize_identifier(input, acc)
          }
        }
        Ok(_) -> tokenize_identifier(input, acc)
        Error(Nil) -> acc
      }
    }
  }
}

fn tokenize_identifier(input: String, acc: List(Token)) -> List(Token) {
  let #(ident, rest) = take_identifier(input)
  case ident {
    "" -> acc
    _ -> tokenize_impl(rest, [TokenIdentifier(ident), ..acc])
  }
}

fn take_identifier(input: String) -> #(String, String) {
  take_identifier_impl(input, "")
}

fn take_identifier_impl(input: String, acc: String) -> #(String, String) {
  case string.first(input) {
    Error(Nil) -> #(acc, "")
    Ok(c) -> {
      case is_identifier_char(c) {
        True -> take_identifier_impl(string.drop_start(input, 1), acc <> c)
        False -> #(acc, input)
      }
    }
  }
}

fn is_identifier_char(c: String) -> Bool {
  case c {
    "." | ":" | "_" | "-" -> True
    _ -> {
      // Check if alphanumeric
      let code = string.to_utf_codepoints(c)
      case code {
        [cp] -> {
          let n = string.utf_codepoint_to_int(cp)
          // a-z, A-Z, 0-9
          { n >= 97 && n <= 122 }
          || { n >= 65 && n <= 90 }
          || { n >= 48 && n <= 57 }
        }
        _ -> False
      }
    }
  }
}

/// Get current token
fn current(tokenizer: Tokenizer) -> Token {
  case list.drop(tokenizer.tokens, tokenizer.pos) {
    [] -> TokenEof
    [tok, ..] -> tok
  }
}

/// Advance to next token
fn advance(tokenizer: Tokenizer) -> Tokenizer {
  Tokenizer(..tokenizer, pos: tokenizer.pos + 1)
}

/// Parse a list of selectors separated by commas
fn parse_selector_list(
  tokenizer: Tokenizer,
  acc: List(ScopeMatcher),
) -> List(ScopeMatcher) {
  let #(tokenizer, priority) = parse_priority(tokenizer)
  let #(tokenizer, matcher) = parse_conjunction(tokenizer)
  let result = ScopeMatcher(matcher: matcher, priority: priority)

  case current(tokenizer) {
    TokenOr -> {
      let tokenizer = advance(tokenizer)
      parse_selector_list(tokenizer, [result, ..acc])
    }
    _ -> list.reverse([result, ..acc])
  }
}

/// Parse priority prefix (L: or R:)
/// Note: In vscode-textmate, L: means priority -1 and these injections
/// WIN at equal positions against normal rules. R: means priority 1.
fn parse_priority(tokenizer: Tokenizer) -> #(Tokenizer, Int) {
  case current(tokenizer) {
    TokenPriorityL -> #(advance(tokenizer), -1)
    // L = priority -1 (wins at equal position)
    TokenPriorityR -> #(advance(tokenizer), 1)
    // R = priority 1
    _ -> #(tokenizer, 0)
  }
}

/// Parse a conjunction (AND of operands)
fn parse_conjunction(
  tokenizer: Tokenizer,
) -> #(Tokenizer, fn(List(String)) -> Bool) {
  parse_conjunction_impl(tokenizer, [])
}

fn parse_conjunction_impl(
  tokenizer: Tokenizer,
  matchers: List(fn(List(String)) -> Bool),
) -> #(Tokenizer, fn(List(String)) -> Bool) {
  let #(tokenizer, maybe_matcher) = parse_operand(tokenizer)
  case maybe_matcher {
    None -> {
      // No more operands - combine with AND
      let combined = fn(scopes: List(String)) -> Bool {
        list.all(matchers, fn(m) { m(scopes) })
      }
      #(tokenizer, combined)
    }
    Some(matcher) -> {
      parse_conjunction_impl(tokenizer, [matcher, ..matchers])
    }
  }
}

/// Parse a single operand (identifier, negation, or grouped expression)
fn parse_operand(
  tokenizer: Tokenizer,
) -> #(Tokenizer, Option(fn(List(String)) -> Bool)) {
  case current(tokenizer) {
    TokenNegation -> {
      // Negation: negate the next operand
      let tokenizer = advance(tokenizer)
      let #(tokenizer, maybe_inner) = parse_operand(tokenizer)
      case maybe_inner {
        None -> #(tokenizer, None)
        Some(inner) -> {
          let negated = fn(scopes: List(String)) -> Bool { !inner(scopes) }
          #(tokenizer, Some(negated))
        }
      }
    }

    TokenOpenParen -> {
      // Grouped expression
      let tokenizer = advance(tokenizer)
      let #(tokenizer, inner) = parse_inner_expression(tokenizer)
      let tokenizer = case current(tokenizer) {
        TokenCloseParen -> advance(tokenizer)
        _ -> tokenizer
      }
      #(tokenizer, Some(inner))
    }

    TokenIdentifier(_) -> {
      // Identifier - collect consecutive identifiers
      let #(tokenizer, idents) = collect_identifiers(tokenizer, [])
      let matcher = fn(scopes: List(String)) -> Bool {
        matches_identifiers(idents, scopes)
      }
      #(tokenizer, Some(matcher))
    }

    _ -> #(tokenizer, None)
  }
}

/// Collect consecutive identifiers
fn collect_identifiers(
  tokenizer: Tokenizer,
  acc: List(String),
) -> #(Tokenizer, List(String)) {
  case current(tokenizer) {
    TokenIdentifier(ident) -> {
      collect_identifiers(advance(tokenizer), [ident, ..acc])
    }
    _ -> #(tokenizer, list.reverse(acc))
  }
}

/// Parse inner expression (handles OR)
fn parse_inner_expression(
  tokenizer: Tokenizer,
) -> #(Tokenizer, fn(List(String)) -> Bool) {
  parse_inner_expression_impl(tokenizer, [])
}

fn parse_inner_expression_impl(
  tokenizer: Tokenizer,
  matchers: List(fn(List(String)) -> Bool),
) -> #(Tokenizer, fn(List(String)) -> Bool) {
  let #(tokenizer, matcher) = parse_conjunction(tokenizer)
  let matchers = [matcher, ..matchers]

  case current(tokenizer) {
    TokenOr -> {
      let tokenizer = advance(tokenizer)
      parse_inner_expression_impl(tokenizer, matchers)
    }
    _ -> {
      // Combine with OR
      let combined = fn(scopes: List(String)) -> Bool {
        list.any(matchers, fn(m) { m(scopes) })
      }
      #(tokenizer, combined)
    }
  }
}

/// Check if scope identifiers match the scope stack
/// All identifiers must be found in the scope stack (in order, but not necessarily adjacent)
fn matches_identifiers(identifiers: List(String), scopes: List(String)) -> Bool {
  case identifiers {
    [] -> True
    [ident, ..rest_idents] -> {
      find_and_continue(ident, rest_idents, scopes)
    }
  }
}

fn find_and_continue(
  ident: String,
  rest_idents: List(String),
  scopes: List(String),
) -> Bool {
  case scopes {
    [] -> False
    [scope, ..rest_scopes] -> {
      case scope_matches(scope, ident) {
        True -> matches_identifiers(rest_idents, rest_scopes)
        False -> find_and_continue(ident, rest_idents, rest_scopes)
      }
    }
  }
}

/// Check if a scope name matches an identifier pattern
/// The pattern can be a prefix: "keyword" matches "keyword.operator"
fn scope_matches(scope: String, pattern: String) -> Bool {
  case scope == pattern {
    True -> True
    False -> {
      // Check if scope starts with pattern followed by "."
      string.starts_with(scope, pattern <> ".")
    }
  }
}
