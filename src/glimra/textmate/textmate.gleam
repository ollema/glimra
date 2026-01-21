//// TextMate tokenization API
////
//// This module provides a clean interface for tokenizing code using the
//// TextMate implementation.
////
//// Usage:
//// ```gleam
//// import textmate/textmate
//// import textmate/raw_grammar
//// import ffi/onig_scanner
////
//// // Create scanner factory (requires onig initialization)
//// let scanner_factory = onig_scanner.create_scanner
////
//// // Compile grammar from raw grammar data
//// let grammar = textmate.compile_grammar(raw, scanner_factory)
////
//// // Tokenize lines
//// let result = textmate.tokenize_line(grammar, "let x = 1", textmate.initial())
//// ```

import gleam/dict
import gleam/list
import glimra/ffi/onig_scanner.{type OnigScanner}
import glimra/textmate/grammar.{type Grammar}
import glimra/textmate/grammar_compiler
import glimra/textmate/line_tokens.{type Token}
import glimra/textmate/raw_grammar.{type RawGrammar}
import glimra/textmate/state_stack.{type StateStack, StateStackNull}
import glimra/textmate/tokenize

/// A token with scope information
pub type TokenWithScopes {
  TokenWithScopes(
    /// Start index in the line
    start_index: Int,
    /// End index in the line
    end_index: Int,
    /// Scope names (most general to most specific)
    scopes: List(String),
  )
}

/// Result from tokenizing a line with scope information
pub type TokenizeLineResult {
  TokenizeLineResult(
    /// Tokens with their scopes
    tokens: List(TokenWithScopes),
    /// The state stack for the next line
    rule_stack: StateStack,
  )
}

/// Get the initial state for tokenization
pub fn initial() -> StateStack {
  StateStackNull
}

/// Compile a raw grammar into a Grammar ready for tokenization
///
/// Note: For embedded language support, use compile_grammar_with_externals
/// or the Registry which automatically handles external grammar resolution.
pub fn compile_grammar(
  raw: RawGrammar,
  scanner_factory: fn(List(String)) -> OnigScanner,
) -> Grammar {
  grammar_compiler.compile_grammar(raw, dict.new(), scanner_factory)
}

/// Tokenize a line of text, returning tokens with scope information
///
/// Parameters:
/// - grammar: The compiled grammar
/// - line: The line of text to tokenize
/// - prev_state: The state from the previous line (use initial() for first line)
///
/// Returns tokens with scope information and the updated state
pub fn tokenize_line(
  grammar: Grammar,
  line: String,
  prev_state: StateStack,
) -> TokenizeLineResult {
  let is_first_line = prev_state == StateStackNull
  let result =
    tokenize.tokenize_line(grammar, line, is_first_line, prev_state, True)

  // Convert internal tokens to TokenWithScopes
  let tokens =
    result.tokens
    |> list.map(token_to_token_with_scopes)

  TokenizeLineResult(tokens: tokens, rule_stack: result.rule_stack)
}

/// Tokenize multiple lines of code
///
/// Returns a list of token lists (one per line) and the final state
pub fn tokenize_lines(
  grammar: Grammar,
  lines: List(String),
) -> #(List(List(TokenWithScopes)), StateStack) {
  tokenize_lines_impl(grammar, lines, initial(), [])
}

fn tokenize_lines_impl(
  grammar: Grammar,
  lines: List(String),
  state: StateStack,
  acc: List(List(TokenWithScopes)),
) -> #(List(List(TokenWithScopes)), StateStack) {
  case lines {
    [] -> #(list.reverse(acc), state)
    [line, ..rest] -> {
      let result = tokenize_line(grammar, line, state)
      tokenize_lines_impl(grammar, rest, result.rule_stack, [
        result.tokens,
        ..acc
      ])
    }
  }
}

/// Convert internal Token to public TokenWithScopes
fn token_to_token_with_scopes(token: Token) -> TokenWithScopes {
  TokenWithScopes(
    start_index: token.start_index,
    end_index: token.end_index,
    scopes: token.scopes,
  )
}
