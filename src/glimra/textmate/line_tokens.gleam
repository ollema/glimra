//// Token accumulation during line tokenization
////
//// LineTokens collects tokens as the tokenizer processes a line.
//// It supports both regular tokens (with scope names) and binary
//// tokens (packed metadata format for performance).

import gleam/list
import gleam/option.{type Option, None, Some}
import glimra/textmate/state_stack.{
  type AttributedScopeStack, type StateStack, StateStackFrame, StateStackNull,
  attributed_get_scope_names, attributed_get_token_attributes,
}

/// A token with scope names (used for explanation/debugging)
pub type Token {
  Token(
    /// Start position in the line
    start_index: Int,
    /// End position in the line
    end_index: Int,
    /// List of scope names from root to innermost
    scopes: List(String),
  )
}

/// A binary token with packed metadata
pub type BinaryToken {
  BinaryToken(
    /// Start position in the line
    start_index: Int,
    /// Packed metadata (language ID, token type, font style, colors)
    metadata: Int,
  )
}

/// LineTokens accumulator for building tokens during tokenization
pub type LineTokens {
  LineTokens(
    /// Whether to emit binary tokens (vs regular tokens)
    emit_binary: Bool,
    /// Regular tokens (when emit_binary is False)
    tokens: List(Token),
    /// Binary tokens (when emit_binary is True)
    binary_tokens: List(BinaryToken),
    /// End index of the last token produced
    last_token_end_index: Int,
    /// The length of the line being tokenized
    line_length: Int,
  )
}

/// Create a new LineTokens accumulator
pub fn new(emit_binary: Bool, line_length: Int) -> LineTokens {
  LineTokens(
    emit_binary: emit_binary,
    tokens: [],
    binary_tokens: [],
    last_token_end_index: 0,
    line_length: line_length,
  )
}

/// Produce a token from the current state stack up to the given end index
pub fn produce(
  line_tokens: LineTokens,
  stack: StateStack,
  end_index: Int,
) -> LineTokens {
  case stack {
    StateStackNull -> line_tokens
    StateStackFrame(content_name_scopes: Some(scopes), ..) ->
      produce_from_scopes(line_tokens, scopes, end_index)
    StateStackFrame(content_name_scopes: None, name_scopes: Some(scopes), ..) ->
      produce_from_scopes(line_tokens, scopes, end_index)
    StateStackFrame(..) -> line_tokens
  }
}

/// Produce a token from an attributed scope stack
pub fn produce_from_scopes(
  line_tokens: LineTokens,
  scopes: AttributedScopeStack,
  end_index: Int,
) -> LineTokens {
  // Clamp end_index to line_length to avoid producing tokens beyond the line
  // This can happen when captures match the appended newline character
  let end_index = case end_index > line_tokens.line_length {
    True -> line_tokens.line_length
    False -> end_index
  }

  // Don't produce empty tokens or tokens that overlap
  case end_index <= line_tokens.last_token_end_index {
    True -> line_tokens
    False -> {
      case line_tokens.emit_binary {
        True -> produce_binary_token(line_tokens, scopes, end_index)
        False -> produce_regular_token(line_tokens, scopes, end_index)
      }
    }
  }
}

/// Produce a regular token with scope names
fn produce_regular_token(
  line_tokens: LineTokens,
  scopes: AttributedScopeStack,
  end_index: Int,
) -> LineTokens {
  let scope_names = attributed_get_scope_names(scopes)
  let token =
    Token(
      start_index: line_tokens.last_token_end_index,
      end_index: end_index,
      scopes: scope_names,
    )
  LineTokens(
    ..line_tokens,
    tokens: [token, ..line_tokens.tokens],
    last_token_end_index: end_index,
  )
}

/// Produce a binary token with packed metadata
fn produce_binary_token(
  line_tokens: LineTokens,
  scopes: AttributedScopeStack,
  end_index: Int,
) -> LineTokens {
  let metadata = attributed_get_token_attributes(scopes)

  // Check if we can merge with the previous token (same metadata)
  case line_tokens.binary_tokens {
    [BinaryToken(metadata: prev_metadata, ..), ..] if prev_metadata == metadata ->
      // Same metadata - just extend the previous token by updating last_token_end_index
      LineTokens(..line_tokens, last_token_end_index: end_index)
    _ -> {
      // Different metadata - create new token
      let token =
        BinaryToken(
          start_index: line_tokens.last_token_end_index,
          metadata: metadata,
        )
      LineTokens(
        ..line_tokens,
        binary_tokens: [token, ..line_tokens.binary_tokens],
        last_token_end_index: end_index,
      )
    }
  }
}

/// Get the final result as regular tokens
pub fn get_result(line_tokens: LineTokens, stack: StateStack) -> List(Token) {
  // Ensure final token covers to end of line (excluding newline)
  let final_tokens = finalize_tokens(line_tokens, stack)

  // Reverse to get tokens in order
  let tokens = list.reverse(final_tokens.tokens)

  // Handle empty line case - ensure at least one token
  case tokens {
    [] -> {
      case stack {
        StateStackNull -> [
          Token(start_index: 0, end_index: line_tokens.line_length, scopes: []),
        ]
        StateStackFrame(content_name_scopes: Some(scopes), ..) -> {
          let scope_names = attributed_get_scope_names(scopes)
          [
            Token(
              start_index: 0,
              end_index: line_tokens.line_length,
              scopes: scope_names,
            ),
          ]
        }
        StateStackFrame(name_scopes: Some(scopes), ..) -> {
          let scope_names = attributed_get_scope_names(scopes)
          [
            Token(
              start_index: 0,
              end_index: line_tokens.line_length,
              scopes: scope_names,
            ),
          ]
        }
        _ -> [
          Token(start_index: 0, end_index: line_tokens.line_length, scopes: []),
        ]
      }
    }
    _ -> tokens
  }
}

/// Finalize tokens by producing any remaining content
fn finalize_tokens(line_tokens: LineTokens, stack: StateStack) -> LineTokens {
  // Produce token to end of line if needed
  case line_tokens.last_token_end_index < line_tokens.line_length {
    True -> produce(line_tokens, stack, line_tokens.line_length)
    False -> line_tokens
  }
}

/// Local scope stack for handling captures within a match.
/// Captures create temporary scopes that are nested based on
/// their position ranges.
pub type LocalStackElement {
  LocalStackElement(
    /// The attributed scope for this capture
    scopes: AttributedScopeStack,
    /// End position of this capture (where to pop)
    end_pos: Int,
  )
}

/// Pop local stack elements that have ended
pub fn pop_local_stack(
  local_stack: List(LocalStackElement),
  current_pos: Int,
  line_tokens: LineTokens,
) -> #(List(LocalStackElement), LineTokens) {
  pop_local_stack_impl(local_stack, current_pos, line_tokens)
}

fn pop_local_stack_impl(
  local_stack: List(LocalStackElement),
  current_pos: Int,
  line_tokens: LineTokens,
) -> #(List(LocalStackElement), LineTokens) {
  case local_stack {
    [] -> #([], line_tokens)
    [LocalStackElement(scopes: scopes, end_pos: end_pos), ..rest] -> {
      case end_pos <= current_pos {
        True -> {
          // Pop this element and produce its token
          let updated_tokens = produce_from_scopes(line_tokens, scopes, end_pos)
          pop_local_stack_impl(rest, current_pos, updated_tokens)
        }
        False -> #(local_stack, line_tokens)
      }
    }
  }
}

/// Get current scopes from local stack or main stack
pub fn get_current_scopes(
  local_stack: List(LocalStackElement),
  stack: StateStack,
) -> Option(AttributedScopeStack) {
  case local_stack {
    [LocalStackElement(scopes: scopes, ..), ..] -> Some(scopes)
    [] ->
      case stack {
        StateStackNull -> None
        StateStackFrame(content_name_scopes: Some(scopes), ..) -> Some(scopes)
        StateStackFrame(name_scopes: Some(scopes), ..) -> Some(scopes)
        StateStackFrame(..) -> None
      }
  }
}
