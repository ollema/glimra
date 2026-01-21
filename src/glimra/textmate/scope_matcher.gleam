//// Scope Matcher - utilities for matching TextMate scopes
////
//// This module provides functions to match scope names against patterns
//// according to TextMate matching rules.

import gleam/list
import gleam/string

/// A scope stack represents a path of nested scopes
/// e.g., ["source.gleam", "meta.function", "entity.name.function"]
pub type ScopeStack {
  ScopeStack(parent: ScopeStack, scope_name: String)
  ScopeStackNull
}

/// Build a scope stack from a list of scope names
pub fn from_scopes(scopes: List(String)) -> ScopeStack {
  list.fold(scopes, ScopeStackNull, fn(stack, scope) {
    ScopeStack(parent: stack, scope_name: scope)
  })
}

/// Get all scopes from a stack as a list (outermost to innermost)
pub fn to_list(stack: ScopeStack) -> List(String) {
  to_list_loop(stack, [])
}

fn to_list_loop(stack: ScopeStack, acc: List(String)) -> List(String) {
  case stack {
    ScopeStackNull -> acc
    ScopeStack(parent, scope_name) -> to_list_loop(parent, [scope_name, ..acc])
  }
}

/// Check if a single scope name matches a pattern
/// e.g., "keyword" matches "keyword.operator" (pattern is prefix)
/// Pattern matching follows TextMate rules:
/// - Exact match: "keyword" == "keyword"
/// - Prefix match: "keyword" matches "keyword.operator" (scope starts with pattern + ".")
pub fn matches_scope(scope_name: String, pattern: String) -> Bool {
  case scope_name == pattern {
    True -> True
    False -> {
      // Check if scope_name starts with pattern followed by "."
      let pattern_len = string.length(pattern)
      case string.length(scope_name) > pattern_len {
        True -> {
          let prefix = string.slice(scope_name, 0, pattern_len)
          let next_char = string.slice(scope_name, pattern_len, 1)
          prefix == pattern && next_char == "."
        }
        False -> False
      }
    }
  }
}

/// Check if a scope path matches a list of parent scope constraints
/// Parent scopes are in reverse order (deepest first)
/// Supports the child combinator ">" for direct parent matching
pub fn matches_parent_scopes(
  scope_path: ScopeStack,
  parent_scopes: List(String),
) -> Bool {
  case parent_scopes {
    [] -> True
    _ -> matches_parent_scopes_loop(scope_path, parent_scopes)
  }
}

fn matches_parent_scopes_loop(
  scope_path: ScopeStack,
  parent_scopes: List(String),
) -> Bool {
  case parent_scopes {
    [] -> True
    [pattern, ..rest] -> {
      // Check for child combinator
      case pattern == ">" {
        True -> {
          // Next pattern must match immediately
          case rest {
            [] -> False
            [next_pattern, ..remaining] -> {
              case scope_path {
                ScopeStackNull -> False
                ScopeStack(parent, scope_name) -> {
                  case matches_scope(scope_name, next_pattern) {
                    True -> matches_parent_scopes_loop(parent, remaining)
                    False -> False
                  }
                }
              }
            }
          }
        }
        False -> {
          // Regular pattern - search up the stack
          search_parent_stack(scope_path, pattern, rest)
        }
      }
    }
  }
}

fn search_parent_stack(
  scope_path: ScopeStack,
  pattern: String,
  remaining: List(String),
) -> Bool {
  case scope_path {
    ScopeStackNull -> False
    ScopeStack(parent, scope_name) -> {
      case matches_scope(scope_name, pattern) {
        True -> matches_parent_scopes_loop(parent, remaining)
        False -> search_parent_stack(parent, pattern, remaining)
      }
    }
  }
}
