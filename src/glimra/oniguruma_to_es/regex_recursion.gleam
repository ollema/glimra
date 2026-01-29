//// Transforms recursion tokens into expanded non-recursive patterns.
////
//// This module handles `(?R=N)` for global recursion and `\g<name&R=N>` for local
//// subroutine recursion. It expands these tokens by repeating the pattern with
//// depth-suffixed capture group names.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import glimra/oniguruma_to_es/regex_utilities

// ============================================================================
// Types
// ============================================================================

/// Configuration for the recursion function
pub type RecursionConfig {
  RecursionConfig(
    capture_transfers: Dict(Int, List(Int)),
    hidden_captures: List(Int),
  )
}

/// Result of the recursion function
pub type RecursionResult {
  RecursionResult(
    pattern: String,
    capture_transfers: Dict(Int, List(Int)),
    hidden_captures: List(Int),
  )
}

/// Default configuration with empty transfer maps
pub fn default_config() -> RecursionConfig {
  RecursionConfig(capture_transfers: dict.new(), hidden_captures: [])
}

// ============================================================================
// Constants
// ============================================================================

const overlapping_recursion_msg = "Cannot use multiple overlapping recursions"

// ============================================================================
// Main Function
// ============================================================================

/// Transform recursion tokens into expanded non-recursive patterns.
///
/// Handles:
/// - `(?R=N)` - global recursion with max depth N
/// - `\g<name&R=N>` - local subroutine recursion with max depth N
///
/// Mode is always 'external' (this is called by transpilers).
pub fn recursion(
  pattern: String,
  config: RecursionConfig,
) -> Result(RecursionResult, String) {
  let hidden_captures = config.hidden_captures
  let capture_transfers = config.capture_transfers

  // Quick check - if no recursive token exists, return unchanged
  case has_recursive_token(pattern) {
    False ->
      Ok(RecursionResult(
        pattern: pattern,
        capture_transfers: capture_transfers,
        hidden_captures: hidden_captures,
      ))
    True -> do_recursion(pattern, capture_transfers, hidden_captures)
  }
}

/// Check if pattern contains any recursive tokens
fn has_recursive_token(pattern: String) -> Bool {
  // Look for (?R= or \g<..&R=
  has_global_recursion_token(pattern, 0, 0)
  || has_local_recursion_token(pattern, 0, 0)
}

fn has_global_recursion_token(
  pattern: String,
  pos: Int,
  num_char_classes_open: Int,
) -> Bool {
  let remaining = string.drop_start(pattern, pos)
  case string.length(remaining) {
    0 -> False
    _ -> {
      case string.starts_with(remaining, "\\") {
        True ->
          has_global_recursion_token(pattern, pos + 2, num_char_classes_open)
        False -> {
          case string.starts_with(remaining, "[") {
            True ->
              has_global_recursion_token(
                pattern,
                pos + 1,
                num_char_classes_open + 1,
              )
            False -> {
              case
                string.starts_with(remaining, "]") && num_char_classes_open > 0
              {
                True ->
                  has_global_recursion_token(
                    pattern,
                    pos + 1,
                    num_char_classes_open - 1,
                  )
                False if num_char_classes_open > 0 ->
                  has_global_recursion_token(
                    pattern,
                    pos + 1,
                    num_char_classes_open,
                  )
                False -> {
                  case string.starts_with(remaining, "(?R=") {
                    True -> True
                    False ->
                      has_global_recursion_token(
                        pattern,
                        pos + 1,
                        num_char_classes_open,
                      )
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

fn has_local_recursion_token(
  pattern: String,
  pos: Int,
  num_char_classes_open: Int,
) -> Bool {
  let remaining = string.drop_start(pattern, pos)
  case string.length(remaining) {
    0 -> False
    _ -> {
      case string.starts_with(remaining, "\\") {
        True -> {
          // Check for \g<...&R=
          case string.starts_with(remaining, "\\g<") {
            True -> {
              // Look for &R= inside
              case string.contains(remaining, "&R=") {
                True -> True
                False ->
                  has_local_recursion_token(
                    pattern,
                    pos + 2,
                    num_char_classes_open,
                  )
              }
            }
            False ->
              has_local_recursion_token(pattern, pos + 2, num_char_classes_open)
          }
        }
        False -> {
          case string.starts_with(remaining, "[") {
            True ->
              has_local_recursion_token(
                pattern,
                pos + 1,
                num_char_classes_open + 1,
              )
            False -> {
              case
                string.starts_with(remaining, "]") && num_char_classes_open > 0
              {
                True ->
                  has_local_recursion_token(
                    pattern,
                    pos + 1,
                    num_char_classes_open - 1,
                  )
                False if num_char_classes_open > 0 ->
                  has_local_recursion_token(
                    pattern,
                    pos + 1,
                    num_char_classes_open,
                  )
                False ->
                  has_local_recursion_token(
                    pattern,
                    pos + 1,
                    num_char_classes_open,
                  )
              }
            }
          }
        }
      }
    }
  }
}

fn do_recursion(
  pattern: String,
  capture_transfers: Dict(Int, List(Int)),
  hidden_captures: List(Int),
) -> Result(RecursionResult, String) {
  // Check for numbered backrefs (we don't support them with global recursion)
  let has_numbered_backref = has_numbered_backref_token(pattern, 0, 0)

  // Parse the pattern token by token
  let initial_state =
    ProcessState(
      pattern: pattern,
      pos: 0,
      num_char_classes_open: 0,
      num_captures_passed: 0,
      group_contents_start_pos: dict.new(),
      open_groups: [],
      has_recursed: False,
      capture_transfers: capture_transfers,
      hidden_captures: hidden_captures,
      added_hidden_captures: [],
    )

  case process_tokens_loop(initial_state, has_numbered_backref) {
    Error(e) -> Error(e)
    Ok(final_state) -> {
      // Reverse added_hidden_captures since we built it by prepending
      let final_hidden =
        list.append(
          final_state.hidden_captures,
          list.reverse(final_state.added_hidden_captures),
        )
      Ok(RecursionResult(
        pattern: final_state.pattern,
        capture_transfers: final_state.capture_transfers,
        hidden_captures: final_hidden,
      ))
    }
  }
}

fn has_numbered_backref_token(
  pattern: String,
  pos: Int,
  num_char_classes_open: Int,
) -> Bool {
  let remaining = string.drop_start(pattern, pos)
  case string.length(remaining) {
    0 -> False
    _ -> {
      case string.starts_with(remaining, "\\") {
        True -> {
          // Check for \1-9
          let after_slash = string.drop_start(remaining, 1)
          case string.pop_grapheme(after_slash) {
            Ok(#(c, _)) -> {
              case
                c == "1"
                || c == "2"
                || c == "3"
                || c == "4"
                || c == "5"
                || c == "6"
                || c == "7"
                || c == "8"
                || c == "9"
              {
                True if num_char_classes_open == 0 -> True
                _ ->
                  has_numbered_backref_token(
                    pattern,
                    pos + 2,
                    num_char_classes_open,
                  )
              }
            }
            Error(_) -> False
          }
        }
        False -> {
          case string.starts_with(remaining, "[") {
            True ->
              has_numbered_backref_token(
                pattern,
                pos + 1,
                num_char_classes_open + 1,
              )
            False -> {
              case
                string.starts_with(remaining, "]") && num_char_classes_open > 0
              {
                True ->
                  has_numbered_backref_token(
                    pattern,
                    pos + 1,
                    num_char_classes_open - 1,
                  )
                False ->
                  has_numbered_backref_token(
                    pattern,
                    pos + 1,
                    num_char_classes_open,
                  )
              }
            }
          }
        }
      }
    }
  }
}

/// State for processing tokens
type ProcessState {
  ProcessState(
    pattern: String,
    pos: Int,
    num_char_classes_open: Int,
    num_captures_passed: Int,
    group_contents_start_pos: Dict(String, Int),
    open_groups: List(OpenGroup),
    has_recursed: Bool,
    capture_transfers: Dict(Int, List(Int)),
    hidden_captures: List(Int),
    added_hidden_captures: List(Int),
  )
}

/// Tracking info for an open group
type OpenGroup {
  OpenGroup(
    num: Option(Int),
    name: Option(String),
    has_recursed_within: Bool,
    is_capturing: Bool,
  )
}

fn process_tokens_loop(
  state: ProcessState,
  has_numbered_backref: Bool,
) -> Result(ProcessState, String) {
  let remaining = string.drop_start(state.pattern, state.pos)

  case string.length(remaining) {
    0 -> Ok(state)
    _ -> {
      // Check for escape sequence
      case string.starts_with(remaining, "\\") {
        True -> {
          // Check for \g<name&R=N>
          case string.starts_with(remaining, "\\g<") {
            True -> {
              case parse_local_recursion_at(remaining) {
                Some(#(name_or_num, depth_str, token_len)) -> {
                  handle_local_recursion(
                    state,
                    has_numbered_backref,
                    name_or_num,
                    depth_str,
                    state.pos,
                    token_len,
                  )
                }
                None -> {
                  // Regular \g<...> - skip
                  process_tokens_loop(
                    ProcessState(..state, pos: state.pos + 2),
                    has_numbered_backref,
                  )
                }
              }
            }
            False -> {
              // Regular escape - skip two chars
              process_tokens_loop(
                ProcessState(..state, pos: state.pos + 2),
                has_numbered_backref,
              )
            }
          }
        }
        False -> {
          // Check for character class
          case string.starts_with(remaining, "[") {
            True -> {
              process_tokens_loop(
                ProcessState(
                  ..state,
                  pos: state.pos + 1,
                  num_char_classes_open: state.num_char_classes_open + 1,
                ),
                has_numbered_backref,
              )
            }
            False -> {
              case
                string.starts_with(remaining, "]")
                && state.num_char_classes_open > 0
              {
                True -> {
                  process_tokens_loop(
                    ProcessState(
                      ..state,
                      pos: state.pos + 1,
                      num_char_classes_open: state.num_char_classes_open - 1,
                    ),
                    has_numbered_backref,
                  )
                }
                False if state.num_char_classes_open > 0 -> {
                  // Inside char class - skip
                  process_tokens_loop(
                    ProcessState(..state, pos: state.pos + 1),
                    has_numbered_backref,
                  )
                }
                False -> {
                  // Check for (?R=N) - global recursion
                  case string.starts_with(remaining, "(?R=") {
                    True -> {
                      case parse_global_recursion_at(remaining) {
                        Some(#(depth_str, token_len)) -> {
                          handle_global_recursion(
                            state,
                            depth_str,
                            has_numbered_backref,
                            state.pos,
                            token_len,
                          )
                        }
                        None -> {
                          process_tokens_loop(
                            ProcessState(..state, pos: state.pos + 1),
                            has_numbered_backref,
                          )
                        }
                      }
                    }
                    False -> {
                      // Check for named capture: (?<name>
                      case string.starts_with(remaining, "(?<") {
                        True -> {
                          case parse_named_capture_at(remaining) {
                            Some(#(capture_name, token_len)) -> {
                              let new_num = state.num_captures_passed + 1
                              let new_group_starts =
                                state.group_contents_start_pos
                                |> dict.insert(
                                  int.to_string(new_num),
                                  state.pos + token_len,
                                )
                                |> dict.insert(
                                  capture_name,
                                  state.pos + token_len,
                                )
                              let new_open_groups = [
                                OpenGroup(
                                  num: Some(new_num),
                                  name: Some(capture_name),
                                  has_recursed_within: False,
                                  is_capturing: True,
                                ),
                                ..state.open_groups
                              ]
                              process_tokens_loop(
                                ProcessState(
                                  ..state,
                                  pos: state.pos + token_len,
                                  num_captures_passed: new_num,
                                  group_contents_start_pos: new_group_starts,
                                  open_groups: new_open_groups,
                                ),
                                has_numbered_backref,
                              )
                            }
                            None -> {
                              // (?<= or (?<! - lookbehind, treat like non-capturing group
                              // We need to track it on the stack for proper `)` matching
                              let new_open_groups = [
                                OpenGroup(
                                  num: None,
                                  name: None,
                                  has_recursed_within: False,
                                  is_capturing: False,
                                ),
                                ..state.open_groups
                              ]
                              process_tokens_loop(
                                ProcessState(
                                  ..state,
                                  pos: state.pos + 1,
                                  open_groups: new_open_groups,
                                ),
                                has_numbered_backref,
                              )
                            }
                          }
                        }
                        False -> {
                          // Check for group opening
                          case string.starts_with(remaining, "(") {
                            True -> {
                              let rest = string.drop_start(remaining, 1)
                              let is_unnamed_capture =
                                !string.starts_with(rest, "?")

                              case is_unnamed_capture {
                                True -> {
                                  let new_num = state.num_captures_passed + 1
                                  let new_group_starts =
                                    dict.insert(
                                      state.group_contents_start_pos,
                                      int.to_string(new_num),
                                      state.pos + 1,
                                    )
                                  // Add capturing group with its number
                                  let new_open_groups = [
                                    OpenGroup(
                                      num: Some(new_num),
                                      name: None,
                                      has_recursed_within: False,
                                      is_capturing: True,
                                    ),
                                    ..state.open_groups
                                  ]
                                  process_tokens_loop(
                                    ProcessState(
                                      ..state,
                                      pos: state.pos + 1,
                                      num_captures_passed: new_num,
                                      group_contents_start_pos: new_group_starts,
                                      open_groups: new_open_groups,
                                    ),
                                    has_numbered_backref,
                                  )
                                }
                                False -> {
                                  // Non-capturing group - still track it for proper nesting
                                  let new_open_groups = [
                                    OpenGroup(
                                      num: None,
                                      name: None,
                                      has_recursed_within: False,
                                      is_capturing: False,
                                    ),
                                    ..state.open_groups
                                  ]
                                  process_tokens_loop(
                                    ProcessState(
                                      ..state,
                                      pos: state.pos + 1,
                                      open_groups: new_open_groups,
                                    ),
                                    has_numbered_backref,
                                  )
                                }
                              }
                            }
                            False -> {
                              // Check for closing paren
                              case string.starts_with(remaining, ")") {
                                True -> {
                                  // Pop the most recent group (capturing or not)
                                  let new_open_groups = case state.open_groups {
                                    [_, ..rest] -> rest
                                    [] -> []
                                  }
                                  process_tokens_loop(
                                    ProcessState(
                                      ..state,
                                      pos: state.pos + 1,
                                      open_groups: new_open_groups,
                                    ),
                                    has_numbered_backref,
                                  )
                                }
                                False -> {
                                  // Regular character
                                  process_tokens_loop(
                                    ProcessState(..state, pos: state.pos + 1),
                                    has_numbered_backref,
                                  )
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Parse (?R=N) token, returns (depth_str, total_token_length) or None
fn parse_global_recursion_at(s: String) -> Option(#(String, Int)) {
  // s starts with "(?R="
  let after_prefix = string.drop_start(s, 4)
  case extract_until_char(after_prefix, ")") {
    Some(depth_str) -> Some(#(depth_str, 4 + string.length(depth_str) + 1))
    None -> None
  }
}

/// Parse \g<name&R=N> token, returns (name_or_num, depth_str, total_token_length) or None
fn parse_local_recursion_at(s: String) -> Option(#(String, String, Int)) {
  // s starts with "\g<"
  let after_prefix = string.drop_start(s, 3)
  // Find the &R= part
  case find_and_r_equals(after_prefix) {
    Some(#(name_or_num, rest_after_and_r)) -> {
      // rest_after_and_r starts after "&R="
      case extract_until_char(rest_after_and_r, ">") {
        Some(depth_str) -> {
          let total_len =
            3 + string.length(name_or_num) + 3 + string.length(depth_str) + 1
          Some(#(name_or_num, depth_str, total_len))
        }
        None -> None
      }
    }
    None -> None
  }
}

fn find_and_r_equals(s: String) -> Option(#(String, String)) {
  do_find_and_r_equals(s, "")
}

fn do_find_and_r_equals(s: String, acc: String) -> Option(#(String, String)) {
  case string.starts_with(s, "&R=") {
    True -> Some(#(acc, string.drop_start(s, 3)))
    False -> {
      case string.pop_grapheme(s) {
        Error(_) -> None
        Ok(#(c, rest)) -> {
          // Stop at ">" which ends the \g<...> token
          case c == ">" {
            True -> None
            False -> do_find_and_r_equals(rest, acc <> c)
          }
        }
      }
    }
  }
}

/// Parse (?<name> token, returns (name, total_token_length) or None if lookbehind
fn parse_named_capture_at(s: String) -> Option(#(String, Int)) {
  // s starts with "(?<"
  let after_prefix = string.drop_start(s, 3)
  // Check it's not (?<= or (?<!
  case
    string.starts_with(after_prefix, "=")
    || string.starts_with(after_prefix, "!")
  {
    True -> None
    False -> {
      case extract_until_char(after_prefix, ">") {
        Some(name) -> Some(#(name, 3 + string.length(name) + 1))
        None -> None
      }
    }
  }
}

fn extract_until_char(s: String, delim: String) -> Option(String) {
  do_extract_until_char(s, delim, "")
}

fn do_extract_until_char(
  s: String,
  delim: String,
  acc: String,
) -> Option(String) {
  case string.pop_grapheme(s) {
    Error(_) -> None
    Ok(#(char, rest)) -> {
      case char == delim {
        True -> Some(acc)
        False -> do_extract_until_char(rest, delim, acc <> char)
      }
    }
  }
}

fn handle_global_recursion(
  state: ProcessState,
  depth_str: String,
  has_numbered_backref: Bool,
  abs_index: Int,
  match_len: Int,
) -> Result(ProcessState, String) {
  case assert_max_in_bounds(depth_str) {
    Error(e) -> Error(e)
    Ok(depth) -> {
      case state.has_recursed {
        True -> Error(overlapping_recursion_msg)
        False -> {
          case has_numbered_backref {
            True -> Error("Backrefs cannot be used with global recursion")
            False -> {
              // Check for overlapping recursion in remaining pattern
              let right =
                string.drop_start(state.pattern, abs_index + match_len)
              case has_recursive_token(right) {
                True -> Error(overlapping_recursion_msg)
                False -> {
                  let left = string.slice(state.pattern, 0, abs_index)
                  let reps = depth - 1

                  let #(new_pattern, new_added_hidden) =
                    make_recursive(
                      left,
                      right,
                      reps,
                      False,
                      state.hidden_captures,
                      state.added_hidden_captures,
                      state.num_captures_passed,
                    )

                  let new_capture_transfers =
                    map_capture_transfers(
                      state.capture_transfers,
                      left,
                      reps,
                      list.length(new_added_hidden)
                        - list.length(state.added_hidden_captures),
                      0,
                      state.num_captures_passed,
                    )

                  // Global recursion consumes the rest - we're done parsing
                  Ok(
                    ProcessState(
                      ..state,
                      pattern: new_pattern,
                      has_recursed: True,
                      capture_transfers: new_capture_transfers,
                      added_hidden_captures: new_added_hidden,
                    ),
                  )
                }
              }
            }
          }
        }
      }
    }
  }
}

fn handle_local_recursion(
  state: ProcessState,
  has_numbered_backref: Bool,
  name_or_num: String,
  depth_str: String,
  abs_index: Int,
  match_len: Int,
) -> Result(ProcessState, String) {
  case assert_max_in_bounds(depth_str) {
    Error(e) -> Error(e)
    Ok(depth) -> {
      // Find if we're within the referenced group
      case find_reffed_group(state.open_groups, name_or_num) {
        None ->
          Error(
            "Recursive \\g cannot be used outside the referenced group \""
            <> name_or_num
            <> "\"",
          )
        Some(#(is_recursed_within, group_index)) -> {
          case is_recursed_within {
            True -> Error(overlapping_recursion_msg)
            False -> {
              let start_pos =
                dict.get(state.group_contents_start_pos, name_or_num)
                |> result.unwrap(0)
              let group_contents =
                regex_utilities.get_group_contents(state.pattern, start_pos)

              // Check for numbered backrefs if the group contains captures
              case has_numbered_backref {
                True -> {
                  let has_capture_in_group =
                    regex_utilities.count_captures(
                      group_contents,
                      Some(regex_utilities.Default),
                    )
                    > 0
                  case has_capture_in_group {
                    True ->
                      Error(
                        "Backrefs cannot be used with recursion of capturing groups",
                      )
                    False ->
                      do_local_expansion(
                        state,
                        has_numbered_backref,
                        depth,
                        abs_index,
                        match_len,
                        start_pos,
                        group_contents,
                        group_index,
                      )
                  }
                }
                False ->
                  do_local_expansion(
                    state,
                    has_numbered_backref,
                    depth,
                    abs_index,
                    match_len,
                    start_pos,
                    group_contents,
                    group_index,
                  )
              }
            }
          }
        }
      }
    }
  }
}

fn do_local_expansion(
  state: ProcessState,
  has_numbered_backref: Bool,
  depth: Int,
  abs_index: Int,
  match_len: Int,
  start_pos: Int,
  group_contents: String,
  group_index: Int,
) -> Result(ProcessState, String) {
  let group_contents_left =
    string.slice(state.pattern, start_pos, abs_index - start_pos)
  let group_contents_right =
    string.drop_start(
      group_contents,
      string.length(group_contents_left) + match_len,
    )

  let num_added_pre = list.length(state.added_hidden_captures)
  let reps = depth - 1

  let #(expansion, new_added_hidden) =
    make_recursive(
      group_contents_left,
      group_contents_right,
      reps,
      True,
      state.hidden_captures,
      state.added_hidden_captures,
      state.num_captures_passed,
    )

  let new_capture_transfers =
    map_capture_transfers(
      state.capture_transfers,
      group_contents_left,
      reps,
      list.length(new_added_hidden) - num_added_pre,
      num_added_pre,
      state.num_captures_passed,
    )

  let pre = string.slice(state.pattern, 0, start_pos)
  let post =
    string.drop_start(state.pattern, start_pos + string.length(group_contents))
  let new_pattern = pre <> expansion <> post

  // Calculate new position
  let expansion_len = string.length(expansion)
  let new_pos =
    abs_index
    + expansion_len
    - string.length(group_contents_left)
    - string.length(group_contents_right)

  // Mark all open groups as having recursed within
  let new_open_groups =
    list.index_map(state.open_groups, fn(g, i) {
      case i <= group_index {
        True -> OpenGroup(..g, has_recursed_within: True)
        False -> g
      }
    })

  let new_state =
    ProcessState(
      ..state,
      pattern: new_pattern,
      pos: new_pos,
      has_recursed: True,
      capture_transfers: new_capture_transfers,
      added_hidden_captures: new_added_hidden,
      open_groups: new_open_groups,
    )

  process_tokens_loop(new_state, has_numbered_backref)
}

fn find_reffed_group(
  open_groups: List(OpenGroup),
  name_or_num: String,
) -> Option(#(Bool, Int)) {
  let as_num = int.parse(name_or_num)
  find_reffed_group_loop(open_groups, name_or_num, as_num, 0)
}

fn find_reffed_group_loop(
  groups: List(OpenGroup),
  name: String,
  as_num: Result(Int, Nil),
  index: Int,
) -> Option(#(Bool, Int)) {
  case groups {
    [] -> None
    [g, ..rest] -> {
      let name_matches = case g.name {
        Some(n) -> n == name
        None -> False
      }
      let num_matches = case g.num, as_num {
        Some(n), Ok(target) -> n == target
        _, _ -> False
      }
      case name_matches || num_matches {
        True -> Some(#(g.has_recursed_within, index))
        False -> find_reffed_group_loop(rest, name, as_num, index + 1)
      }
    }
  }
}

fn assert_max_in_bounds(max_str: String) -> Result(Int, String) {
  let err_msg = "Max depth must be integer between 2 and 100; used " <> max_str

  case int.parse(max_str) {
    Error(_) -> Error(err_msg)
    Ok(max) -> {
      case max >= 2 && max <= 100 {
        True -> Ok(max)
        False -> Error(err_msg)
      }
    }
  }
}

// ============================================================================
// Pattern Building
// ============================================================================

/// Build the recursively expanded pattern.
/// Depth 2: 'left(?:left(?:)right)right'
/// Depth 3: 'left(?:left(?:left(?:)right)right)right'
fn make_recursive(
  left: String,
  right: String,
  reps: Int,
  is_subpattern: Bool,
  hidden_captures: List(Int),
  added_hidden_captures: List(Int),
  num_captures_passed: Int,
) -> #(String, List(Int)) {
  // Collect names in recursed pattern if subpattern
  let names_in_recursed = case is_subpattern {
    True -> set.from_list(regex_utilities.collect_capture_names(left <> right))
    False -> set.new()
  }

  // Build the repeated pattern parts
  let #(forward_part, added1) =
    repeat_with_depth(
      "(?:" <> left,
      Forward,
      reps,
      names_in_recursed,
      hidden_captures,
      added_hidden_captures,
      num_captures_passed,
      is_subpattern,
    )

  let #(backward_part, added2) =
    repeat_with_depth(
      right <> ")",
      Backward,
      reps,
      names_in_recursed,
      hidden_captures,
      added1,
      num_captures_passed,
      is_subpattern,
    )

  let result = left <> forward_part <> "(?:)" <> backward_part <> right
  #(result, added2)
}

type Direction {
  Forward
  Backward
}

fn repeat_with_depth(
  pattern: String,
  direction: Direction,
  reps: Int,
  names_in_recursed: Set(String),
  hidden_captures: List(Int),
  added_hidden_captures: List(Int),
  num_captures_passed: Int,
  is_subpattern: Bool,
) -> #(String, List(Int)) {
  let start_num = 2
  do_repeat_with_depth(
    pattern,
    direction,
    reps,
    names_in_recursed,
    hidden_captures,
    added_hidden_captures,
    num_captures_passed,
    is_subpattern,
    0,
    start_num,
    "",
  )
}

fn do_repeat_with_depth(
  pattern: String,
  direction: Direction,
  reps: Int,
  names_in_recursed: Set(String),
  hidden_captures: List(Int),
  added_hidden_captures: List(Int),
  num_captures_passed: Int,
  is_subpattern: Bool,
  i: Int,
  start_num: Int,
  result: String,
) -> #(String, List(Int)) {
  case i >= reps {
    True -> #(result, added_hidden_captures)
    False -> {
      let depth_num = case direction {
        Forward -> i + start_num
        Backward -> reps - i + start_num - 1
      }
      let suffix = "_$" <> int.to_string(depth_num)

      // Replace captures and backrefs with depth-suffixed versions
      // Note: hidden_captures is passed but we don't use the updated version
      // because it only needs to be incremented within a single repetition
      let #(replaced, new_added) =
        replace_captures_and_backrefs(
          pattern,
          suffix,
          names_in_recursed,
          hidden_captures,
          added_hidden_captures,
          num_captures_passed,
          is_subpattern,
        )

      do_repeat_with_depth(
        pattern,
        direction,
        reps,
        names_in_recursed,
        hidden_captures,
        new_added,
        num_captures_passed,
        is_subpattern,
        i + 1,
        start_num,
        result <> replaced,
      )
    }
  }
}

fn replace_captures_and_backrefs(
  pattern: String,
  suffix: String,
  names_in_recursed: Set(String),
  hidden_captures: List(Int),
  added_hidden_captures: List(Int),
  num_captures_passed: Int,
  is_subpattern: Bool,
) -> #(String, List(Int)) {
  // Scan through the pattern and replace:
  // - (?<name> with (?<name_$depth>
  // - ( with ( (but track it as hidden capture)
  // - \k<name> with \k<name_$depth> (if name is in recursed pattern for subpatterns)
  let #(replaced, new_added, _new_hidden) =
    do_replace_captures(
      pattern,
      suffix,
      names_in_recursed,
      is_subpattern,
      added_hidden_captures,
      hidden_captures,
      num_captures_passed,
      0,
      0,
      "",
    )
  #(replaced, new_added)
}

fn do_replace_captures(
  remaining: String,
  suffix: String,
  names_in_recursed: Set(String),
  is_subpattern: Bool,
  added_hidden: List(Int),
  hidden_captures: List(Int),
  num_captures_passed: Int,
  pos: Int,
  num_char_classes_open: Int,
  result: String,
) -> #(String, List(Int), List(Int)) {
  case string.length(remaining) {
    0 -> #(result, added_hidden, hidden_captures)
    _ -> {
      // Check for escape
      case string.starts_with(remaining, "\\") {
        True -> {
          // Check for \k<name>
          case string.starts_with(remaining, "\\k<") {
            True -> {
              let after = string.drop_start(remaining, 3)
              case extract_until_char(after, ">") {
                Some(name) -> {
                  let token_len = 3 + string.length(name) + 1
                  // Check if we should suffix this backref
                  let should_suffix = case is_subpattern {
                    True -> set.contains(names_in_recursed, name)
                    False -> True
                  }
                  let replacement = case should_suffix {
                    True -> "\\k<" <> name <> suffix <> ">"
                    False -> "\\k<" <> name <> ">"
                  }
                  do_replace_captures(
                    string.drop_start(remaining, token_len),
                    suffix,
                    names_in_recursed,
                    is_subpattern,
                    added_hidden,
                    hidden_captures,
                    num_captures_passed,
                    pos + token_len,
                    num_char_classes_open,
                    result <> replacement,
                  )
                }
                None -> {
                  // Malformed - keep as is
                  do_replace_captures(
                    string.drop_start(remaining, 2),
                    suffix,
                    names_in_recursed,
                    is_subpattern,
                    added_hidden,
                    hidden_captures,
                    num_captures_passed,
                    pos + 2,
                    num_char_classes_open,
                    result <> string.slice(remaining, 0, 2),
                  )
                }
              }
            }
            False -> {
              // Regular escape
              do_replace_captures(
                string.drop_start(remaining, 2),
                suffix,
                names_in_recursed,
                is_subpattern,
                added_hidden,
                hidden_captures,
                num_captures_passed,
                pos + 2,
                num_char_classes_open,
                result <> string.slice(remaining, 0, 2),
              )
            }
          }
        }
        False -> {
          // Track char class
          case string.starts_with(remaining, "[") {
            True ->
              do_replace_captures(
                string.drop_start(remaining, 1),
                suffix,
                names_in_recursed,
                is_subpattern,
                added_hidden,
                hidden_captures,
                num_captures_passed,
                pos + 1,
                num_char_classes_open + 1,
                result <> "[",
              )
            False -> {
              case
                string.starts_with(remaining, "]") && num_char_classes_open > 0
              {
                True ->
                  do_replace_captures(
                    string.drop_start(remaining, 1),
                    suffix,
                    names_in_recursed,
                    is_subpattern,
                    added_hidden,
                    hidden_captures,
                    num_captures_passed,
                    pos + 1,
                    num_char_classes_open - 1,
                    result <> "]",
                  )
                False if num_char_classes_open > 0 ->
                  // Inside char class - just copy
                  do_replace_captures(
                    string.drop_start(remaining, 1),
                    suffix,
                    names_in_recursed,
                    is_subpattern,
                    added_hidden,
                    hidden_captures,
                    num_captures_passed,
                    pos + 1,
                    num_char_classes_open,
                    result <> string.slice(remaining, 0, 1),
                  )
                False -> {
                  // Check for named capture
                  case string.starts_with(remaining, "(?<") {
                    True -> {
                      let after = string.drop_start(remaining, 3)
                      // Check not lookbehind
                      case
                        string.starts_with(after, "=")
                        || string.starts_with(after, "!")
                      {
                        True ->
                          do_replace_captures(
                            string.drop_start(remaining, 1),
                            suffix,
                            names_in_recursed,
                            is_subpattern,
                            added_hidden,
                            hidden_captures,
                            num_captures_passed,
                            pos + 1,
                            num_char_classes_open,
                            result <> "(",
                          )
                        False -> {
                          case extract_until_char(after, ">") {
                            Some(name) -> {
                              let token_len = 3 + string.length(name) + 1
                              let replacement = "(?<" <> name <> suffix <> ">"
                              // Track hidden capture: add new capture number
                              let added_capture_num =
                                num_captures_passed
                                + list.length(added_hidden)
                                + 1
                              let new_added_hidden = [
                                added_capture_num,
                                ..added_hidden
                              ]
                              let new_hidden =
                                increment_if_at_least(
                                  hidden_captures,
                                  added_capture_num,
                                )
                              do_replace_captures(
                                string.drop_start(remaining, token_len),
                                suffix,
                                names_in_recursed,
                                is_subpattern,
                                new_added_hidden,
                                new_hidden,
                                num_captures_passed,
                                pos + token_len,
                                num_char_classes_open,
                                result <> replacement,
                              )
                            }
                            None ->
                              do_replace_captures(
                                string.drop_start(remaining, 1),
                                suffix,
                                names_in_recursed,
                                is_subpattern,
                                added_hidden,
                                hidden_captures,
                                num_captures_passed,
                                pos + 1,
                                num_char_classes_open,
                                result <> "(",
                              )
                          }
                        }
                      }
                    }
                    False -> {
                      // Check for unnamed capture
                      case string.starts_with(remaining, "(") {
                        True -> {
                          let after = string.drop_start(remaining, 1)
                          case string.starts_with(after, "?") {
                            True ->
                              // Non-capturing group - just pass through
                              do_replace_captures(
                                after,
                                suffix,
                                names_in_recursed,
                                is_subpattern,
                                added_hidden,
                                hidden_captures,
                                num_captures_passed,
                                pos + 1,
                                num_char_classes_open,
                                result <> "(",
                              )
                            False -> {
                              // Unnamed capture - track it as hidden
                              let added_capture_num =
                                num_captures_passed
                                + list.length(added_hidden)
                                + 1
                              let new_added_hidden = [
                                added_capture_num,
                                ..added_hidden
                              ]
                              let new_hidden =
                                increment_if_at_least(
                                  hidden_captures,
                                  added_capture_num,
                                )
                              do_replace_captures(
                                after,
                                suffix,
                                names_in_recursed,
                                is_subpattern,
                                new_added_hidden,
                                new_hidden,
                                num_captures_passed,
                                pos + 1,
                                num_char_classes_open,
                                result <> "(",
                              )
                            }
                          }
                        }
                        False ->
                          // Regular character
                          do_replace_captures(
                            string.drop_start(remaining, 1),
                            suffix,
                            names_in_recursed,
                            is_subpattern,
                            added_hidden,
                            hidden_captures,
                            num_captures_passed,
                            pos + 1,
                            num_char_classes_open,
                            result <> string.slice(remaining, 0, 1),
                          )
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Increment each value in the list that is >= threshold
fn increment_if_at_least(arr: List(Int), threshold: Int) -> List(Int) {
  list.map(arr, fn(x) {
    case x >= threshold {
      True -> x + 1
      False -> x
    }
  })
}

// ============================================================================
// Capture Transfer Mapping
// ============================================================================

fn map_capture_transfers(
  capture_transfers: Dict(Int, List(Int)),
  left: String,
  reps: Int,
  num_captures_added_in_expansion: Int,
  num_added_hidden_captures_pre_expansion: Int,
  num_captures_passed: Int,
) -> Dict(Int, List(Int)) {
  case dict.size(capture_transfers) > 0 && num_captures_added_in_expansion > 0 {
    False -> capture_transfers
    True -> {
      let num_captures_in_left =
        regex_utilities.count_captures(left, Some(regex_utilities.Default))

      // Is 0 for global recursion
      let recursion_delim_capture_num =
        num_captures_passed
        - num_captures_in_left
        + num_added_hidden_captures_pre_expansion

      let num_captures_in_right =
        { num_captures_added_in_expansion - num_captures_in_left * reps } / reps
      let num_captures_added_in_left = num_captures_in_left * reps

      // Transform each entry in capture_transfers
      dict.fold(capture_transfers, dict.new(), fn(acc, to, from) {
        let new_to = case
          to > recursion_delim_capture_num + num_captures_in_left
        {
          True -> to + num_captures_added_in_expansion
          False -> to
        }

        let new_from =
          list.flat_map(from, fn(f) {
            map_single_from(
              f,
              recursion_delim_capture_num,
              num_captures_in_left,
              num_captures_in_right,
              num_captures_added_in_left,
              num_captures_added_in_expansion,
              reps,
            )
          })

        dict.insert(acc, new_to, new_from)
      })
    }
  }
}

fn map_single_from(
  f: Int,
  recursion_delim_capture_num: Int,
  num_captures_in_left: Int,
  num_captures_in_right: Int,
  num_captures_added_in_left: Int,
  num_captures_added_in_expansion: Int,
  reps: Int,
) -> List(Int) {
  case f <= recursion_delim_capture_num {
    True ->
      // Before the recursed subpattern
      [f]
    False -> {
      case
        f
        > recursion_delim_capture_num
        + num_captures_in_left
        + num_captures_in_right
      {
        True ->
          // After the recursed subpattern
          [f + num_captures_added_in_expansion]
        False -> {
          case f <= recursion_delim_capture_num + num_captures_in_left {
            True -> {
              // Within the recursed subpattern, on the left of the recursion token
              list.range(0, reps)
              |> list.map(fn(i) { f + num_captures_in_left * i })
            }
            False -> {
              // Within the recursed subpattern, on the right of the recursion token
              list.range(0, reps)
              |> list.map(fn(i) {
                f + num_captures_added_in_left + num_captures_in_right * i
              })
            }
          }
        }
      }
    }
  }
}
