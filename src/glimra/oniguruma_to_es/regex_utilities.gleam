//// Utility functions for regex pattern string manipulation.
////
//// They scan through patterns character by character to find and replace tokens.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Context for matching - DEFAULT (outside char class) or CHAR_CLASS (inside)
pub type Context {
  Default
  CharClass
}

/// Match details passed to callbacks
pub type MatchDetails {
  MatchDetails(context: Context, negated: Bool)
}

/// A match result
pub type MatchResult {
  MatchResult(match: String, index: Int)
}

/// Replaces all unescaped instances of a literal needle in the given context.
/// This is a simplified version that handles literal string matching.
pub fn replace_unescaped_literal(
  expression: String,
  needle: String,
  replacement: fn(MatchResult, MatchDetails) -> String,
  context: Option(Context),
) -> String {
  do_replace_literal(
    expression,
    needle,
    replacement,
    context,
    0,
    0,
    [False],
    "",
  )
}

fn do_replace_literal(
  expression: String,
  needle: String,
  replacement: fn(MatchResult, MatchDetails) -> String,
  context: Option(Context),
  pos: Int,
  num_char_classes_open: Int,
  negated_stack: List(Bool),
  result: String,
) -> String {
  let remaining = string.drop_start(expression, pos)
  let needle_len = string.length(needle)

  case string.length(remaining) {
    0 -> result
    _ -> {
      // Check for escape sequence first
      case string.starts_with(remaining, "\\") {
        True -> {
          // Escaped character - skip both chars
          let escaped = string.slice(remaining, 0, 2)
          do_replace_literal(
            expression,
            needle,
            replacement,
            context,
            pos + 2,
            num_char_classes_open,
            negated_stack,
            result <> escaped,
          )
        }
        False -> {
          // Check for character class boundaries
          case string.starts_with(remaining, "[") {
            True -> {
              let is_negated =
                string.starts_with(string.drop_start(remaining, 1), "^")
              let skip_len = case is_negated {
                True -> 2
                False -> 1
              }
              let chars = string.slice(remaining, 0, skip_len)
              do_replace_literal(
                expression,
                needle,
                replacement,
                context,
                pos + skip_len,
                num_char_classes_open + 1,
                [is_negated, ..negated_stack],
                result <> chars,
              )
            }
            False -> {
              case
                string.starts_with(remaining, "]") && num_char_classes_open > 0
              {
                True -> {
                  let new_negated = case negated_stack {
                    [_, ..rest] -> rest
                    [] -> []
                  }
                  do_replace_literal(
                    expression,
                    needle,
                    replacement,
                    context,
                    pos + 1,
                    num_char_classes_open - 1,
                    new_negated,
                    result <> "]",
                  )
                }
                False -> {
                  // Check if needle matches here
                  let current_context = case num_char_classes_open > 0 {
                    True -> CharClass
                    False -> Default
                  }
                  let context_matches = case context {
                    None -> True
                    Some(ctx) -> ctx == current_context
                  }

                  case
                    context_matches && string.starts_with(remaining, needle)
                  {
                    True -> {
                      // Found a match
                      let current_negated = case negated_stack {
                        [n, ..] -> n
                        [] -> False
                      }
                      let details =
                        MatchDetails(
                          context: current_context,
                          negated: current_negated,
                        )
                      let match_result = MatchResult(match: needle, index: pos)
                      let replaced = replacement(match_result, details)
                      do_replace_literal(
                        expression,
                        needle,
                        replacement,
                        context,
                        pos + needle_len,
                        num_char_classes_open,
                        negated_stack,
                        result <> replaced,
                      )
                    }
                    False -> {
                      // No match - advance one character
                      let char = string.slice(remaining, 0, 1)
                      do_replace_literal(
                        expression,
                        needle,
                        replacement,
                        context,
                        pos + 1,
                        num_char_classes_open,
                        negated_stack,
                        result <> char,
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

/// Checks whether an unescaped instance of a literal needle appears in the given context.
pub fn has_unescaped_literal(
  expression: String,
  needle: String,
  context: Option(Context),
) -> Bool {
  do_has_literal(expression, needle, context, 0, 0)
}

fn do_has_literal(
  expression: String,
  needle: String,
  context: Option(Context),
  pos: Int,
  num_char_classes_open: Int,
) -> Bool {
  let remaining = string.drop_start(expression, pos)

  case string.length(remaining) {
    0 -> False
    _ -> {
      // Check for escape sequence first
      case string.starts_with(remaining, "\\") {
        True -> {
          // Skip escaped character
          do_has_literal(
            expression,
            needle,
            context,
            pos + 2,
            num_char_classes_open,
          )
        }
        False -> {
          // Track char class
          case string.starts_with(remaining, "[") {
            True -> {
              let skip = case
                string.starts_with(string.drop_start(remaining, 1), "^")
              {
                True -> 2
                False -> 1
              }
              do_has_literal(
                expression,
                needle,
                context,
                pos + skip,
                num_char_classes_open + 1,
              )
            }
            False -> {
              case
                string.starts_with(remaining, "]") && num_char_classes_open > 0
              {
                True ->
                  do_has_literal(
                    expression,
                    needle,
                    context,
                    pos + 1,
                    num_char_classes_open - 1,
                  )
                False -> {
                  let current_context = case num_char_classes_open > 0 {
                    True -> CharClass
                    False -> Default
                  }
                  let context_matches = case context {
                    None -> True
                    Some(ctx) -> ctx == current_context
                  }

                  case
                    context_matches && string.starts_with(remaining, needle)
                  {
                    True -> True
                    False ->
                      do_has_literal(
                        expression,
                        needle,
                        context,
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

/// Extracts the full contents of a group (subpattern) from the given expression.
/// The group is identified by the position where its contents start
/// (the string index just after the group's opening delimiter).
/// Returns the rest of the string if the group is unclosed.
pub fn get_group_contents(expression: String, contents_start_pos: Int) -> String {
  let after_start = string.drop_start(expression, contents_start_pos)
  let end_offset = find_group_end(after_start, 0, 1, 0)
  string.slice(after_start, 0, end_offset)
}

fn find_group_end(
  remaining: String,
  pos: Int,
  num_groups_open: Int,
  num_char_classes_open: Int,
) -> Int {
  case string.pop_grapheme(remaining) {
    Error(_) -> pos
    Ok(#(char, rest)) -> {
      // Check for escape
      case char == "\\" {
        True -> {
          // Skip escaped char
          case string.pop_grapheme(rest) {
            Error(_) -> pos + 1
            Ok(#(_, rest2)) ->
              find_group_end(
                rest2,
                pos + 2,
                num_groups_open,
                num_char_classes_open,
              )
          }
        }
        False -> {
          case char == "[" {
            True ->
              find_group_end(
                rest,
                pos + 1,
                num_groups_open,
                num_char_classes_open + 1,
              )
            False -> {
              case char == "]" && num_char_classes_open > 0 {
                True ->
                  find_group_end(
                    rest,
                    pos + 1,
                    num_groups_open,
                    num_char_classes_open - 1,
                  )
                False if num_char_classes_open > 0 ->
                  // Inside char class - skip
                  find_group_end(
                    rest,
                    pos + 1,
                    num_groups_open,
                    num_char_classes_open,
                  )
                False -> {
                  case char == "(" {
                    True ->
                      find_group_end(
                        rest,
                        pos + 1,
                        num_groups_open + 1,
                        num_char_classes_open,
                      )
                    False -> {
                      case char == ")" {
                        True -> {
                          case num_groups_open - 1 {
                            0 -> pos
                            n ->
                              find_group_end(
                                rest,
                                pos + 1,
                                n,
                                num_char_classes_open,
                              )
                          }
                        }
                        False ->
                          find_group_end(
                            rest,
                            pos + 1,
                            num_groups_open,
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
  }
}

/// Count the number of captures (named and unnamed) in a pattern.
/// Captures are: (?<name>...) for named, ( not followed by ? for unnamed
pub fn count_captures(pattern: String, context: Option(Context)) -> Int {
  do_count_captures(pattern, context, 0, 0, 0)
}

fn do_count_captures(
  remaining: String,
  context: Option(Context),
  pos: Int,
  count: Int,
  num_char_classes_open: Int,
) -> Int {
  case string.pop_grapheme(remaining) {
    Error(_) -> count
    Ok(#(char, rest)) -> {
      // Check for escape
      case char == "\\" {
        True -> {
          case string.pop_grapheme(rest) {
            Error(_) -> count
            Ok(#(_, rest2)) ->
              do_count_captures(
                rest2,
                context,
                pos + 2,
                count,
                num_char_classes_open,
              )
          }
        }
        False -> {
          // Track char class
          case char == "[" {
            True ->
              do_count_captures(
                rest,
                context,
                pos + 1,
                count,
                num_char_classes_open + 1,
              )
            False -> {
              case char == "]" && num_char_classes_open > 0 {
                True ->
                  do_count_captures(
                    rest,
                    context,
                    pos + 1,
                    count,
                    num_char_classes_open - 1,
                  )
                False if num_char_classes_open > 0 ->
                  // Inside char class - skip
                  do_count_captures(
                    rest,
                    context,
                    pos + 1,
                    count,
                    num_char_classes_open,
                  )
                False -> {
                  let current_context = case num_char_classes_open > 0 {
                    True -> CharClass
                    False -> Default
                  }
                  let context_matches = case context {
                    None -> True
                    Some(ctx) -> ctx == current_context
                  }

                  case char == "(" && context_matches {
                    True -> {
                      // Check if this is a capture
                      // Named capture: (?<name> where name doesn't start with = or !
                      // Unnamed capture: ( not followed by ?
                      case string.starts_with(rest, "?<") {
                        True -> {
                          // Check it's not (?<= or (?<!
                          let after_angle = string.drop_start(rest, 2)
                          case
                            string.starts_with(after_angle, "=")
                            || string.starts_with(after_angle, "!")
                          {
                            True ->
                              // Lookbehind, not a capture
                              do_count_captures(
                                rest,
                                context,
                                pos + 1,
                                count,
                                num_char_classes_open,
                              )
                            False ->
                              // Named capture
                              do_count_captures(
                                rest,
                                context,
                                pos + 1,
                                count + 1,
                                num_char_classes_open,
                              )
                          }
                        }
                        False -> {
                          case string.starts_with(rest, "?") {
                            True ->
                              // Non-capturing group
                              do_count_captures(
                                rest,
                                context,
                                pos + 1,
                                count,
                                num_char_classes_open,
                              )
                            False ->
                              // Unnamed capture
                              do_count_captures(
                                rest,
                                context,
                                pos + 1,
                                count + 1,
                                num_char_classes_open,
                              )
                          }
                        }
                      }
                    }
                    False ->
                      do_count_captures(
                        rest,
                        context,
                        pos + 1,
                        count,
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

/// Scan pattern for tokens, calling callback for each one.
/// Returns list of captured group names found.
pub fn collect_capture_names(pattern: String) -> List(String) {
  do_collect_names(pattern, 0, 0, [])
}

fn do_collect_names(
  remaining: String,
  pos: Int,
  num_char_classes_open: Int,
  names: List(String),
) -> List(String) {
  case string.pop_grapheme(remaining) {
    Error(_) -> list.reverse(names)
    Ok(#(char, rest)) -> {
      case char == "\\" {
        True -> {
          case string.pop_grapheme(rest) {
            Error(_) -> list.reverse(names)
            Ok(#(_, rest2)) ->
              do_collect_names(rest2, pos + 2, num_char_classes_open, names)
          }
        }
        False -> {
          case char == "[" {
            True ->
              do_collect_names(rest, pos + 1, num_char_classes_open + 1, names)
            False -> {
              case char == "]" && num_char_classes_open > 0 {
                True ->
                  do_collect_names(
                    rest,
                    pos + 1,
                    num_char_classes_open - 1,
                    names,
                  )
                False if num_char_classes_open > 0 ->
                  do_collect_names(rest, pos + 1, num_char_classes_open, names)
                False -> {
                  // Look for (?<name> pattern
                  case string.starts_with(remaining, "(?<") {
                    True -> {
                      let after = string.drop_start(remaining, 3)
                      // Make sure it's not (?<= or (?<!
                      case
                        string.starts_with(after, "=")
                        || string.starts_with(after, "!")
                      {
                        True ->
                          do_collect_names(
                            rest,
                            pos + 1,
                            num_char_classes_open,
                            names,
                          )
                        False -> {
                          // Extract the name (up to >)
                          case extract_until(after, ">") {
                            Some(name) -> {
                              let skip = 3 + string.length(name) + 1
                              do_collect_names(
                                string.drop_start(remaining, skip),
                                pos + skip,
                                num_char_classes_open,
                                [name, ..names],
                              )
                            }
                            None ->
                              do_collect_names(
                                rest,
                                pos + 1,
                                num_char_classes_open,
                                names,
                              )
                          }
                        }
                      }
                    }
                    False ->
                      do_collect_names(
                        rest,
                        pos + 1,
                        num_char_classes_open,
                        names,
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

/// Extract string up to (but not including) delimiter
fn extract_until(s: String, delim: String) -> Option(String) {
  do_extract_until(s, delim, "")
}

fn do_extract_until(s: String, delim: String, acc: String) -> Option(String) {
  case string.pop_grapheme(s) {
    Error(_) -> None
    Ok(#(char, rest)) -> {
      case char == delim {
        True -> Some(acc)
        False -> do_extract_until(rest, delim, acc <> char)
      }
    }
  }
}
