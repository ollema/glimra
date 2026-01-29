//// Pure Gleam implementation of vscode-textmate's RegExpSource handling
////
//// This module handles regex pattern processing for TextMate grammars:
//// - Detecting and resolving anchors (\A, \G, \z)
//// - Detecting and resolving back-references (\1, \2, etc.)
//// - Escaping regex metacharacters
////
//// TextMate grammars use Oniguruma regex syntax with special anchors:
//// - \A: Start of string (not line)
//// - \G: Current match position
//// - \z: End of string (not line)

import gleam/list
import gleam/string
import glimra/textmate/rule.{type CaptureIndex, CaptureIndex}

/// Flags indicating which anchors are present in a pattern
pub type AnchorFlags {
  AnchorFlags(
    /// Pattern contains \A (start of string)
    has_anchor_a: Bool,
    /// Pattern contains \G (current position)
    has_anchor_g: Bool,
    /// Pattern contains \z or \Z (end of string)
    has_anchor_z: Bool,
  )
}

/// No anchors present
const no_anchors = AnchorFlags(
  has_anchor_a: False,
  has_anchor_g: False,
  has_anchor_z: False,
)

/// Check if any anchor is present
pub fn has_any_anchor(flags: AnchorFlags) -> Bool {
  flags.has_anchor_a || flags.has_anchor_g || flags.has_anchor_z
}

/// Detect anchors in a regex pattern.
/// Scans the pattern for \A, \G, \z, and \Z sequences.
pub fn detect_anchors(pattern: String) -> AnchorFlags {
  detect_anchors_impl(pattern, no_anchors)
}

fn detect_anchors_impl(remaining: String, flags: AnchorFlags) -> AnchorFlags {
  case string.pop_grapheme(remaining) {
    Error(Nil) -> flags
    Ok(#("\\", rest)) ->
      case string.pop_grapheme(rest) {
        Error(Nil) -> flags
        Ok(#("A", rest2)) ->
          detect_anchors_impl(rest2, AnchorFlags(..flags, has_anchor_a: True))
        Ok(#("G", rest2)) ->
          detect_anchors_impl(rest2, AnchorFlags(..flags, has_anchor_g: True))
        Ok(#("z", rest2)) | Ok(#("Z", rest2)) ->
          detect_anchors_impl(rest2, AnchorFlags(..flags, has_anchor_z: True))
        Ok(#(_, rest2)) -> detect_anchors_impl(rest2, flags)
      }
    Ok(#(_, rest)) -> detect_anchors_impl(rest, flags)
  }
}

/// Check if a pattern contains back-references (\1, \2, etc.)
pub fn has_back_references(pattern: String) -> Bool {
  has_back_references_impl(pattern)
}

fn has_back_references_impl(remaining: String) -> Bool {
  case string.pop_grapheme(remaining) {
    Error(Nil) -> False
    Ok(#("\\", rest)) ->
      case string.pop_grapheme(rest) {
        Error(Nil) -> False
        Ok(#(digit, rest2)) ->
          case is_digit(digit) {
            True -> True
            False -> has_back_references_impl(rest2)
          }
      }
    Ok(#(_, rest)) -> has_back_references_impl(rest)
  }
}

/// Check if a character is a digit (0-9)
fn is_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

/// Parse a digit character to an integer
fn digit_to_int(char: String) -> Int {
  case char {
    "0" -> 0
    "1" -> 1
    "2" -> 2
    "3" -> 3
    "4" -> 4
    "5" -> 5
    "6" -> 6
    "7" -> 7
    "8" -> 8
    "9" -> 9
    _ -> 0
  }
}

/// Get element at index from a list (0-indexed)
fn list_at(lst: List(a), index: Int) -> Result(a, Nil) {
  lst
  |> list.drop(index)
  |> list.first
}

/// Resolve back-references in a pattern by substituting captured text.
///
/// For example, if the pattern is `\1\2` and captures are ["foo", "bar"],
/// the result will be `foo bar` (properly escaped for regex).
pub fn resolve_back_references(
  pattern: String,
  line_text: String,
  capture_indices: List(CaptureIndex),
) -> String {
  let captured_values =
    list.map(capture_indices, fn(capture) {
      string.slice(line_text, capture.start, capture.end - capture.start)
    })
  resolve_back_refs_impl(pattern, captured_values, "")
}

fn resolve_back_refs_impl(
  remaining: String,
  captured_values: List(String),
  acc: String,
) -> String {
  case string.pop_grapheme(remaining) {
    Error(Nil) -> acc
    Ok(#("\\", rest)) ->
      case string.pop_grapheme(rest) {
        Error(Nil) -> acc <> "\\"
        Ok(#(digit, rest2)) ->
          case is_digit(digit) {
            True -> {
              // Parse the full number (could be multi-digit like \10)
              let #(num, rest3) = parse_back_ref_number(digit, rest2)
              let replacement = case list_at(captured_values, num) {
                Ok(value) -> escape_regexp_characters(value)
                Error(Nil) -> ""
              }
              resolve_back_refs_impl(rest3, captured_values, acc <> replacement)
            }
            False ->
              resolve_back_refs_impl(
                rest2,
                captured_values,
                acc <> "\\" <> digit,
              )
          }
      }
    Ok(#(char, rest)) ->
      resolve_back_refs_impl(rest, captured_values, acc <> char)
  }
}

/// Parse a back-reference number (potentially multi-digit)
fn parse_back_ref_number(
  first_digit: String,
  remaining: String,
) -> #(Int, String) {
  parse_back_ref_number_impl(remaining, digit_to_int(first_digit))
}

fn parse_back_ref_number_impl(remaining: String, acc: Int) -> #(Int, String) {
  case string.pop_grapheme(remaining) {
    Error(Nil) -> #(acc, "")
    Ok(#(char, rest)) ->
      case is_digit(char) {
        True -> parse_back_ref_number_impl(rest, acc * 10 + digit_to_int(char))
        False -> #(acc, remaining)
      }
  }
}

/// Escape special regex characters in a string.
/// This is used when substituting captured text into patterns.
fn escape_regexp_characters(text: String) -> String {
  escape_regexp_impl(text, "")
}

fn escape_regexp_impl(remaining: String, acc: String) -> String {
  case string.pop_grapheme(remaining) {
    Error(Nil) -> acc
    Ok(#(char, rest)) -> {
      let escaped = case char {
        "-"
        | "\\"
        | "{"
        | "}"
        | "*"
        | "+"
        | "?"
        | "|"
        | "^"
        | "$"
        | "."
        | "["
        | "]"
        | "("
        | ")"
        | "/" -> "\\" <> char
        _ -> char
      }
      escape_regexp_impl(rest, acc <> escaped)
    }
  }
}

/// Resolve anchors in a pattern based on matching context.
///
/// vscode-textmate caches 4 versions of each pattern with anchors:
/// - A0_G0: Neither \A nor \G should match
/// - A0_G1: \G should match (at current position)
/// - A1_G0: \A should match (at string start)
/// - A1_G1: Both should match
///
/// This function returns the appropriate pattern variant.
pub fn resolve_anchors(pattern: String, allow_a: Bool, allow_g: Bool) -> String {
  resolve_anchors_impl(pattern, allow_a, allow_g, "")
}

fn resolve_anchors_impl(
  remaining: String,
  allow_a: Bool,
  allow_g: Bool,
  acc: String,
) -> String {
  case string.pop_grapheme(remaining) {
    Error(Nil) -> acc
    Ok(#("\\", rest)) ->
      case string.pop_grapheme(rest) {
        Error(Nil) -> acc <> "\\"
        Ok(#("A", rest2)) -> {
          // \A - start of string anchor
          let replacement = case allow_a {
            True -> "\\A"
            // Replace with pattern that won't match
            False -> "\\uFFFF"
          }
          resolve_anchors_impl(rest2, allow_a, allow_g, acc <> replacement)
        }
        Ok(#("G", rest2)) -> {
          // \G - current position anchor
          let replacement = case allow_g {
            True -> "\\G"
            // Replace with pattern that won't match
            False -> "\\uFFFF"
          }
          resolve_anchors_impl(rest2, allow_a, allow_g, acc <> replacement)
        }
        Ok(#(char, rest2)) ->
          resolve_anchors_impl(rest2, allow_a, allow_g, acc <> "\\" <> char)
      }
    Ok(#(char, rest)) ->
      resolve_anchors_impl(rest, allow_a, allow_g, acc <> char)
  }
}

/// Resolve capture references ($1, $2, etc.) in a scope name.
///
/// TextMate scope names can contain references to captured text:
/// - $1, $2, etc. for simple substitution
/// - ${1:/downcase}, ${1:/upcase} for case transformation
pub fn resolve_capture_refs(
  name: String,
  line_text: String,
  capture_indices: List(CaptureIndex),
) -> String {
  // Fast path: if there's no $ in the name, no capture references exist
  case string.contains(name, "$") {
    False -> name
    True -> resolve_capture_refs_impl(name, line_text, capture_indices, "")
  }
}

fn resolve_capture_refs_impl(
  remaining: String,
  line_text: String,
  capture_indices: List(CaptureIndex),
  acc: String,
) -> String {
  case string.pop_grapheme(remaining) {
    Error(Nil) -> acc
    Ok(#("$", rest)) ->
      case string.pop_grapheme(rest) {
        Error(Nil) -> acc <> "$"
        // ${N:/transform} syntax
        Ok(#("{", rest2)) -> {
          let #(result, rest3) =
            parse_capture_transform(rest2, line_text, capture_indices)
          resolve_capture_refs_impl(
            rest3,
            line_text,
            capture_indices,
            acc <> result,
          )
        }
        // $N syntax
        Ok(#(digit, rest2)) ->
          case is_digit(digit) {
            True -> {
              let #(num, rest3) = parse_back_ref_number(digit, rest2)
              let replacement =
                get_capture_text(num, line_text, capture_indices)
              resolve_capture_refs_impl(
                rest3,
                line_text,
                capture_indices,
                acc <> replacement,
              )
            }
            False ->
              resolve_capture_refs_impl(
                rest,
                line_text,
                capture_indices,
                acc <> "$",
              )
          }
      }
    Ok(#(char, rest)) ->
      resolve_capture_refs_impl(rest, line_text, capture_indices, acc <> char)
  }
}

/// Parse ${N:/transform} syntax and apply transformation
fn parse_capture_transform(
  remaining: String,
  line_text: String,
  capture_indices: List(CaptureIndex),
) -> #(String, String) {
  // Parse the number
  case string.pop_grapheme(remaining) {
    Error(Nil) -> #("${", "")
    Ok(#(digit, rest)) ->
      case is_digit(digit) {
        False -> #("${", remaining)
        True -> {
          let #(num, rest2) = parse_back_ref_number(digit, rest)
          let text = get_capture_text(num, line_text, capture_indices)
          // Look for :/transform}
          case string.split_once(rest2, "}") {
            Error(Nil) -> #(text, rest2)
            Ok(#(transform_part, after_brace)) -> {
              let transformed = case string.starts_with(transform_part, ":/") {
                False -> text
                True -> {
                  let transform = string.drop_start(transform_part, 2)
                  apply_transform(text, transform)
                }
              }
              #(transformed, after_brace)
            }
          }
        }
      }
  }
}

/// Apply a case transformation to text
fn apply_transform(text: String, transform: String) -> String {
  case transform {
    "downcase" -> string.lowercase(text)
    "upcase" -> string.uppercase(text)
    _ -> text
  }
}

/// Get captured text by index
fn get_capture_text(
  index: Int,
  line_text: String,
  capture_indices: List(CaptureIndex),
) -> String {
  case list_at(capture_indices, index) {
    Ok(CaptureIndex(start, end)) -> string.slice(line_text, start, end - start)
    Error(Nil) -> ""
  }
}
