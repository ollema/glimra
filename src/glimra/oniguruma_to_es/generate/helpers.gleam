//// Helper functions for the generate module.
//// Includes character escaping and quantifier string generation.

import gleam/int
import gleam/option.{type Option}
import gleam/set.{type Set}
import gleam/string
import glimra/oniguruma_parser/parser/ast_types.{
  type CharacterClassElement, type FlagGroupModifiers, type QuantifierKind,
  CharacterCCE, CharacterNode, Greedy, Lazy, Possessive, quantifier_max_infinity,
}

// ============================================================================
// Character Escape Maps
// ============================================================================

/// Get character escape for special character codes
pub fn char_code_escape(code_point: Int) -> Option(String) {
  case code_point {
    9 -> option.Some("\\t")
    // horizontal tab
    10 -> option.Some("\\n")
    // line feed
    11 -> option.Some("\\v")
    // vertical tab
    12 -> option.Some("\\f")
    // form feed
    13 -> option.Some("\\r")
    // carriage return
    0x2028 -> option.Some("\\u2028")
    // line separator
    0x2029 -> option.Some("\\u2029")
    // paragraph separator
    0xFEFF -> option.Some("\\uFEFF")
    // ZWNBSP/BOM
    _ -> option.None
  }
}

/// Base escape characters for regex patterns (outside char class)
fn base_escape_chars() -> Set(String) {
  set.from_list([
    "$", "(", ")", "*", "+", ".", "?", "[", "\\", "]", "^", "{", "|", "}",
  ])
}

/// Escape characters for character classes (without flag v)
fn char_class_escape_chars() -> Set(String) {
  set.from_list([
    "-", "\\", "]", "^",
    // Literal `[` doesn't require escaping with flag u, but helps work around
    // regex source linters that expect unescaped `[` to create a nested class
    "[",
  ])
}

/// Escape characters for character classes with flag v (ES2024+)
fn char_class_escape_chars_flag_v() -> Set(String) {
  set.from_list([
    "(", ")", "-", "/", "[", "\\", "]", "^", "{", "|", "}",
    // Double punctuators; also includes already-listed `-` and `^`
    "!", "#", "$", "%", "&", "*", "+", ",", ".", ":", ";", "<", "=", ">", "?",
    "@", "`", "~",
  ])
}

// ============================================================================
// Character Escaping
// ============================================================================

/// Check if a code point is a digit (48-57, which are '0'-'9')
pub fn is_digit_char_code(code_point: Int) -> Bool {
  code_point > 47 && code_point < 58
}

/// Convert a code point to a character string
pub fn code_point_to_string(code_point: Int) -> String {
  let assert Ok(char) = string.utf_codepoint(code_point)
  string.from_utf_codepoints([char])
}

/// Escape a character for regex pattern
pub fn get_char_escape(
  code_point: Int,
  esc_digit: Bool,
  in_char_class: Bool,
  use_flag_v: Bool,
) -> String {
  // Check for special character codes first
  case char_code_escape(code_point) {
    option.Some(escaped) -> escaped
    option.None -> {
      // Check if we need hex/unicode escape
      case needs_hex_escape(code_point, esc_digit) {
        True -> format_code_point_escape(code_point)
        False -> {
          let char = code_point_to_string(code_point)
          let escape_chars = get_escape_chars(in_char_class, use_flag_v)
          case set.contains(escape_chars, char) {
            True -> "\\" <> char
            False -> char
          }
        }
      }
    }
  }
}

/// Check if a code point needs hex/unicode escape
fn needs_hex_escape(code_point: Int, esc_digit: Bool) -> Bool {
  // Control chars, etc.; condition modeled on Chrome console display
  code_point < 32
  || { code_point > 126 && code_point < 160 }
  // Unicode planes 4-16; unassigned, special purpose, private use
  || code_point > 0x3FFFF
  // Avoid corrupting a preceding backref by immediately following with digit
  || { esc_digit && is_digit_char_code(code_point) }
}

/// Format a code point as \xHH or \u{HHHH}
fn format_code_point_escape(code_point: Int) -> String {
  case code_point > 0xFF {
    True -> "\\u{" <> int_to_hex_upper(code_point) <> "}"
    False -> "\\x" <> int_to_hex_upper_padded(code_point, 2)
  }
}

/// Get the appropriate escape character set
fn get_escape_chars(in_char_class: Bool, use_flag_v: Bool) -> Set(String) {
  case in_char_class {
    True ->
      case use_flag_v {
        True -> char_class_escape_chars_flag_v()
        False -> char_class_escape_chars()
      }
    False -> base_escape_chars()
  }
}

/// Convert int to uppercase hex string
fn int_to_hex_upper(n: Int) -> String {
  int.to_base16(n)
}

/// Convert int to uppercase hex string, padded to length
fn int_to_hex_upper_padded(n: Int, len: Int) -> String {
  let hex = int.to_base16(n)
  string.pad_start(hex, len, "0")
}

// ============================================================================
// Quantifier Strings
// ============================================================================

/// Generate quantifier string (?, *, +, {n}, {n,m})
pub fn get_quantifier_str(kind: QuantifierKind, min: Int, max: Int) -> String {
  let base = case min, max {
    0, 1 -> "?"
    0, m if m == quantifier_max_infinity -> "*"
    1, m if m == quantifier_max_infinity -> "+"
    n, m if n == m -> "{" <> int.to_string(n) <> "}"
    n, m if m == quantifier_max_infinity -> "{" <> int.to_string(n) <> ",}"
    n, m -> "{" <> int.to_string(n) <> "," <> int.to_string(m) <> "}"
  }
  let suffix = case kind {
    Greedy -> ""
    Lazy -> "?"
    Possessive -> "+"
  }
  base <> suffix
}

// ============================================================================
// Group Prefix
// ============================================================================

/// Generate group prefix (:, >, i:, -i:, etc.)
pub fn get_group_prefix(
  atomic: Bool,
  flags: Option(FlagGroupModifiers),
  use_flag_mods: Bool,
) -> String {
  case atomic {
    True -> ">"
    False -> {
      let mods = case flags, use_flag_mods {
        option.Some(f), True -> format_flag_mods(f)
        _, _ -> ""
      }
      mods <> ":"
    }
  }
}

/// Format flag modifiers for group prefix
fn format_flag_mods(flags: FlagGroupModifiers) -> String {
  let enable_part = case flags.enable {
    option.Some(e) -> {
      let i = case e.ignore_case {
        option.Some(True) -> "i"
        _ -> ""
      }
      let s = case e.dot_all {
        option.Some(True) -> "s"
        _ -> ""
      }
      i <> s
    }
    option.None -> ""
  }
  let disable_part = case flags.disable {
    option.Some(d) -> {
      let has_disable =
        d.ignore_case == option.Some(True) || d.dot_all == option.Some(True)
      case has_disable {
        True -> {
          let i = case d.ignore_case {
            option.Some(True) -> "i"
            _ -> ""
          }
          let s = case d.dot_all {
            option.Some(True) -> "s"
            _ -> ""
          }
          "-" <> i <> s
        }
        False -> ""
      }
    }
    option.None -> ""
  }
  enable_part <> disable_part
}

// ============================================================================
// Character Class Helpers
// ============================================================================

/// Check if element is a literal hyphen (value 45)
pub fn is_literal_hyphen(element: CharacterClassElement) -> Bool {
  case element {
    CharacterCCE(CharacterNode(value: 45)) -> True
    _ -> False
  }
}
