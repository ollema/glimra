//// Unicode case expansion utilities for the generate module.
////
//// Note: These functions are only needed when targeting pre-ES2025 and
//// when there's mixed case sensitivity usage. Since we target ES2025,
//// these are largely unused in practice but are included for completeness.

import gleam/dict.{type Dict}
import gleam/list
import gleam/set.{type Set}
import gleam/string

// ============================================================================
// Unicode Properties with Specific Case
// ============================================================================

/// Unicode properties that have specific case
/// These properties shouldn't be used case-insensitively when other chars
/// have specific case.
pub fn properties_with_specific_case() -> Set(String) {
  set.from_list([
    "Lower",
    "Lowercase",
    "Upper",
    "Uppercase",
    "Ll",
    "Lowercase_Letter",
    "Lt",
    "Titlecase_Letter",
    "Lu",
    "Uppercase_Letter",
  ])
}

// ============================================================================
// Case Detection
// ============================================================================

/// Characters that should NOT match the chars they case swap to
fn chars_without_ignore_case_expansion() -> Set(Int) {
  set.from_list([
    0x130,
    // İ (Latin Capital Letter I With Dot Above)
    0x131,
    // ı (Latin Small Letter Dotless I)
  ])
}

/// Check if a character has case (Unicode Cased property)
/// This is a simplified implementation - full Unicode support would require
/// importing Unicode data tables.
pub fn char_has_case(char: String) -> Bool {
  // Simple check: if lowercase != uppercase, it has case
  let lower = string.lowercase(char)
  let upper = string.uppercase(char)
  lower != upper
}

// ============================================================================
// Case Expansion
// ============================================================================

/// Get all case variants for case-insensitive matching
pub fn get_ignore_case_match_chars(char: String) -> List(String) {
  let code_point = case string.to_utf_codepoints(char) {
    [cp] -> string.utf_codepoint_to_int(cp)
    _ -> 0
  }

  // Some chars should not match the chars they case swap to
  case set.contains(chars_without_ignore_case_expansion(), code_point) {
    True -> [char]
    False -> {
      let lower = string.lowercase(char)
      let upper = string.uppercase(lower)

      // Build set of unique variants
      let variants = case string.length(upper) == 1 {
        True -> [upper, lower]
        False -> [lower]
        // Multi-char uppercase (e.g., German ß -> SS)
      }

      // Add alternative cases if any
      let variants = case
        dict.get(lower_to_alternative_lower_case_map(), lower)
      {
        Ok(alt) -> list.append(variants, [alt])
        Error(_) -> variants
      }

      let variants = case
        dict.get(lower_to_alternative_upper_case_map(), lower)
      {
        Ok(alt) -> list.append(variants, [alt])
        Error(_) -> variants
      }

      let variants = case dict.get(lower_to_title_case_map(), lower) {
        Ok(alt) -> list.append(variants, [alt])
        Error(_) -> variants
      }

      // Return unique variants
      variants |> list.unique
    }
  }
}

/// Get case variants outside a character class range
/// Used for expanding ranges like A-Z to include case variants
pub fn get_cases_outside_range(
  min: Int,
  max: Int,
  first_only: Bool,
) -> List(String) {
  // Optimization: skip ranges that can't have case variants outside
  // - No case variants cross the Basic Multilingual Plane boundary
  // - No cased chars appear beyond the Supplementary Multilingual Plane
  case
    { min < 65 && { max == 0xFFFF || max >= 0x1FFFF } }
    || { min == 0x10000 && max >= 0x1FFFF }
  {
    True -> []
    False -> get_cases_outside_range_loop(min, max, first_only, [])
  }
}

fn get_cases_outside_range_loop(
  current: Int,
  max: Int,
  first_only: Bool,
  found: List(String),
) -> List(String) {
  case current > max {
    True -> found
    False -> {
      let char = code_point_to_string(current)
      case char_has_case(char) {
        False ->
          get_cases_outside_range_loop(current + 1, max, first_only, found)
        True -> {
          let variants = get_ignore_case_match_chars(char)
          let outside =
            variants
            |> list.filter(fn(v) {
              let cp = string_to_code_point(v)
              cp < current || cp > max
            })
          case list.is_empty(outside) {
            True ->
              get_cases_outside_range_loop(current + 1, max, first_only, found)
            False ->
              case first_only {
                True -> outside
                False ->
                  get_cases_outside_range_loop(
                    current + 1,
                    max,
                    first_only,
                    list.append(found, outside),
                  )
              }
          }
        }
      }
    }
  }
}

fn code_point_to_string(code_point: Int) -> String {
  let assert Ok(cp) = string.utf_codepoint(code_point)
  string.from_utf_codepoints([cp])
}

fn string_to_code_point(s: String) -> Int {
  case string.to_utf_codepoints(s) {
    [cp] -> string.utf_codepoint_to_int(cp)
    _ -> 0
  }
}

// ============================================================================
// Case Maps
// ============================================================================

/// Alternative lowercase mappings
fn lower_to_alternative_lower_case_map() -> Dict(String, String) {
  dict.from_list([
    #("s", code_point_to_string(0x17F)),
    // s -> ſ (long s)
    #(code_point_to_string(0x17F), "s"),
    // ſ -> s
  ])
}

/// Alternative uppercase mappings
fn lower_to_alternative_upper_case_map() -> Dict(String, String) {
  dict.from_list([
    #(code_point_to_string(0xDF), code_point_to_string(0x1E9E)),
    // ß -> ẞ
    #("k", code_point_to_string(0x212A)),
    // k -> K (Kelvin)
    #(code_point_to_string(0xE5), code_point_to_string(0x212B)),
    // å -> Å (Angstrom)
    #(code_point_to_string(0x3C9), code_point_to_string(0x2126)),
    // ω -> Ω (Ohm)
  ])
}

/// Titlecase mappings
fn lower_to_title_case_map() -> Dict(String, String) {
  let entries =
    [
      title_entry(0x1C5),
      title_entry(0x1C8),
      title_entry(0x1CB),
      title_entry(0x1F2),
    ]
    |> list.append(title_range(0x1F88, 0x1F8F))
    |> list.append(title_range(0x1F98, 0x1F9F))
    |> list.append(title_range(0x1FA8, 0x1FAF))
    |> list.append([
      title_entry(0x1FBC),
      title_entry(0x1FCC),
      title_entry(0x1FFC),
    ])

  dict.from_list(entries)
}

fn title_entry(code_point: Int) -> #(String, String) {
  let char = code_point_to_string(code_point)
  let lower = string.lowercase(char)
  #(lower, char)
}

fn title_range(start: Int, end: Int) -> List(#(String, String)) {
  range(start, end)
  |> list.map(title_entry)
}

fn range(start: Int, end: Int) -> List(Int) {
  range_loop(start, end, [])
}

fn range_loop(current: Int, end: Int, acc: List(Int)) -> List(Int) {
  case current > end {
    True -> list.reverse(acc)
    False -> range_loop(current + 1, end, [current, ..acc])
  }
}
