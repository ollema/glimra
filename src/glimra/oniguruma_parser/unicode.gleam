//// Unicode property map for Oniguruma regex patterns.
//// Contains all Unicode properties supported by Oniguruma 6.9.10.

import gleam/dict.{type Dict}
import gleam/list
import gleam/string

/// Type alias for the Unicode property map
pub type UnicodePropertyMap =
  Dict(String, String)

/// POSIX class names
const posix_class_names: List(String) = [
  "alnum", "alpha", "ascii", "blank", "cntrl", "digit", "graph", "lower",
  "print", "punct", "space", "upper", "word", "xdigit",
]

/// Check if a name is a POSIX class name
pub fn is_posix_class_name(name: String) -> Bool {
  list.contains(posix_class_names, name)
}

/// Generate a Unicode property lookup name: lowercase, without spaces, hyphens, or underscores
pub fn slug(name: String) -> String {
  name
  |> string.replace("-", "")
  |> string.replace("_", "")
  |> string.replace(" ", "")
  |> string.lowercase
}

/// Normalize a Unicode property name (best-effort formatting to official values)
pub fn normalize_property_name(name: String) -> String {
  name
  |> string.trim
  |> string.replace(each: "-", with: "_")
  |> string.replace(each: " ", with: "_")
  |> normalize_case
}

/// Normalize the case of a property name
fn normalize_case(name: String) -> String {
  // Simple normalization: capitalize first letter of each segment
  name
  |> string.split("_")
  |> list.map(capitalize_segment)
  |> string.join("_")
}

fn capitalize_segment(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> string.lowercase(rest)
    Error(_) -> s
  }
}
