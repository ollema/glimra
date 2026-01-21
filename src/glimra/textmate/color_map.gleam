//// Color Map for theme color ID management
////
//// This module provides bidirectional mapping between color strings and integer IDs.
//// Color IDs are used in the binary token encoding for efficient storage and lookup.

import gleam/dict.{type Dict}
import gleam/string

/// Color map providing bidirectional color string ↔ ID mapping
pub type ColorMap {
  ColorMap(
    /// Map from color ID to color string
    id_to_color: Dict(Int, String),
    /// Map from color string (uppercase) to ID
    color_to_id: Dict(String, Int),
    /// Next available ID
    next_id: Int,
  )
}

/// Create a new empty color map
/// ID 0 is reserved for "no color" / null
pub fn new() -> ColorMap {
  ColorMap(id_to_color: dict.new(), color_to_id: dict.new(), next_id: 1)
}

/// Get or create an ID for a color string
/// Returns the updated color map and the ID
/// Passing an empty string or None-like value returns ID 0
pub fn get_id(map: ColorMap, color: String) -> #(ColorMap, Int) {
  case string.is_empty(color) {
    True -> #(map, 0)
    False -> {
      let upper_color = string.uppercase(color)
      case dict.get(map.color_to_id, upper_color) {
        Ok(id) -> #(map, id)
        Error(Nil) -> {
          let id = map.next_id
          let new_map =
            ColorMap(
              id_to_color: dict.insert(map.id_to_color, id, upper_color),
              color_to_id: dict.insert(map.color_to_id, upper_color, id),
              next_id: id + 1,
            )
          #(new_map, id)
        }
      }
    }
  }
}

/// Get the color string for an ID
/// Returns empty string if ID not found or ID is 0
pub fn get_color(map: ColorMap, id: Int) -> String {
  case id {
    0 -> ""
    _ ->
      case dict.get(map.id_to_color, id) {
        Ok(color) -> color
        Error(Nil) -> ""
      }
  }
}
