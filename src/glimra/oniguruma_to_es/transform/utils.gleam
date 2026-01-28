/// Utility functions for the transform module.
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import glimra/oniguruma_parser/parser/ast_types.{
  type CapturingGroupNode, type FlagGroupModifiers, type FlagGroupSwitches,
  FlagGroupModifiers, FlagGroupSwitches,
}

// ============================================================================
// JS Group Name Validation
// ============================================================================

/// Check if a name is a valid JavaScript group name
/// JS group names follow identifier rules
pub fn is_valid_js_group_name(name: String) -> Bool {
  // JS group names must start with $, _, or a Unicode ID_Start character
  // and continue with $, \u200C, \u200D, or Unicode ID_Continue characters
  // For simplicity, we check for basic ASCII identifier rules plus some common cases
  case string.length(name) {
    0 -> False
    _ -> {
      let chars = string.to_graphemes(name)
      case chars {
        [] -> False
        [first, ..rest] -> {
          is_valid_js_id_start(first) && list.all(rest, is_valid_js_id_continue)
        }
      }
    }
  }
}

/// Check if a character can start a JS identifier
fn is_valid_js_id_start(char: String) -> Bool {
  case char {
    "$" | "_" -> True
    _ -> {
      // Check if it's a letter (simplified ASCII check)
      case string.to_utf_codepoints(char) {
        [cp] -> {
          let code = string.utf_codepoint_to_int(cp)
          // Basic Latin letters
          { code >= 65 && code <= 90 }
          // A-Z
          || { code >= 97 && code <= 122 }
          // a-z
          // Unicode letters (simplified - accept most non-ASCII)
          || code > 127
        }
        _ -> False
      }
    }
  }
}

/// Check if a character can continue a JS identifier
fn is_valid_js_id_continue(char: String) -> Bool {
  case char {
    "$" | "_" -> True
    _ -> {
      case string.to_utf_codepoints(char) {
        [cp] -> {
          let code = string.utf_codepoint_to_int(cp)
          // Basic Latin letters
          { code >= 65 && code <= 90 }
          // A-Z
          || { code >= 97 && code <= 122 }
          // a-z
          // Digits
          || { code >= 48 && code <= 57 }
          // 0-9
          // Zero-width non-joiner and zero-width joiner
          || code == 0x200C
          || code == 0x200D
          // Unicode (simplified)
          || code > 127
        }
        _ -> False
      }
    }
  }
}

/// Get or create a valid JS group name from an Oniguruma name
pub fn get_or_insert_group_name(
  name: String,
  map: Dict(String, String),
) -> #(String, Dict(String, String)) {
  case dict.get(map, name) {
    Ok(js_name) -> #(js_name, map)
    Error(_) -> {
      // Onig group names can't start with $, but JS names can
      // Create a safe name by prefixing with $ and index
      let index = dict.size(map)
      let safe_name = sanitize_group_name(name)
      let js_name = "$" <> int.to_string(index) <> "_" <> safe_name
      let new_map = dict.insert(map, name, js_name)
      #(js_name, new_map)
    }
  }
}

/// Sanitize a group name for JS use
fn sanitize_group_name(name: String) -> String {
  name
  |> string.to_graphemes
  |> list.map(fn(char) {
    case is_valid_js_id_continue(char) {
      True -> char
      False -> "_"
    }
  })
  |> string.concat
}

// ============================================================================
// Flag Helpers
// ============================================================================

/// Current flags as a tuple (dot_all, ignore_case)
pub type CurrentFlags =
  #(Bool, Bool)

/// Get new current flags after applying flag modifiers
pub fn get_new_current_flags(
  current: CurrentFlags,
  modifiers: FlagGroupModifiers,
) -> CurrentFlags {
  let #(dot_all, ignore_case) = current

  let new_dot_all = case modifiers.enable {
    Some(FlagGroupSwitches(dot_all: Some(True), ..)) -> True
    _ ->
      case modifiers.disable {
        Some(FlagGroupSwitches(dot_all: Some(True), ..)) -> False
        _ -> dot_all
      }
  }

  let new_ignore_case = case modifiers.enable {
    Some(FlagGroupSwitches(ignore_case: Some(True), ..)) -> True
    _ ->
      case modifiers.disable {
        Some(FlagGroupSwitches(ignore_case: Some(True), ..)) -> False
        _ -> ignore_case
      }
  }

  #(new_dot_all, new_ignore_case)
}

/// Check if two flag states are equal
pub fn are_flags_equal(a: CurrentFlags, b: CurrentFlags) -> Bool {
  a.0 == b.0 && a.1 == b.1
}

/// Get flag modifiers from flag state
pub fn get_flag_mods_from_flags(flags: CurrentFlags) -> FlagGroupModifiers {
  let #(dot_all, ignore_case) = flags

  let enable = case dot_all || ignore_case {
    True ->
      Some(FlagGroupSwitches(
        dot_all: case dot_all {
          True -> Some(True)
          False -> None
        },
        ignore_case: case ignore_case {
          True -> Some(True)
          False -> None
        },
        extended: None,
      ))
    False -> None
  }

  let disable = case !dot_all || !ignore_case {
    True ->
      Some(FlagGroupSwitches(
        dot_all: case dot_all {
          False -> Some(True)
          True -> None
        },
        ignore_case: case ignore_case {
          False -> Some(True)
          True -> None
        },
        extended: None,
      ))
    False -> None
  }

  FlagGroupModifiers(enable: enable, disable: disable)
}

// ============================================================================
// Node Cloning
// ============================================================================

/// Clone a capturing group node and update the origin map
pub fn clone_capturing_group(
  node: CapturingGroupNode,
  origin_map: Dict(CapturingGroupNode, CapturingGroupNode),
) -> #(CapturingGroupNode, Dict(CapturingGroupNode, CapturingGroupNode)) {
  // Create a copy of the node
  let copy =
    ast_types.CapturingGroupNode(
      number: node.number,
      name: node.name,
      is_subroutined: node.is_subroutined,
      body: node.body,
    )

  // Update the origin map - copy points to original (or original's origin)
  let origin = case dict.get(origin_map, node) {
    Ok(orig) -> orig
    Error(_) -> node
  }
  let new_map = dict.insert(origin_map, copy, origin)

  #(copy, new_map)
}

// ============================================================================
// Participation Check
// ============================================================================

/// Check if a capture can participate with a node
/// (i.e., the capture is in the same alternation path)
pub fn can_participate_with_node(
  capture: CapturingGroupNode,
  node_number: Int,
) -> Bool {
  // Simplified check: capture number must be less than the referencing node's context
  // A full implementation would walk the tree
  capture.number < node_number || capture.number == node_number
}
