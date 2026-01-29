//// Utility functions for the transform module.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import glimra/oniguruma_parser/parser/ast_types.{
  type AlternativeElement, type AlternativeNode, type CapturingGroupNode,
  type CharacterClassElement, type FlagGroupModifiers, type QuantifiableNode,
  AbsenceFunctionE, AbsenceFunctionNode, AbsenceFunctionQ, AlternativeNode,
  AssertionE, BackreferenceE, BackreferenceQ, CapturingGroupE,
  CapturingGroupNode, CapturingGroupQ, CharacterCCE, CharacterClassCCE,
  CharacterClassE, CharacterClassNode, CharacterClassQ, CharacterClassRangeCCE,
  CharacterE, CharacterQ, CharacterSetCCE, CharacterSetE, CharacterSetQ,
  DirectiveE, FlagGroupModifiers, FlagGroupSwitches, GroupE, GroupNode, GroupQ,
  LookaroundAssertionE, LookaroundAssertionNode, NamedCalloutE, QuantifierE,
  QuantifierNode, QuantifierQ, SubroutineE, SubroutineQ,
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
// Node Cloning (Deep Clone)
// ============================================================================

/// State for cloning operations
pub type CloneState {
  CloneState(
    next_id: Int,
    /// Map from clone transform_id to origin transform_id (for recursion detection)
    id_relationships: Dict(Int, Int),
    /// List of (clone_node, origin_id) pairs for the origin_map
    /// clone_node is stored at cloning time (before transforms)
    /// origin_id is used to look up origin from ast_after_second (after second pass)
    origin_entries: List(#(CapturingGroupNode, Int)),
  )
}

/// Initialize a new clone state
pub fn new_clone_state() -> CloneState {
  CloneState(next_id: 1, id_relationships: dict.new(), origin_entries: [])
}

/// Initialize a clone state with a specific starting ID
/// Used to continue ID sequence from first pass
pub fn new_clone_state_from(starting_id: Int) -> CloneState {
  CloneState(
    next_id: starting_id,
    id_relationships: dict.new(),
    origin_entries: [],
  )
}

/// Deep clone a capturing group node with unique transform_ids
/// This is the top-level clone function called from transform_subroutine.
/// It sets is_subroutined: Some(True) and adds to origin_entries.
/// Returns: (cloned_node, updated_clone_state)
pub fn clone_capturing_group(
  node: CapturingGroupNode,
  clone_state: CloneState,
) -> #(CapturingGroupNode, CloneState) {
  clone_capturing_group_impl(node, clone_state, True)
}

/// Internal clone function with control over is_subroutined and origin_entries
fn clone_capturing_group_impl(
  node: CapturingGroupNode,
  clone_state: CloneState,
  is_top_level: Bool,
) -> #(CapturingGroupNode, CloneState) {
  // Assign ID to the origin if it doesn't have one
  let #(origin_node, state1, origin_id) = case node.transform_id {
    Some(id) -> #(node, clone_state, id)
    None -> {
      let new_id = clone_state.next_id
      let new_node = CapturingGroupNode(..node, transform_id: Some(new_id))
      let new_state =
        CloneState(..clone_state, next_id: clone_state.next_id + 1)
      #(new_node, new_state, new_id)
    }
  }

  // Reserve ID for the clone BEFORE cloning body
  // This ensures parent entry is added before nested entries
  let clone_id = state1.next_id
  let state_after_id = CloneState(..state1, next_id: state1.next_id + 1)

  // Only top-level clones get is_subroutined: Some(True)
  // Nested clones preserve the original is_subroutined value
  let clone_is_subroutined = case is_top_level {
    True -> Some(True)
    False -> origin_node.is_subroutined
  }

  // Create a placeholder node with the clone_id for origin_entries
  // This will be looked up by transform_id in transform.gleam
  let placeholder =
    CapturingGroupNode(
      number: origin_node.number,
      name: origin_node.name,
      is_subroutined: clone_is_subroutined,
      body: [],
      transform_id: Some(clone_id),
    )

  // Track the relationship: clone_id -> origin_id (for recursion detection)
  let new_relationships =
    dict.insert(state_after_id.id_relationships, clone_id, origin_id)

  // Add parent entry BEFORE cloning body using append
  // This ensures parent entry comes before nested entries in natural order
  // Store origin_id; origin will be looked up from ast_after_second
  let state_with_entry =
    CloneState(
      next_id: state_after_id.next_id,
      id_relationships: new_relationships,
      origin_entries: list.append(state_after_id.origin_entries, [
        #(placeholder, origin_id),
      ]),
    )

  // NOW clone the body (nested entries will be appended AFTER parent entry)
  let #(cloned_body, state2) =
    clone_alternatives_impl(origin_node.body, state_with_entry)

  // Create the real copy with the cloned body
  let copy =
    CapturingGroupNode(
      number: origin_node.number,
      name: origin_node.name,
      is_subroutined: clone_is_subroutined,
      body: cloned_body,
      transform_id: Some(clone_id),
    )

  // Replace the placeholder in origin_entries with the real copy
  // This is needed for recursive clones that become SubroutineE and won't be
  // in the final AST - we need the real copy with its body for the fallback
  let updated_entries =
    list.map(state2.origin_entries, fn(entry) {
      let #(node, oid) = entry
      case node.transform_id {
        Some(id) if id == clone_id -> #(copy, oid)
        _ -> entry
      }
    })

  let final_state = CloneState(..state2, origin_entries: updated_entries)

  #(copy, final_state)
}

/// Clone a list of alternatives (internal implementation)
fn clone_alternatives_impl(
  alts: List(AlternativeNode),
  state: CloneState,
) -> #(List(AlternativeNode), CloneState) {
  list.fold(alts, #([], state), fn(acc, alt) {
    let #(cloned_alts, st) = acc
    let #(cloned_body, new_st) = clone_elements_impl(alt.body, st)
    let cloned_alt = AlternativeNode(body: cloned_body)
    #(list.append(cloned_alts, [cloned_alt]), new_st)
  })
}

/// Clone a list of alternative elements (internal implementation)
fn clone_elements_impl(
  elements: List(AlternativeElement),
  state: CloneState,
) -> #(List(AlternativeElement), CloneState) {
  list.fold(elements, #([], state), fn(acc, elem) {
    let #(cloned_elems, st) = acc
    let #(cloned_elem, new_st) = clone_element_impl(elem, st)
    #(list.append(cloned_elems, [cloned_elem]), new_st)
  })
}

/// Clone a single alternative element (internal implementation - nested clones)
fn clone_element_impl(
  elem: AlternativeElement,
  state: CloneState,
) -> #(AlternativeElement, CloneState) {
  case elem {
    CapturingGroupE(node) -> {
      // Nested clone - is_top_level=False (don't set is_subroutined)
      let #(cloned, new_st) = clone_capturing_group_impl(node, state, False)
      #(CapturingGroupE(cloned), new_st)
    }
    GroupE(node) -> {
      let #(cloned_body, new_st) = clone_alternatives_impl(node.body, state)
      let cloned = GroupNode(..node, body: cloned_body)
      #(GroupE(cloned), new_st)
    }
    LookaroundAssertionE(node) -> {
      let #(cloned_body, new_st) = clone_alternatives_impl(node.body, state)
      let cloned = LookaroundAssertionNode(..node, body: cloned_body)
      #(LookaroundAssertionE(cloned), new_st)
    }
    AbsenceFunctionE(node) -> {
      let #(cloned_body, new_st) = clone_alternatives_impl(node.body, state)
      let cloned = AbsenceFunctionNode(..node, body: cloned_body)
      #(AbsenceFunctionE(cloned), new_st)
    }
    QuantifierE(node) -> {
      let #(cloned_body, new_st) = clone_quantifiable_impl(node.body, state)
      let cloned = QuantifierNode(..node, body: cloned_body)
      #(QuantifierE(cloned), new_st)
    }
    CharacterClassE(node) -> {
      let #(cloned_body, new_st) =
        clone_character_class_elements_impl(node.body, state)
      let cloned = CharacterClassNode(..node, body: cloned_body)
      #(CharacterClassE(cloned), new_st)
    }
    // These don't contain nested CapturingGroups - just return as-is
    CharacterE(_)
    | CharacterSetE(_)
    | AssertionE(_)
    | BackreferenceE(_)
    | SubroutineE(_)
    | DirectiveE(_)
    | NamedCalloutE(_) -> #(elem, state)
  }
}

/// Clone a quantifiable node (internal implementation)
fn clone_quantifiable_impl(
  node: QuantifiableNode,
  state: CloneState,
) -> #(QuantifiableNode, CloneState) {
  case node {
    CapturingGroupQ(group) -> {
      // Nested clone - is_top_level=False
      let #(cloned, new_st) = clone_capturing_group_impl(group, state, False)
      #(CapturingGroupQ(cloned), new_st)
    }
    GroupQ(group) -> {
      let #(cloned_body, new_st) = clone_alternatives_impl(group.body, state)
      let cloned = GroupNode(..group, body: cloned_body)
      #(GroupQ(cloned), new_st)
    }
    AbsenceFunctionQ(af) -> {
      let #(cloned_body, new_st) = clone_alternatives_impl(af.body, state)
      let cloned = AbsenceFunctionNode(..af, body: cloned_body)
      #(AbsenceFunctionQ(cloned), new_st)
    }
    QuantifierQ(q) -> {
      let #(cloned_body, new_st) = clone_quantifiable_impl(q.body, state)
      let cloned = QuantifierNode(..q, body: cloned_body)
      #(QuantifierQ(cloned), new_st)
    }
    // These don't contain nested CapturingGroups
    CharacterQ(_)
    | CharacterSetQ(_)
    | CharacterClassQ(_)
    | BackreferenceQ(_)
    | SubroutineQ(_) -> #(node, state)
  }
}

/// Clone character class elements (rarely contains groups but for completeness)
fn clone_character_class_elements_impl(
  elements: List(CharacterClassElement),
  state: CloneState,
) -> #(List(CharacterClassElement), CloneState) {
  // Character classes typically don't contain CapturingGroups, but for nested
  // classes and intersections we need to handle them
  list.fold(elements, #([], state), fn(acc, elem) {
    let #(cloned_elems, st) = acc
    let #(cloned_elem, new_st) = clone_character_class_element_impl(elem, st)
    #(list.append(cloned_elems, [cloned_elem]), new_st)
  })
}

fn clone_character_class_element_impl(
  elem: CharacterClassElement,
  state: CloneState,
) -> #(CharacterClassElement, CloneState) {
  case elem {
    CharacterClassCCE(node) -> {
      let #(cloned_body, new_st) =
        clone_character_class_elements_impl(node.body, state)
      let cloned = CharacterClassNode(..node, body: cloned_body)
      #(CharacterClassCCE(cloned), new_st)
    }
    // These don't contain nested structures that could have CapturingGroups
    CharacterCCE(_) | CharacterSetCCE(_) | CharacterClassRangeCCE(_) -> #(
      elem,
      state,
    )
  }
}
