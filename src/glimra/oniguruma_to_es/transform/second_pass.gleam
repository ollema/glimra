//// Second pass transformations for Oniguruma to ES conversion.
////
//// Handles:
//// - Subroutine expansion by cloning
//// - Recursion detection
//// - Backref multiplexing state tracking
//// - Duplicate group name tracking

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import glimra/oniguruma_parser/parser/ast_types.{
  type AlternativeElement, type AlternativeNode, type BackreferenceNode,
  type BackreferenceRef, type CapturingGroupNode, type CharacterClassElement,
  type GroupNode, type QuantifiableNode, type QuantifierNode, type RegexNode,
  type SubroutineNode, AbsenceFunctionE, AbsenceFunctionNode, AlternativeNode,
  AssertionE, BackreferenceE, CapturingGroupE, CapturingGroupNode, CharacterCCE,
  CharacterClassCCE, CharacterClassE, CharacterClassNode, CharacterClassRangeCCE,
  CharacterE, CharacterSetCCE, CharacterSetE, DirectiveE, GroupE, GroupNode,
  LookaroundAssertionE, LookaroundAssertionNode, NamedCalloutE, NamedRef,
  NamedSubroutineRef, NumberedRef, NumberedSubroutineRef, QuantifierE,
  QuantifierNode, RegexNode, SubroutineE, SubroutineNode,
}
import glimra/oniguruma_to_es/transform/types.{
  type GroupNameInfo, type SubroutineRefEntry, type SubroutineRefKey,
  GroupNameInfo, NamedKey, NumberedKey,
}
import glimra/oniguruma_to_es/transform/utils.{
  type CloneState, type CurrentFlags, are_flags_equal, clone_capturing_group,
  get_flag_mods_from_flags, get_new_current_flags,
}

// ============================================================================
// State Types
// ============================================================================

/// State for second pass transformation
pub type SecondPassState {
  SecondPassState(
    current_flags: CurrentFlags,
    prev_flags: Option(CurrentFlags),
    global_flags: CurrentFlags,
    /// Clone state tracking (replaces group_origin_by_copy)
    clone_state: CloneState,
    groups_by_name: Dict(String, Dict(CapturingGroupNode, GroupNameInfo)),
    multiplex_captures_to_left_by_ref: Dict(MultiplexKey, List(MultiplexEntry)),
    open_refs: Dict(Int, CapturingGroupNode),
    reffed_nodes_by_referencer: Dict(Int, List(CapturingGroupNode)),
    subroutine_ref_map: Dict(SubroutineRefKey, SubroutineRefEntry),
    /// Set of group numbers that are actually targeted by subroutines
    subroutine_target_numbers: Set(Int),
  )
}

/// Key for multiplex tracking
pub type MultiplexKey {
  MultiplexNumberKey(Int)
  MultiplexNameKey(String)
}

/// Entry for multiplex tracking
pub type MultiplexEntry {
  MultiplexEntry(node: CapturingGroupNode, origin: Option(CapturingGroupNode))
}

// ============================================================================
// Main Entry Point
// ============================================================================

/// Run the second pass transformation
pub fn run(
  ast: RegexNode,
  state: SecondPassState,
) -> #(RegexNode, SecondPassState) {
  let #(new_body, final_state) = transform_alternatives(ast.body, state)
  #(RegexNode(..ast, body: new_body), final_state)
}

// ============================================================================
// Alternative Transformation
// ============================================================================

fn transform_alternatives(
  alts: List(AlternativeNode),
  state: SecondPassState,
) -> #(List(AlternativeNode), SecondPassState) {
  transform_alternatives_loop(alts, [], state)
}

fn transform_alternatives_loop(
  remaining: List(AlternativeNode),
  acc: List(AlternativeNode),
  state: SecondPassState,
) -> #(List(AlternativeNode), SecondPassState) {
  case remaining {
    [] -> #(list.reverse(acc), state)
    [alt, ..rest] -> {
      let #(new_alt, new_state) = transform_alternative(alt, state)
      transform_alternatives_loop(rest, [new_alt, ..acc], new_state)
    }
  }
}

fn transform_alternative(
  alt: AlternativeNode,
  state: SecondPassState,
) -> #(AlternativeNode, SecondPassState) {
  let #(new_elements, new_state) = transform_elements(alt.body, state)
  #(AlternativeNode(body: new_elements), new_state)
}

// ============================================================================
// Element Transformation
// ============================================================================

fn transform_elements(
  elements: List(AlternativeElement),
  state: SecondPassState,
) -> #(List(AlternativeElement), SecondPassState) {
  transform_elements_loop(elements, [], state)
}

fn transform_elements_loop(
  remaining: List(AlternativeElement),
  acc: List(AlternativeElement),
  state: SecondPassState,
) -> #(List(AlternativeElement), SecondPassState) {
  case remaining {
    [] -> #(list.reverse(acc), state)
    [elem, ..rest] -> {
      let #(new_elems, new_state) = transform_element(elem, state)
      transform_elements_loop(
        rest,
        list.append(list.reverse(new_elems), acc),
        new_state,
      )
    }
  }
}

fn transform_element(
  element: AlternativeElement,
  state: SecondPassState,
) -> #(List(AlternativeElement), SecondPassState) {
  case element {
    AbsenceFunctionE(node) -> {
      let #(new_body, new_state) = transform_alternatives(node.body, state)
      let new_node = AbsenceFunctionNode(..node, body: new_body)
      #([AbsenceFunctionE(new_node)], new_state)
    }
    AssertionE(_) -> #([element], state)
    BackreferenceE(node) -> transform_backreference(node, state)
    CapturingGroupE(node) -> transform_capturing_group(node, state)
    CharacterE(_) -> #([element], state)
    CharacterClassE(node) -> {
      let #(new_body, new_state) = transform_cc_elements(node.body, state)
      let new_node = CharacterClassNode(..node, body: new_body)
      #([CharacterClassE(new_node)], new_state)
    }
    CharacterSetE(_) -> #([element], state)
    DirectiveE(_) -> #([element], state)
    GroupE(node) -> transform_group(node, state)
    LookaroundAssertionE(node) -> {
      let #(new_body, new_state) = transform_alternatives(node.body, state)
      let new_node = LookaroundAssertionNode(..node, body: new_body)
      #([LookaroundAssertionE(new_node)], new_state)
    }
    NamedCalloutE(_) -> #([element], state)
    QuantifierE(node) -> transform_quantifier(node, state)
    SubroutineE(node) -> transform_subroutine(node, state)
  }
}

// ============================================================================
// Backreference Transformation
// ============================================================================

fn transform_backreference(
  node: BackreferenceNode,
  state: SecondPassState,
) -> #(List(AlternativeElement), SecondPassState) {
  case node.orphan {
    Some(True) -> #([BackreferenceE(node)], state)
    _ -> {
      // Track the multiplex state for later expansion
      let key = case node.ref {
        NumberedRef(n) -> MultiplexNumberKey(n)
        NamedRef(name) -> MultiplexNameKey(name)
      }

      let captures = case
        dict.get(state.multiplex_captures_to_left_by_ref, key)
      {
        Ok(entries) -> list.map(entries, fn(e) { e.node })
        Error(_) -> []
      }

      // Store the referenced nodes for third pass
      let new_reffed =
        dict.insert(
          state.reffed_nodes_by_referencer,
          node.ref |> ref_to_int,
          captures,
        )

      #(
        [BackreferenceE(node)],
        SecondPassState(..state, reffed_nodes_by_referencer: new_reffed),
      )
    }
  }
}

/// Convert a BackreferenceRef to an Int for tracking
fn ref_to_int(ref: BackreferenceRef) -> Int {
  case ref {
    NumberedRef(n) -> n
    NamedRef(name) -> string_hash(name)
  }
}

/// Simple string hash for tracking
fn string_hash(s: String) -> Int {
  s
  |> string.to_utf_codepoints
  |> list.fold(0, fn(acc, cp) { acc * 31 + string.utf_codepoint_to_int(cp) })
}

// ============================================================================
// Capturing Group Transformation
// ============================================================================

fn transform_capturing_group(
  node: CapturingGroupNode,
  state: SecondPassState,
) -> #(List(AlternativeElement), SecondPassState) {
  // Check if this node is a clone (has a transform_id that's a key in id_relationships)
  let is_clone = case node.transform_id {
    Some(id) -> dict.has_key(state.clone_state.id_relationships, id)
    None -> False
  }

  // Handle recursion: if this is a clone AND its number is in openRefs,
  // it's a recursive reference - replace with recursion marker
  case is_clone && dict.has_key(state.open_refs, node.number) {
    True -> {
      // Get the open node for tracking
      let open_node = case dict.get(state.open_refs, node.number) {
        Ok(n) -> n
        Error(_) -> node
        // Shouldn't happen
      }
      // Create recursion marker
      let recursion_node =
        SubroutineNode(
          ref: NumberedSubroutineRef(node.number),
          is_recursive: Some(True),
        )
      // Track the referenced node for third pass
      let recursion_key =
        node.number * 1000 + dict.size(state.reffed_nodes_by_referencer)
      let new_reffed =
        dict.insert(state.reffed_nodes_by_referencer, recursion_key, [open_node])
      let new_state =
        SecondPassState(..state, reffed_nodes_by_referencer: new_reffed)
      #([SubroutineE(recursion_node)], new_state)
    }
    False -> transform_capturing_group_normal(node, state)
  }
}

fn transform_capturing_group_normal(
  node: CapturingGroupNode,
  state: SecondPassState,
) -> #(List(AlternativeElement), SecondPassState) {
  // Check if this group is referenced by a subroutine (should be marked is_subroutined)
  let is_reffed_by_subroutine =
    is_subroutine_target(node, state.subroutine_target_numbers)
  let node_with_subroutined = case is_reffed_by_subroutine {
    True -> CapturingGroupNode(..node, is_subroutined: Some(True))
    False -> node
  }

  // Mark this group as open for recursion detection
  let state_with_open =
    SecondPassState(
      ..state,
      open_refs: dict.insert(
        state.open_refs,
        node_with_subroutined.number,
        node_with_subroutined,
      ),
    )

  // Track multiplex data
  let state_with_multiplex =
    track_multiplex_data(node_with_subroutined, state_with_open)

  // Track duplicate names
  let state_with_names =
    track_duplicate_names(node_with_subroutined, state_with_multiplex)

  // Transform body
  let #(new_body, state_after_body) =
    transform_alternatives(node_with_subroutined.body, state_with_names)

  // Remove from open refs
  let final_state =
    SecondPassState(
      ..state_after_body,
      open_refs: dict.delete(
        state_after_body.open_refs,
        node_with_subroutined.number,
      ),
    )

  let new_node = CapturingGroupNode(..node_with_subroutined, body: new_body)
  #([CapturingGroupE(new_node)], final_state)
}

/// Check if a capturing group is referenced by a subroutine
fn is_subroutine_target(
  node: CapturingGroupNode,
  subroutine_target_numbers: Set(Int),
) -> Bool {
  set.contains(subroutine_target_numbers, node.number)
}

/// Track multiplex data for backref multiplexing
fn track_multiplex_data(
  node: CapturingGroupNode,
  state: SecondPassState,
) -> SecondPassState {
  // Check if this node is a clone by looking up its transform_id
  let origin = case node.transform_id {
    Some(id) ->
      case dict.get(state.clone_state.id_relationships, id) {
        Ok(_origin_id) -> Some(node)
        // Just mark as having an origin
        Error(_) -> None
      }
    None -> None
  }

  let entry = MultiplexEntry(node: node, origin: origin)

  // Add to multiplex map by number
  let by_number =
    add_to_multiplex_list(
      state.multiplex_captures_to_left_by_ref,
      MultiplexNumberKey(node.number),
      entry,
    )

  // Add to multiplex map by name if present
  let by_name_and_number = case node.name {
    Some(name) ->
      add_to_multiplex_list(by_number, MultiplexNameKey(name), entry)
    None -> by_number
  }

  SecondPassState(
    ..state,
    multiplex_captures_to_left_by_ref: by_name_and_number,
  )
}

/// Add an entry to a multiplex list, handling origin replacement
fn add_to_multiplex_list(
  map: Dict(MultiplexKey, List(MultiplexEntry)),
  key: MultiplexKey,
  entry: MultiplexEntry,
) -> Dict(MultiplexKey, List(MultiplexEntry)) {
  let existing = case dict.get(map, key) {
    Ok(entries) -> entries
    Error(_) -> []
  }

  // Filter out entries that this one replaces (same origin)
  let filtered = case entry.origin {
    Some(origin) ->
      list.filter(existing, fn(e) {
        case e.origin {
          Some(o) -> o != origin && e.node != origin
          None -> e.node != origin
        }
      })
    None -> existing
  }

  dict.insert(map, key, list.append(filtered, [entry]))
}

/// Track duplicate names for later removal
fn track_duplicate_names(
  node: CapturingGroupNode,
  state: SecondPassState,
) -> SecondPassState {
  case node.name {
    None -> state
    Some(name) -> {
      // Check if this node is a clone
      let is_clone = case node.transform_id {
        Some(id) -> dict.has_key(state.clone_state.id_relationships, id)
        None -> False
      }
      let existing = case dict.get(state.groups_by_name, name) {
        Ok(groups) -> groups
        Error(_) -> dict.new()
      }

      // Check if we need to mark this as duplicate
      let has_duplicate = case is_clone {
        True -> True
        // From subroutine expansion
        False -> dict.size(existing) > 0
      }

      let info =
        GroupNameInfo(node: node, has_duplicate_name_to_remove: has_duplicate)
      let new_groups = dict.insert(existing, node, info)
      let new_groups_by_name =
        dict.insert(state.groups_by_name, name, new_groups)

      SecondPassState(..state, groups_by_name: new_groups_by_name)
    }
  }
}

// ============================================================================
// Group Transformation
// ============================================================================

fn transform_group(
  node: GroupNode,
  state: SecondPassState,
) -> #(List(AlternativeElement), SecondPassState) {
  // Save previous flags
  let prev = state.current_flags
  let state_with_prev = SecondPassState(..state, prev_flags: Some(prev))

  // Update current flags if group has flags
  let state_with_flags = case node.flags {
    Some(flags) ->
      SecondPassState(
        ..state_with_prev,
        current_flags: get_new_current_flags(state.current_flags, flags),
      )
    None -> state_with_prev
  }

  // Transform body
  let #(new_body, state_after_body) =
    transform_alternatives(node.body, state_with_flags)

  // Restore previous flags
  let final_state = SecondPassState(..state_after_body, current_flags: prev)

  let new_node = GroupNode(..node, body: new_body)
  #([GroupE(new_node)], final_state)
}

// ============================================================================
// Quantifier Transformation
// ============================================================================

fn transform_quantifier(
  node: QuantifierNode,
  state: SecondPassState,
) -> #(List(AlternativeElement), SecondPassState) {
  let #(new_body, new_state) = transform_quantifiable(node.body, state)
  let new_node = QuantifierNode(..node, body: new_body)
  #([QuantifierE(new_node)], new_state)
}

fn transform_quantifiable(
  node: QuantifiableNode,
  state: SecondPassState,
) -> #(QuantifiableNode, SecondPassState) {
  case node {
    ast_types.AbsenceFunctionQ(af) -> {
      let #(new_body, new_state) = transform_alternatives(af.body, state)
      #(
        ast_types.AbsenceFunctionQ(AbsenceFunctionNode(..af, body: new_body)),
        new_state,
      )
    }
    ast_types.BackreferenceQ(br) -> {
      let #(elems, new_state) = transform_backreference(br, state)
      case elems {
        [BackreferenceE(new_br)] -> #(
          ast_types.BackreferenceQ(new_br),
          new_state,
        )
        _ -> #(node, new_state)
      }
    }
    ast_types.CapturingGroupQ(cg) -> {
      let #(elems, new_state) = transform_capturing_group(cg, state)
      case elems {
        [CapturingGroupE(new_cg)] -> #(
          ast_types.CapturingGroupQ(new_cg),
          new_state,
        )
        _ -> #(node, new_state)
      }
    }
    ast_types.CharacterQ(_) -> #(node, state)
    ast_types.CharacterClassQ(cc) -> {
      let #(new_body, new_state) = transform_cc_elements(cc.body, state)
      #(
        ast_types.CharacterClassQ(CharacterClassNode(..cc, body: new_body)),
        new_state,
      )
    }
    ast_types.CharacterSetQ(_) -> #(node, state)
    ast_types.GroupQ(g) -> {
      let #(elems, new_state) = transform_group(g, state)
      case elems {
        [GroupE(new_g)] -> #(ast_types.GroupQ(new_g), new_state)
        _ -> #(node, new_state)
      }
    }
    ast_types.QuantifierQ(q) -> {
      let #(elems, new_state) = transform_quantifier(q, state)
      case elems {
        [QuantifierE(new_q)] -> #(ast_types.QuantifierQ(new_q), new_state)
        _ -> #(node, new_state)
      }
    }
    ast_types.SubroutineQ(sub) -> {
      let #(elems, new_state) = transform_subroutine(sub, state)
      case elems {
        [SubroutineE(new_sub)] -> #(ast_types.SubroutineQ(new_sub), new_state)
        [CapturingGroupE(cg)] -> #(ast_types.CapturingGroupQ(cg), new_state)
        [GroupE(g)] -> #(ast_types.GroupQ(g), new_state)
        _ -> #(node, new_state)
      }
    }
  }
}

// ============================================================================
// Character Class Transformation
// ============================================================================

fn transform_cc_elements(
  elements: List(CharacterClassElement),
  state: SecondPassState,
) -> #(List(CharacterClassElement), SecondPassState) {
  transform_cc_elements_loop(elements, [], state)
}

fn transform_cc_elements_loop(
  remaining: List(CharacterClassElement),
  acc: List(CharacterClassElement),
  state: SecondPassState,
) -> #(List(CharacterClassElement), SecondPassState) {
  case remaining {
    [] -> #(list.reverse(acc), state)
    [elem, ..rest] -> {
      let #(new_elem, new_state) = transform_cc_element(elem, state)
      transform_cc_elements_loop(rest, [new_elem, ..acc], new_state)
    }
  }
}

fn transform_cc_element(
  element: CharacterClassElement,
  state: SecondPassState,
) -> #(CharacterClassElement, SecondPassState) {
  case element {
    CharacterCCE(_) -> #(element, state)
    CharacterClassCCE(node) -> {
      let #(new_body, new_state) = transform_cc_elements(node.body, state)
      #(
        CharacterClassCCE(CharacterClassNode(..node, body: new_body)),
        new_state,
      )
    }
    CharacterClassRangeCCE(_) -> #(element, state)
    CharacterSetCCE(_) -> #(element, state)
  }
}

// ============================================================================
// Subroutine Transformation
// ============================================================================

fn transform_subroutine(
  node: SubroutineNode,
  state: SecondPassState,
) -> #(List(AlternativeElement), SecondPassState) {
  // Get the referenced group
  let key = case node.ref {
    NumberedSubroutineRef(n) -> NumberedKey(n)
    NamedSubroutineRef(name) -> NamedKey(name)
  }

  case dict.get(state.subroutine_ref_map, key) {
    Error(_) -> #([SubroutineE(node)], state)
    Ok(reffed_entry) -> {
      // Check for global recursion (ref 0) - only case we don't clone
      case node.ref {
        NumberedSubroutineRef(0) -> {
          // Global recursion - mark as recursive
          #(
            [SubroutineE(SubroutineNode(..node, is_recursive: Some(True)))],
            state,
          )
        }
        _ -> {
          // Clone the referenced group and traverse it
          // Recursion is handled in transform_capturing_group when the clone
          // is detected AND its number is already open
          let #(cloned, new_clone_state) =
            clone_capturing_group(reffed_entry.group, state.clone_state)

          let state_with_clone =
            SecondPassState(..state, clone_state: new_clone_state)

          // Check if flags need to be wrapped
          let reffed_flags = reffed_entry.flags

          case are_flags_equal(reffed_flags, state.current_flags) {
            True -> {
              // Transform the cloned group
              let #(elems, final_state) =
                transform_capturing_group(cloned, state_with_clone)
              #(elems, final_state)
            }
            False -> {
              // Wrap in a flag group
              let flag_mods = get_flag_mods_from_flags(reffed_flags)
              let wrapper =
                GroupNode(atomic: None, flags: Some(flag_mods), body: [
                  AlternativeNode(body: [CapturingGroupE(cloned)]),
                ])
              let #(elems, final_state) =
                transform_group(wrapper, state_with_clone)
              #(elems, final_state)
            }
          }
        }
      }
    }
  }
}

import gleam/string
