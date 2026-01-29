//// Third pass transformations for Oniguruma to ES conversion.
////
//// Handles:
//// - CapturingGroup → renumber groups, remove duplicate names
//// - Backreference → recalculate numbers, multiplex for duplicates
//// - Subroutine → update recursion refs
//// - Regex.exit → add dummy captures for orphan backrefs

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/set.{type Set}
import gleam/string
import glimra/oniguruma_parser/parser/ast_types.{
  type AlternativeElement, type AlternativeNode, type BackreferenceNode,
  type BackreferenceRef, type CapturingGroupNode, type CharacterClassElement,
  type QuantifiableNode, type QuantifierNode, type RegexNode,
  type SubroutineNode, AbsenceFunctionE, AbsenceFunctionNode, AlternativeNode,
  AssertionE, BackreferenceE, BackreferenceNode, CapturingGroupE,
  CapturingGroupNode, CharacterCCE, CharacterClassCCE, CharacterClassE,
  CharacterClassNode, CharacterClassRangeCCE, CharacterE, CharacterSetCCE,
  CharacterSetE, DirectiveE, GroupE, GroupNode, Lookahead, LookaroundAssertionE,
  LookaroundAssertionNode, NamedCalloutE, NamedRef, NamedSubroutineRef,
  NumberedRef, NumberedSubroutineRef, QuantifierE, QuantifierNode, RegexNode,
  SubroutineE, SubroutineNode,
}
import glimra/oniguruma_to_es/transform/types.{type GroupNameInfo}
import glimra/oniguruma_to_es/transform/utils.{type CloneState}

// ============================================================================
// State Types
// ============================================================================

/// State for third pass transformation
pub type ThirdPassState {
  ThirdPassState(
    /// Names we've already emitted (used to remove duplicates)
    emitted_names: Set(String),
    groups_by_name: Dict(String, Dict(CapturingGroupNode, GroupNameInfo)),
    /// Clone state from second pass (contains id_relationships for origin tracking)
    clone_state: CloneState,
    highest_orphan_backref: Int,
    num_captures_to_left: Int,
    /// Track currently open groups (groups we're inside)
    /// Each entry is #(original_number, new_number)
    open_groups: List(#(Int, Int)),
    reffed_nodes_by_referencer: Dict(Int, List(CapturingGroupNode)),
    /// Captures that can participate with backreferences in the current alternation path
    /// This tracks captures that are "to the left" in the same path, excluding sibling alternatives
    captures_in_path: List(CapturingGroupNode),
  )
}

// ============================================================================
// Main Entry Point
// ============================================================================

/// Run the third pass transformation
pub fn run(
  ast: RegexNode,
  state: ThirdPassState,
) -> #(RegexNode, ThirdPassState) {
  let #(new_body, state_after_body) = transform_alternatives(ast.body, state)

  // Add dummy captures for orphan backrefs if needed
  let #(final_body, final_state) =
    add_dummy_captures(new_body, state_after_body)

  #(RegexNode(..ast, body: final_body), final_state)
}

// ============================================================================
// Dummy Capture Addition
// ============================================================================

/// Add empty capturing groups at the end for orphan backrefs
fn add_dummy_captures(
  body: List(AlternativeNode),
  state: ThirdPassState,
) -> #(List(AlternativeNode), ThirdPassState) {
  let num_caps_needed =
    int.max(state.highest_orphan_backref - state.num_captures_to_left, 0)

  case num_caps_needed {
    0 -> #(body, state)
    _ -> {
      // Add empty captures to the last alternative
      case list.reverse(body) {
        [] -> #(body, state)
        [last_alt, ..rest_reversed] -> {
          // Create dummy captures with number=-1 (sentinel for null)
          // and body containing an empty alternative
          let dummy_captures =
            list.range(1, num_caps_needed)
            |> list.map(fn(_i) {
              CapturingGroupE(CapturingGroupNode(
                number: -1,
                // Sentinel for null
                name: None,
                is_subroutined: None,
                body: [AlternativeNode(body: [])],
                transform_id: None,
              ))
            })

          let new_last =
            AlternativeNode(body: list.append(last_alt.body, dummy_captures))
          let new_body = list.reverse([new_last, ..rest_reversed])
          let new_state =
            ThirdPassState(
              ..state,
              num_captures_to_left: state.num_captures_to_left + num_caps_needed,
            )
          #(new_body, new_state)
        }
      }
    }
  }
}

// ============================================================================
// Alternative Transformation
// ============================================================================

fn transform_alternatives(
  alts: List(AlternativeNode),
  state: ThirdPassState,
) -> #(List(AlternativeNode), ThirdPassState) {
  // Save captures_in_path before processing alternatives
  // Sibling alternatives don't share their in-progress captures with each other
  let saved_captures = state.captures_in_path
  transform_alternatives_loop(alts, [], state, saved_captures, [])
}

fn transform_alternatives_loop(
  remaining: List(AlternativeNode),
  acc: List(AlternativeNode),
  state: ThirdPassState,
  saved_captures: List(CapturingGroupNode),
  accumulated_captures: List(CapturingGroupNode),
) -> #(List(AlternativeNode), ThirdPassState) {
  case remaining {
    [] -> {
      // After processing all alternatives, KEEP the accumulated captures
      // Captures from inside nested groups should be visible to backreferences
      // that come after the group containing these alternatives
      // Merge: saved_captures + all new captures found in any alternative
      let merged_captures = merge_captures(saved_captures, accumulated_captures)
      let final_state =
        ThirdPassState(..state, captures_in_path: merged_captures)
      #(list.reverse(acc), final_state)
    }
    [alt, ..rest] -> {
      // Reset captures_in_path before each alternative (siblings don't share)
      let state_for_alt =
        ThirdPassState(..state, captures_in_path: saved_captures)
      let #(new_alt, new_state) = transform_alternative(alt, state_for_alt)
      // Collect new captures found in this alternative
      let new_captures =
        list.filter(new_state.captures_in_path, fn(cap) {
          !list.any(saved_captures, fn(saved) {
            saved.transform_id == cap.transform_id
          })
        })
      let updated_accumulated = list.append(accumulated_captures, new_captures)
      transform_alternatives_loop(
        rest,
        [new_alt, ..acc],
        new_state,
        saved_captures,
        updated_accumulated,
      )
    }
  }
}

/// Merge two lists of captures, avoiding duplicates by transform_id
fn merge_captures(
  base: List(CapturingGroupNode),
  additions: List(CapturingGroupNode),
) -> List(CapturingGroupNode) {
  list.fold(additions, base, fn(acc, cap) {
    case
      list.any(acc, fn(existing) { existing.transform_id == cap.transform_id })
    {
      True -> acc
      False -> [cap, ..acc]
    }
  })
}

fn transform_alternative(
  alt: AlternativeNode,
  state: ThirdPassState,
) -> #(AlternativeNode, ThirdPassState) {
  let #(new_elements, new_state) = transform_elements(alt.body, state)
  #(AlternativeNode(body: new_elements), new_state)
}

// ============================================================================
// Element Transformation
// ============================================================================

fn transform_elements(
  elements: List(AlternativeElement),
  state: ThirdPassState,
) -> #(List(AlternativeElement), ThirdPassState) {
  transform_elements_loop(elements, [], state)
}

fn transform_elements_loop(
  remaining: List(AlternativeElement),
  acc: List(AlternativeElement),
  state: ThirdPassState,
) -> #(List(AlternativeElement), ThirdPassState) {
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
  state: ThirdPassState,
) -> #(List(AlternativeElement), ThirdPassState) {
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
    GroupE(node) -> {
      let #(new_body, new_state) = transform_alternatives(node.body, state)
      let new_node = GroupNode(..node, body: new_body)
      #([GroupE(new_node)], new_state)
    }
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
  state: ThirdPassState,
) -> #(List(AlternativeElement), ThirdPassState) {
  case node.orphan {
    Some(True) -> {
      // Track highest orphan backref
      let ref_num = case node.ref {
        NumberedRef(n) -> n
        NamedRef(_) -> 0
      }
      let new_state =
        ThirdPassState(
          ..state,
          highest_orphan_backref: int.max(state.highest_orphan_backref, ref_num),
        )
      #([BackreferenceE(node)], new_state)
    }
    _ -> {
      // Get referenced nodes for multiplexing
      let ref_key = ref_to_int(node.ref)
      let reffed_nodes = case
        dict.get(state.reffed_nodes_by_referencer, ref_key)
      {
        Ok(nodes) -> nodes
        Error(_) -> []
      }

      // Filter to participating captures - must be in the current alternation path
      // This handles the case where captures in sibling alternatives can't participate
      let participants =
        list.filter(reffed_nodes, fn(reffed) {
          is_capture_in_path(reffed, state.captures_in_path)
          && !is_in_open_groups(reffed.number, state.open_groups)
        })

      case participants {
        [] -> {
          // No participating capture - convert to (?!)
          let lookahead =
            LookaroundAssertionNode(kind: Lookahead, negate: True, body: [
              AlternativeNode(body: []),
            ])
          #([LookaroundAssertionE(lookahead)], state)
        }
        [single] -> {
          // Single participant - use its number
          let new_node =
            BackreferenceNode(..node, ref: NumberedRef(single.number))
          #([BackreferenceE(new_node)], state)
        }
        multiple -> {
          // Multiple participants - create multiplex group
          let alts =
            list.reverse(multiple)
            |> list.map(fn(reffed) {
              AlternativeNode(body: [
                BackreferenceE(BackreferenceNode(
                  ref: NumberedRef(reffed.number),
                  orphan: None,
                )),
              ])
            })

          let group = GroupNode(atomic: Some(True), flags: None, body: alts)
          #([GroupE(group)], state)
        }
      }
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

/// Check if an original group number is currently open
fn is_in_open_groups(
  original_number: Int,
  open_groups: List(#(Int, Int)),
) -> Bool {
  list.any(open_groups, fn(entry) { entry.0 == original_number })
}

/// Find the new number for an original group number in open_groups
fn find_new_number_for_original(
  original_number: Int,
  open_groups: List(#(Int, Int)),
) -> option.Option(Int) {
  list.find_map(open_groups, fn(entry) {
    case entry.0 == original_number {
      True -> Ok(entry.1)
      False -> Error(Nil)
    }
  })
  |> option.from_result
}

/// Check if a capture is in the current alternation path
/// Compares by transform_id for identity matching between the second pass
/// reffed_nodes and the third pass renumbered captures
fn is_capture_in_path(
  capture: CapturingGroupNode,
  path: List(CapturingGroupNode),
) -> Bool {
  list.any(path, fn(path_capture) {
    // Compare by transform_id for identity matching
    case capture.transform_id, path_capture.transform_id {
      Some(cap_id), Some(path_id) -> cap_id == path_id
      _, _ -> False
    }
  })
}

// ============================================================================
// Capturing Group Transformation
// ============================================================================

fn transform_capturing_group(
  node: CapturingGroupNode,
  state: ThirdPassState,
) -> #(List(AlternativeElement), ThirdPassState) {
  // Increment capture count and renumber
  let new_num = state.num_captures_to_left + 1
  let original_num = node.number

  // Add to open groups (we're inside this group now)
  // Track both original and new numbers for proper backref/subroutine handling
  let state_with_open =
    ThirdPassState(..state, num_captures_to_left: new_num, open_groups: [
      #(original_num, new_num),
      ..state.open_groups
    ])

  // Check if name should be removed (duplicate)
  // Use emitted_names set to track which names have been emitted
  // First occurrence keeps the name, subsequent ones have it removed
  let #(new_name, state_with_emitted) = case node.name {
    None -> #(None, state_with_open)
    Some(name) -> {
      case set.contains(state_with_open.emitted_names, name) {
        True ->
          // This name was already emitted, remove it
          #(None, state_with_open)
        False ->
          // First occurrence, keep the name and add to emitted set
          #(
            Some(name),
            ThirdPassState(
              ..state_with_open,
              emitted_names: set.insert(state_with_open.emitted_names, name),
            ),
          )
      }
    }
  }

  // Transform body
  let #(new_body, state_after_body) =
    transform_alternatives(node.body, state_with_emitted)

  let new_node =
    CapturingGroupNode(..node, number: new_num, name: new_name, body: new_body)

  // Remove from open groups (we've exited this group)
  // Add to captures_in_path (so backreferences after this group can see it)
  // Note: origin_map is built from transform_ids after all transforms are done
  let final_state =
    ThirdPassState(
      ..state_after_body,
      open_groups: list.filter(state_after_body.open_groups, fn(entry) {
        entry.1 != new_num
      }),
      // Add this capture to the path so subsequent backreferences can reference it
      captures_in_path: [new_node, ..state_after_body.captures_in_path],
    )

  #([CapturingGroupE(new_node)], final_state)
}

// ============================================================================
// Quantifier Transformation
// ============================================================================

fn transform_quantifier(
  node: QuantifierNode,
  state: ThirdPassState,
) -> #(List(AlternativeElement), ThirdPassState) {
  let #(new_body, new_state) = transform_quantifiable(node.body, state)
  let new_node = QuantifierNode(..node, body: new_body)
  #([QuantifierE(new_node)], new_state)
}

fn transform_quantifiable(
  node: QuantifiableNode,
  state: ThirdPassState,
) -> #(QuantifiableNode, ThirdPassState) {
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
        [GroupE(g)] -> #(ast_types.GroupQ(g), new_state)
        [LookaroundAssertionE(_)] -> #(node, new_state)
        // Can't quantify lookaround
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
      let #(new_body, new_state) = transform_alternatives(g.body, state)
      #(ast_types.GroupQ(GroupNode(..g, body: new_body)), new_state)
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
  state: ThirdPassState,
) -> #(List(CharacterClassElement), ThirdPassState) {
  transform_cc_elements_loop(elements, [], state)
}

fn transform_cc_elements_loop(
  remaining: List(CharacterClassElement),
  acc: List(CharacterClassElement),
  state: ThirdPassState,
) -> #(List(CharacterClassElement), ThirdPassState) {
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
  state: ThirdPassState,
) -> #(CharacterClassElement, ThirdPassState) {
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
  state: ThirdPassState,
) -> #(List(AlternativeElement), ThirdPassState) {
  // For recursive subroutines, update the ref to use the new number
  // The recursion appears within the group it references, so that group is in open_groups
  case node.is_recursive {
    Some(True) -> {
      // Get the original ref number
      let original_ref = case node.ref {
        NumberedSubroutineRef(n) -> n
        NamedSubroutineRef(_) -> 0
        // Named refs don't need updating
      }
      // Look up the new number from open_groups
      case find_new_number_for_original(original_ref, state.open_groups) {
        Some(new_num) -> {
          // Update ref to use the new number
          let new_node =
            SubroutineNode(..node, ref: NumberedSubroutineRef(new_num))
          #([SubroutineE(new_node)], state)
        }
        None -> {
          // Couldn't find the group - shouldn't happen for valid recursive refs
          #([SubroutineE(node)], state)
        }
      }
    }
    _ -> {
      // Non-recursive subroutines - no change needed
      #([SubroutineE(node)], state)
    }
  }
}
