/// Third pass transformations for Oniguruma to ES conversion.
///
/// Handles:
/// - CapturingGroup → renumber groups, remove duplicate names
/// - Backreference → recalculate numbers, multiplex for duplicates
/// - Subroutine → update recursion refs
/// - Regex.exit → add dummy captures for orphan backrefs
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import glimra/oniguruma_parser/parser/ast_types.{
  type AlternativeElement, type AlternativeNode, type BackreferenceNode,
  type BackreferenceRef, type CapturingGroupNode, type CharacterClassElement,
  type CharacterClassNode, type GroupNode, type LookaroundAssertionNode,
  type QuantifiableNode, type QuantifierNode, type RegexNode,
  type SubroutineNode, type SubroutineRef, AbsenceFunctionE, AbsenceFunctionNode,
  AlternativeNode, AssertionE, BackreferenceE, BackreferenceNode,
  CapturingGroupE, CapturingGroupNode, CharacterCCE, CharacterClassCCE,
  CharacterClassE, CharacterClassNode, CharacterClassRangeCCE, CharacterE,
  CharacterSetCCE, CharacterSetE, DirectiveE, GroupE, GroupNode, Lookahead,
  LookaroundAssertionE, LookaroundAssertionNode, NamedCalloutE, NamedRef,
  NamedSubroutineRef, NumberedRef, NumberedSubroutineRef, QuantifierE,
  QuantifierNode, RegexNode, SubroutineE, SubroutineNode,
}
import glimra/oniguruma_to_es/transform/types.{type GroupNameInfo}

// ============================================================================
// State Types
// ============================================================================

/// State for third pass transformation
pub type ThirdPassState {
  ThirdPassState(
    groups_by_name: Dict(String, Dict(CapturingGroupNode, GroupNameInfo)),
    highest_orphan_backref: Int,
    num_captures_to_left: Int,
    /// Track currently open groups (groups we're inside)
    open_groups: List(Int),
    reffed_nodes_by_referencer: Dict(Int, List(CapturingGroupNode)),
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
              CapturingGroupE(
                CapturingGroupNode(
                  number: -1,
                  // Sentinel for null
                  name: None,
                  is_subroutined: None,
                  body: [AlternativeNode(body: [])],
                ),
              )
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
  transform_alternatives_loop(alts, [], state)
}

fn transform_alternatives_loop(
  remaining: List(AlternativeNode),
  acc: List(AlternativeNode),
  state: ThirdPassState,
) -> #(List(AlternativeNode), ThirdPassState) {
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

      // Filter to participating captures
      let participants =
        list.filter(reffed_nodes, fn(reffed) {
          can_participate_with_node(
            reffed,
            state.num_captures_to_left,
            state.open_groups,
          )
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

/// Check if a capture can participate with a node
/// A capture can participate if:
/// 1. Its number is <= current position (it's been defined)
/// 2. It's not currently "open" (we're not inside it)
fn can_participate_with_node(
  capture: CapturingGroupNode,
  current_num: Int,
  open_groups: List(Int),
) -> Bool {
  // Capture must be defined and not currently open (we're inside it)
  capture.number <= current_num
  && !list.contains(open_groups, capture.number)
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

  // Add to open groups (we're inside this group now)
  let state_with_open =
    ThirdPassState(
      ..state,
      num_captures_to_left: new_num,
      open_groups: [new_num, ..state.open_groups],
    )

  // Check if name should be removed (duplicate)
  let new_name = case node.name {
    None -> None
    Some(name) -> {
      case should_remove_duplicate_name(node, name, state.groups_by_name) {
        True -> None
        False -> Some(name)
      }
    }
  }

  // Transform body
  let #(new_body, state_after_body) =
    transform_alternatives(node.body, state_with_open)

  // Remove from open groups (we've exited this group)
  let final_state =
    ThirdPassState(
      ..state_after_body,
      open_groups: list.filter(state_after_body.open_groups, fn(n) {
        n != new_num
      }),
    )

  let new_node =
    CapturingGroupNode(..node, number: new_num, name: new_name, body: new_body)
  #([CapturingGroupE(new_node)], final_state)
}

/// Check if a duplicate name should be removed
fn should_remove_duplicate_name(
  node: CapturingGroupNode,
  name: String,
  groups_by_name: Dict(String, Dict(CapturingGroupNode, GroupNameInfo)),
) -> Bool {
  case dict.get(groups_by_name, name) {
    Error(_) -> False
    Ok(groups) -> {
      case dict.get(groups, node) {
        Error(_) -> False
        Ok(info) -> info.has_duplicate_name_to_remove
      }
    }
  }
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
  // This is simplified - a full implementation would track the referenced group
  #([SubroutineE(node)], state)
}

import gleam/string
