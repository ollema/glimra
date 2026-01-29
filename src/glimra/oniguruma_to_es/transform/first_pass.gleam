//// First pass transformations for Oniguruma to ES conversion.
////
//// Handles syntactic transformations:
//// - AbsenceFunction → lookaround pattern
//// - Alternative → flag directive processing
//// - Assertion → line boundaries, \G, word boundaries
//// - Backreference → JS name validation
//// - CapturingGroup → name validation, subroutine map building
//// - CharacterSet → Unicode properties, POSIX classes, \d, \w, \s, \X
//// - Directive → flag modifiers, \K (keep)
//// - Flags → remove Onig-specific, set up options
//// - Quantifier → nested quantifier wrapping

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import glimra/oniguruma_parser/parser/ast_types.{
  type AbsenceFunctionNode, type AlternativeElement, type AlternativeNode,
  type AssertionNode, type BackreferenceNode, type CapturingGroupNode,
  type CharacterClassElement, type CharacterClassNode, type CharacterSetNode,
  type DirectiveNode, type FlagGroupModifiers, type FlagGroupSwitches,
  type FlagsNode, type GroupNode, type LookaroundAssertionNode,
  type NamedCalloutNode, type QuantifiableNode, type QuantifierNode,
  type RegexNode, type SubroutineNode, AbsenceFunctionE, AlternativeNode,
  AssertionE, AssertionNode, BackreferenceE, BackreferenceNode, CapturingGroupE,
  CapturingGroupNode, CharacterCCE, CharacterClassCCE, CharacterClassE,
  CharacterClassNode, CharacterClassRangeCCE, CharacterE, CharacterNode,
  CharacterSetCCE, CharacterSetE, CharacterSetNode, DirectiveE,
  FlagGroupModifiers, FlagGroupSwitches, FlagsNode, Greedy, GroupE, GroupNode,
  Intersection, LineEnd, LineStart, Lookahead, LookaroundAssertionE,
  LookaroundAssertionNode, Lookbehind, NamedCalloutE, NamedRef,
  NamedSubroutineRef, NumberedRef, NumberedSubroutineRef, QuantifierE,
  QuantifierNode, RegexNode, Repeater, SearchStart, StringEnd, StringEndNewline,
  StringStart, SubroutineE, SubroutineNode, TextSegmentBoundary, Union,
  WordBoundary,
}
import glimra/oniguruma_to_es/transform/types.{
  type Accuracy, type Strategy, type SubroutineRefKey, ClipSearch,
  DefaultAccuracy, NamedKey, NumberedKey, StrictAccuracy,
}
import glimra/oniguruma_to_es/transform/utils.{
  get_new_current_flags, get_or_insert_group_name, is_valid_js_group_name,
}

// ============================================================================
// State Types
// ============================================================================

/// State for first pass transformation
pub type FirstPassState {
  FirstPassState(
    accuracy: Accuracy,
    ascii_word_boundaries: Bool,
    avoid_subclass: Bool,
    min_target_es2024: Bool,
    /// Current flag context: #(dotAll, ignoreCase)
    current_flags: #(Bool, Bool),
    digit_is_ascii: Bool,
    space_is_ascii: Bool,
    word_is_ascii: Bool,
    flag_directives_by_alt: Dict(Int, List(DirectiveNode)),
    js_group_name_map: Dict(String, String),
    /// Counter for assigning transform_ids to CapturingGroups
    next_transform_id: Int,
    passed_lookbehind: Bool,
    strategy: Option(Strategy),
    subroutine_ref_map: Dict(SubroutineRefKey, types.SubroutineRefEntry),
    /// Set of group numbers that are actually targeted by subroutines
    subroutine_target_numbers: Set(Int),
    supported_g_nodes: List(AssertionNode),
  )
}

// ============================================================================
// Loop Step Types
// ============================================================================

// ============================================================================
// Main Entry Point
// ============================================================================

/// Run the first pass transformation
pub fn run(
  ast: RegexNode,
  state: FirstPassState,
) -> #(RegexNode, FirstPassState) {
  // Check for supported \G nodes before transformation
  let state_with_g = check_supported_g_nodes(ast, state)

  // Transform the body (is_top_level = True for the regex root)
  let #(new_body, final_state) =
    transform_alternatives(ast.body, state_with_g, 0, True)

  // Transform flags (remove Onig-specific)
  let new_flags = transform_flags(ast.flags)

  #(RegexNode(body: new_body, flags: new_flags), final_state)
}

// ============================================================================
// Supported \G Check
// ============================================================================

/// Check for \G nodes at the start of every top-level alternative
/// Note: Alternatives that contain ONLY \G are excluded - they just get removed
fn check_supported_g_nodes(
  ast: RegexNode,
  state: FirstPassState,
) -> FirstPassState {
  // Separate lone-\G alternatives from others
  let #(leading_gs, lone_g_count) =
    list.fold(ast.body, #([], 0), fn(acc, alt) {
      let #(gs, lone_count) = acc
      case is_lone_g_alternative(alt.body) {
        True -> #(gs, lone_count + 1)
        False -> {
          case get_leading_g(alt.body) {
            Ok(found_gs) -> #(list.append(gs, found_gs), lone_count)
            Error(Nil) -> #(gs, lone_count)
          }
        }
      }
    })

  // Count non-lone-G alternatives
  let non_lone_count = list.length(ast.body) - lone_g_count

  // Check if all non-lone alternatives have leading \G
  let has_alt_with_lead_g = leading_gs != []
  let has_alt_without_lead_g = list.length(leading_gs) < non_lone_count

  case has_alt_with_lead_g && !has_alt_without_lead_g && non_lone_count > 0 {
    True -> FirstPassState(..state, supported_g_nodes: leading_gs)
    False -> state
  }
}

/// Check if an alternative contains only \G (nothing else)
fn is_lone_g_alternative(elements: List(AlternativeElement)) -> Bool {
  case elements {
    [AssertionE(node)] -> node.kind == SearchStart
    _ -> False
  }
}

/// Get leading \G assertion from an alternative's elements
fn get_leading_g(
  elements: List(AlternativeElement),
) -> Result(List(AssertionNode), Nil) {
  case elements {
    [] -> Error(Nil)
    [first, ..] -> {
      case first {
        AssertionE(node) if node.kind == SearchStart -> Ok([node])
        GroupE(group) -> {
          // Recursively check group alternatives
          let gs =
            list.filter_map(group.body, fn(alt) { get_leading_g(alt.body) })
          case list.length(gs) == list.length(group.body) {
            True -> Ok(list.flatten(gs))
            False -> Error(Nil)
          }
        }
        CapturingGroupE(cg) -> {
          // Recursively check capturing group alternatives
          let gs = list.filter_map(cg.body, fn(alt) { get_leading_g(alt.body) })
          case list.length(gs) == list.length(cg.body) {
            True -> Ok(list.flatten(gs))
            False -> Error(Nil)
          }
        }
        _ ->
          case is_always_zero_length(first) {
            True -> get_leading_g(list.drop(elements, 1))
            False -> Error(Nil)
          }
      }
    }
  }
}

/// Check if an element is always zero length
fn is_always_zero_length(element: AlternativeElement) -> Bool {
  case element {
    AssertionE(_) -> True
    DirectiveE(_) -> True
    LookaroundAssertionE(_) -> True
    _ -> False
  }
}

// ============================================================================
// Alternative Transformation
// ============================================================================

/// Transform a list of alternatives
/// is_top_level: True only for the root Regex body, False for groups/lookarounds
fn transform_alternatives(
  alts: List(AlternativeNode),
  state: FirstPassState,
  alt_index: Int,
  is_top_level: Bool,
) -> #(List(AlternativeNode), FirstPassState) {
  // First pass: collect flag directives from each alternative for propagation
  let flag_directives_by_index = collect_flag_directives(alts, 0, dict.new())
  transform_alternatives_loop(
    alts,
    [],
    state,
    alt_index,
    flag_directives_by_index,
    is_top_level,
  )
}

/// Collect flag directives from alternatives for propagation to subsequent siblings
fn collect_flag_directives(
  alts: List(AlternativeNode),
  index: Int,
  acc: Dict(Int, List(FlagGroupModifiers)),
) -> Dict(Int, List(FlagGroupModifiers)) {
  case alts {
    [] -> acc
    [alt, ..rest] -> {
      // Find flag directives in this alternative
      let flag_directives =
        list.filter_map(alt.body, fn(elem) {
          case elem {
            DirectiveE(dir) -> {
              case dir.kind {
                ast_types.Flags -> {
                  case dir.flags {
                    Some(flags) -> Ok(flags)
                    None -> Error(Nil)
                  }
                }
                _ -> Error(Nil)
              }
            }
            _ -> Error(Nil)
          }
        })
      // Register these flag directives for all subsequent alternatives
      let updated_acc = case flag_directives {
        [] -> acc
        _ -> {
          register_flags_for_siblings(rest, index + 1, flag_directives, acc)
        }
      }
      collect_flag_directives(rest, index + 1, updated_acc)
    }
  }
}

/// Register flag directives for sibling alternatives
fn register_flags_for_siblings(
  remaining: List(AlternativeNode),
  index: Int,
  flags: List(FlagGroupModifiers),
  acc: Dict(Int, List(FlagGroupModifiers)),
) -> Dict(Int, List(FlagGroupModifiers)) {
  case remaining {
    [] -> acc
    [_, ..rest] -> {
      let existing = result.unwrap(dict.get(acc, index), [])
      let updated = dict.insert(acc, index, list.append(existing, flags))
      register_flags_for_siblings(rest, index + 1, flags, updated)
    }
  }
}

fn transform_alternatives_loop(
  remaining: List(AlternativeNode),
  acc: List(AlternativeNode),
  state: FirstPassState,
  alt_index: Int,
  flag_directives_by_index: Dict(Int, List(FlagGroupModifiers)),
  is_top_level: Bool,
) -> #(List(AlternativeNode), FirstPassState) {
  case remaining {
    [] -> #(list.reverse(acc), state)
    [alt, ..rest] -> {
      let #(new_alt, new_state) =
        transform_alternative(
          alt,
          state,
          alt_index,
          list.length(remaining),
          is_top_level,
        )
      // Check if we need to wrap this alternative with inherited flag directives
      let final_alt = case dict.get(flag_directives_by_index, alt_index) {
        Ok(inherited_flags) if inherited_flags != [] -> {
          wrap_alternative_with_flags(new_alt, inherited_flags)
        }
        _ -> new_alt
      }
      transform_alternatives_loop(
        rest,
        [final_alt, ..acc],
        new_state,
        alt_index + 1,
        flag_directives_by_index,
        is_top_level,
      )
    }
  }
}

/// Wrap an alternative's content with a flag group for inherited flags
fn wrap_alternative_with_flags(
  alt: AlternativeNode,
  inherited_flags: List(FlagGroupModifiers),
) -> AlternativeNode {
  // Combine all inherited flags
  let combined_flags = combine_flag_modifiers(inherited_flags)
  case combined_flags {
    None -> alt
    Some(flags) -> {
      // Wrap the alternative's body in a flag group
      let flag_group =
        GroupNode(atomic: None, flags: Some(flags), body: [
          AlternativeNode(body: alt.body),
        ])
      AlternativeNode(body: [GroupE(flag_group)])
    }
  }
}

/// Combine multiple flag modifiers into one
fn combine_flag_modifiers(
  modifiers: List(FlagGroupModifiers),
) -> Option(FlagGroupModifiers) {
  case modifiers {
    [] -> None
    [single] -> clean_group_flags(single)
    [first, ..rest] -> {
      // Merge all modifiers - later ones take precedence
      let merged = list.fold(rest, first, merge_two_flag_modifiers)
      clean_group_flags(merged)
    }
  }
}

/// Merge two flag modifiers
fn merge_two_flag_modifiers(
  a: FlagGroupModifiers,
  b: FlagGroupModifiers,
) -> FlagGroupModifiers {
  FlagGroupModifiers(
    enable: merge_switches(a.enable, b.enable),
    disable: merge_switches(a.disable, b.disable),
  )
}

/// Merge two flag switches
fn merge_switches(
  a: Option(FlagGroupSwitches),
  b: Option(FlagGroupSwitches),
) -> Option(FlagGroupSwitches) {
  case a, b {
    None, None -> None
    Some(s), None -> Some(s)
    None, Some(s) -> Some(s)
    Some(s1), Some(s2) ->
      Some(FlagGroupSwitches(
        ignore_case: option.or(s2.ignore_case, s1.ignore_case),
        dot_all: option.or(s2.dot_all, s1.dot_all),
        extended: option.or(s2.extended, s1.extended),
      ))
  }
}

/// Transform a single alternative
fn transform_alternative(
  alt: AlternativeNode,
  state: FirstPassState,
  _alt_index: Int,
  _sibling_count: Int,
  is_top_level: Bool,
) -> #(AlternativeNode, FirstPassState) {
  // Special case: at the TOP level only, if the alternative contains only \G,
  // just remove it (leaving an empty alternative, without setting sticky or clip_search)
  // This does NOT apply to \G inside groups or lookarounds
  case is_top_level && is_lone_g_alternative(alt.body) {
    True -> #(AlternativeNode(body: []), state)
    False -> {
      let #(new_elements, new_state) = transform_elements(alt.body, state)
      #(AlternativeNode(body: new_elements), new_state)
    }
  }
}

// ============================================================================
// Element Transformation
// ============================================================================

/// Transform a list of elements
fn transform_elements(
  elements: List(AlternativeElement),
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  transform_elements_loop(elements, [], state)
}

fn transform_elements_loop(
  remaining: List(AlternativeElement),
  acc: List(AlternativeElement),
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  case remaining {
    [] -> #(list.reverse(acc), state)
    [elem, ..rest] -> {
      // Check if this is a flag directive - needs special handling
      case elem {
        DirectiveE(directive_node) -> {
          case directive_node.kind {
            ast_types.Flags -> {
              case directive_node.flags {
                Some(flags) -> {
                  // Update current_flags BEFORE transforming remaining elements
                  // so that capturing groups are registered with the correct flag context
                  let state_with_flags =
                    FirstPassState(
                      ..state,
                      current_flags: get_new_current_flags(
                        state.current_flags,
                        flags,
                      ),
                    )
                  // Transform remaining elements with updated flag context
                  let #(remaining_transformed, new_state) =
                    transform_elements(rest, state_with_flags)
                  // Wrap remaining elements in a flag group
                  let flag_group =
                    GroupNode(
                      atomic: None,
                      flags: clean_group_flags(flags),
                      body: [AlternativeNode(body: remaining_transformed)],
                    )
                  // Add the flag group to acc and return (rest already processed)
                  // Restore original current_flags in returned state
                  let final_state =
                    FirstPassState(
                      ..new_state,
                      current_flags: state.current_flags,
                    )
                  #(list.reverse([GroupE(flag_group), ..acc]), final_state)
                }
                None -> {
                  // No flags, just skip the directive
                  transform_elements_loop(rest, acc, state)
                }
              }
            }
            _ -> {
              // Other directive types - handle normally
              let #(new_elems, new_state) = transform_element(elem, state)
              transform_elements_loop(
                rest,
                list.append(list.reverse(new_elems), acc),
                new_state,
              )
            }
          }
        }
        _ -> {
          // Non-directive element - handle normally
          let #(new_elems, new_state) = transform_element(elem, state)
          transform_elements_loop(
            rest,
            list.append(list.reverse(new_elems), acc),
            new_state,
          )
        }
      }
    }
  }
}

/// Transform a single element
fn transform_element(
  element: AlternativeElement,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  case element {
    AbsenceFunctionE(node) -> transform_absence_function(node, state)
    AssertionE(node) -> transform_assertion(node, state)
    BackreferenceE(node) -> transform_backreference(node, state)
    CapturingGroupE(node) -> transform_capturing_group(node, state)
    CharacterE(_) -> #([element], state)
    CharacterClassE(node) -> transform_character_class(node, state)
    CharacterSetE(node) -> transform_character_set(node, state)
    DirectiveE(node) -> transform_directive(node, state)
    GroupE(node) -> transform_group(node, state)
    LookaroundAssertionE(node) -> transform_lookaround_assertion(node, state)
    NamedCalloutE(node) -> transform_named_callout(node, state)
    QuantifierE(node) -> transform_quantifier(node, state)
    SubroutineE(node) -> transform_subroutine(node, state)
  }
}

// ============================================================================
// Specific Transformations
// ============================================================================

/// Transform absence function (?~...) to lookaround pattern
fn transform_absence_function(
  node: AbsenceFunctionNode,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  case node.kind {
    Repeater -> {
      // Convert (?~...) to (?:(?:(?!...)\p{Any})*)
      let #(transformed_body, new_state) =
        transform_alternatives(node.body, state, 0, False)

      // Create the inner lookahead: (?!...)
      let inner_lookahead =
        LookaroundAssertionNode(
          kind: Lookahead,
          negate: True,
          body: transformed_body,
        )

      // Create \p{Any}
      let any_char =
        CharacterSetNode(
          kind: ast_types.Property,
          value: Some("Any"),
          negate: Some(False),
          variable_length: None,
        )

      // Create inner group: (?:(?!...)\p{Any})
      let inner_group =
        GroupNode(atomic: None, flags: None, body: [
          AlternativeNode(body: [
            LookaroundAssertionE(inner_lookahead),
            CharacterSetE(any_char),
          ]),
        ])

      // Create quantifier: (?:(?!...)\p{Any})*
      let quantified =
        QuantifierNode(
          kind: ast_types.Greedy,
          min: 0,
          max: ast_types.quantifier_max_infinity,
          body: ast_types.GroupQ(inner_group),
        )

      // Create outer group: (?:(?:(?!...)\p{Any})*)
      let outer_group =
        GroupNode(atomic: None, flags: None, body: [
          AlternativeNode(body: [QuantifierE(quantified)]),
        ])

      #([GroupE(outer_group)], new_state)
    }
  }
}

/// Transform assertion
fn transform_assertion(
  node: AssertionNode,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  case node.kind {
    TextSegmentBoundary ->
      // Not yet supported
      #([AssertionE(node)], state)

    LineEnd -> {
      // Convert $ to lookahead for \n?\z (same as \Z)
      // Structure: (?=\n?\z) where \n? is optional newline before end of string
      let lookahead =
        LookaroundAssertionNode(kind: Lookahead, negate: False, body: [
          AlternativeNode(body: [
            QuantifierE(QuantifierNode(
              kind: ast_types.Greedy,
              min: 0,
              max: 1,
              body: ast_types.CharacterQ(CharacterNode(value: 10)),
            )),
            AssertionE(AssertionNode(kind: StringEnd, negate: None)),
          ]),
        ])
      #([LookaroundAssertionE(lookahead)], state)
    }

    LineStart -> {
      // Convert ^ to lookbehind for \A or \n(?!\z)
      // Simplified: just use lookbehind for start or after newline
      let lookbehind =
        LookaroundAssertionNode(kind: Lookbehind, negate: False, body: [
          AlternativeNode(body: [
            AssertionE(AssertionNode(kind: StringStart, negate: None)),
          ]),
          AlternativeNode(body: [
            CharacterE(CharacterNode(value: 10)),
            // \n
            LookaroundAssertionE(
              LookaroundAssertionNode(kind: Lookahead, negate: True, body: [
                AlternativeNode(body: [
                  AssertionE(AssertionNode(kind: StringEnd, negate: None)),
                ]),
              ]),
            ),
          ]),
        ])
      #([LookaroundAssertionE(lookbehind)], state)
    }

    SearchStart -> {
      // Check if this is a supported \G
      let is_supported = list.contains(state.supported_g_nodes, node)
      case is_supported {
        True -> {
          // Remove the \G - sticky flag will be set based on supported_g_nodes being non-empty
          // Strategy stays None because supported \G just uses sticky mode
          #([], state)
        }
        False -> {
          case state.avoid_subclass {
            True ->
              // Can't emulate \G without subclass
              #([AssertionE(node)], state)
            False -> {
              // Replace with string_start and set strategy to clip_search
              #(
                [
                  AssertionE(AssertionNode(kind: StringStart, negate: None)),
                ],
                FirstPassState(..state, strategy: Some(ClipSearch)),
              )
            }
          }
        }
      }
    }

    StringEnd | StringStart -> #([AssertionE(node)], state)

    StringEndNewline -> {
      // Convert \Z to lookahead for \n?\z
      let lookahead =
        LookaroundAssertionNode(kind: Lookahead, negate: False, body: [
          AlternativeNode(body: [
            QuantifierE(QuantifierNode(
              kind: ast_types.Greedy,
              min: 0,
              max: 1,
              body: ast_types.CharacterQ(CharacterNode(value: 10)),
            )),
            AssertionE(AssertionNode(kind: StringEnd, negate: None)),
          ]),
        ])
      #([LookaroundAssertionE(lookahead)], state)
    }

    WordBoundary -> {
      let negate = option.unwrap(node.negate, False)
      case state.word_is_ascii || state.ascii_word_boundaries {
        True -> #([AssertionE(node)], state)
        False -> {
          // Transform to Unicode-aware word boundary using lookarounds
          // \b = (?:(?<=[word])(?![word])|(?<![word])(?=[word]))
          // \B = (?:(?<=[word])(?=[word])|(?<![word])(?![word]))
          let word_class = make_word_char_class()

          case negate {
            False -> {
              // \b: word boundary
              // Alt1: (?<=[word])(?![word]) - at end of word
              // Alt2: (?<![word])(?=[word]) - at start of word
              let alt1 =
                AlternativeNode(body: [
                  LookaroundAssertionE(
                    LookaroundAssertionNode(
                      kind: Lookbehind,
                      negate: False,
                      body: [
                        AlternativeNode(body: [CharacterClassE(word_class)]),
                      ],
                    ),
                  ),
                  LookaroundAssertionE(
                    LookaroundAssertionNode(
                      kind: Lookahead,
                      negate: True,
                      body: [
                        AlternativeNode(body: [CharacterClassE(word_class)]),
                      ],
                    ),
                  ),
                ])
              let alt2 =
                AlternativeNode(body: [
                  LookaroundAssertionE(
                    LookaroundAssertionNode(
                      kind: Lookbehind,
                      negate: True,
                      body: [
                        AlternativeNode(body: [CharacterClassE(word_class)]),
                      ],
                    ),
                  ),
                  LookaroundAssertionE(
                    LookaroundAssertionNode(
                      kind: Lookahead,
                      negate: False,
                      body: [
                        AlternativeNode(body: [CharacterClassE(word_class)]),
                      ],
                    ),
                  ),
                ])
              let group =
                GroupNode(atomic: None, flags: None, body: [alt1, alt2])
              #([GroupE(group)], state)
            }
            True -> {
              // \B: non-word boundary
              // Alt1: (?<=[word])(?=[word]) - inside a word
              // Alt2: (?<![word])(?![word]) - outside a word
              let alt1 =
                AlternativeNode(body: [
                  LookaroundAssertionE(
                    LookaroundAssertionNode(
                      kind: Lookbehind,
                      negate: False,
                      body: [
                        AlternativeNode(body: [CharacterClassE(word_class)]),
                      ],
                    ),
                  ),
                  LookaroundAssertionE(
                    LookaroundAssertionNode(
                      kind: Lookahead,
                      negate: False,
                      body: [
                        AlternativeNode(body: [CharacterClassE(word_class)]),
                      ],
                    ),
                  ),
                ])
              let alt2 =
                AlternativeNode(body: [
                  LookaroundAssertionE(
                    LookaroundAssertionNode(
                      kind: Lookbehind,
                      negate: True,
                      body: [
                        AlternativeNode(body: [CharacterClassE(word_class)]),
                      ],
                    ),
                  ),
                  LookaroundAssertionE(
                    LookaroundAssertionNode(
                      kind: Lookahead,
                      negate: True,
                      body: [
                        AlternativeNode(body: [CharacterClassE(word_class)]),
                      ],
                    ),
                  ),
                ])
              let group =
                GroupNode(atomic: None, flags: None, body: [alt1, alt2])
              #([GroupE(group)], state)
            }
          }
        }
      }
    }
  }
}

/// Transform backreference
fn transform_backreference(
  node: BackreferenceNode,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  case node.ref {
    NamedRef(name) -> {
      case is_valid_js_group_name(name) {
        True -> #([BackreferenceE(node)], state)
        False -> {
          let #(js_name, new_map) =
            get_or_insert_group_name(name, state.js_group_name_map)
          let new_node = BackreferenceNode(..node, ref: NamedRef(js_name))
          #(
            [BackreferenceE(new_node)],
            FirstPassState(..state, js_group_name_map: new_map),
          )
        }
      }
    }
    NumberedRef(_) -> #([BackreferenceE(node)], state)
  }
}

/// Transform capturing group
fn transform_capturing_group(
  node: CapturingGroupNode,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  // Assign transform_id if not present
  let #(node_with_id, state_with_id) = case node.transform_id {
    Some(_) -> #(node, state)
    None -> {
      let new_id = state.next_transform_id
      #(
        CapturingGroupNode(..node, transform_id: Some(new_id)),
        FirstPassState(..state, next_transform_id: state.next_transform_id + 1),
      )
    }
  }

  // Transform name if needed
  let #(new_name, new_js_map) = case node.name {
    Some(name) ->
      case is_valid_js_group_name(name) {
        True -> #(Some(name), state_with_id.js_group_name_map)
        False -> {
          let #(js_name, updated_map) =
            get_or_insert_group_name(name, state_with_id.js_group_name_map)
          #(Some(js_name), updated_map)
        }
      }
    None -> #(None, state_with_id.js_group_name_map)
  }

  let state2 = FirstPassState(..state_with_id, js_group_name_map: new_js_map)

  // Transform body FIRST (so nested groups get their transform_ids)
  let #(new_body, state_after_body) =
    transform_alternatives(node_with_id.body, state2, 0, False)

  // Create the fully transformed node
  let new_node =
    CapturingGroupNode(..node_with_id, name: new_name, body: new_body)

  // Register in subroutine ref map AFTER transforming body
  // This ensures the entry has the fully transformed node with all nested IDs
  let entry =
    types.SubroutineRefEntry(
      group: new_node,
      flags: state_with_id.current_flags,
    )
  let new_map =
    dict.insert(
      state_after_body.subroutine_ref_map,
      NumberedKey(node.number),
      entry,
    )
  let new_map2 = case node.name {
    Some(name) -> dict.insert(new_map, NamedKey(name), entry)
    None -> new_map
  }

  let final_state =
    FirstPassState(..state_after_body, subroutine_ref_map: new_map2)

  #([CapturingGroupE(new_node)], final_state)
}

/// Transform character class
fn transform_character_class(
  node: CharacterClassNode,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  let #(new_body, new_state) = transform_cc_elements(node.body, state)
  let new_node = CharacterClassNode(..node, body: new_body)
  #([CharacterClassE(new_node)], new_state)
}

/// Transform character class elements
fn transform_cc_elements(
  elements: List(CharacterClassElement),
  state: FirstPassState,
) -> #(List(CharacterClassElement), FirstPassState) {
  transform_cc_elements_loop(elements, [], state)
}

fn transform_cc_elements_loop(
  remaining: List(CharacterClassElement),
  acc: List(CharacterClassElement),
  state: FirstPassState,
) -> #(List(CharacterClassElement), FirstPassState) {
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
  state: FirstPassState,
) -> #(CharacterClassElement, FirstPassState) {
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
    CharacterSetCCE(node) -> {
      let #(new_elems, new_state) = transform_character_set(node, state)
      case new_elems {
        [CharacterSetE(new_node)] -> #(CharacterSetCCE(new_node), new_state)
        [CharacterClassE(cc_node)] -> #(CharacterClassCCE(cc_node), new_state)
        _ -> #(element, new_state)
      }
    }
  }
}

/// Transform character set
fn transform_character_set(
  node: CharacterSetNode,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  let negate = option.unwrap(node.negate, False)

  case node.kind {
    // Flag D with \d
    ast_types.Digit -> {
      case state.digit_is_ascii {
        True -> #([CharacterSetE(node)], state)
        False -> {
          // Use \p{Nd} for Unicode digit
          let new_node =
            CharacterSetNode(
              kind: ast_types.Property,
              value: Some("Nd"),
              negate: node.negate,
              variable_length: None,
            )
          #([CharacterSetE(new_node)], state)
        }
      }
    }

    // Flag S with \s
    ast_types.Space -> {
      case state.space_is_ascii {
        True -> #([CharacterSetE(node)], state)
        False -> {
          // Use \p{space} for Unicode space
          let new_node =
            CharacterSetNode(
              kind: ast_types.Property,
              value: Some("space"),
              negate: node.negate,
              variable_length: None,
            )
          #([CharacterSetE(new_node)], state)
        }
      }
    }

    // Flag W with \w
    ast_types.Word -> {
      case state.word_is_ascii {
        True -> #([CharacterSetE(node)], state)
        False -> {
          // Use Unicode word character pattern
          // [\p{L}\p{M}\p{N}\p{Pc}]
          let new_node =
            CharacterClassNode(kind: Union, negate: negate, body: [
              CharacterSetCCE(CharacterSetNode(
                kind: ast_types.Property,
                value: Some("L"),
                negate: Some(False),
                variable_length: None,
              )),
              CharacterSetCCE(CharacterSetNode(
                kind: ast_types.Property,
                value: Some("M"),
                negate: Some(False),
                variable_length: None,
              )),
              CharacterSetCCE(CharacterSetNode(
                kind: ast_types.Property,
                value: Some("N"),
                negate: Some(False),
                variable_length: None,
              )),
              CharacterSetCCE(CharacterSetNode(
                kind: ast_types.Property,
                value: Some("Pc"),
                negate: Some(False),
                variable_length: None,
              )),
            ])
          #([CharacterClassE(new_node)], state)
        }
      }
    }

    ast_types.Any -> {
      // \O - convert to \p{Any}
      let new_node =
        CharacterSetNode(
          kind: ast_types.Property,
          value: Some("Any"),
          negate: node.negate,
          variable_length: None,
        )
      #([CharacterSetE(new_node)], state)
    }

    ast_types.Dot -> #([CharacterSetE(node)], state)

    ast_types.Hex -> {
      // \h - convert to \p{AHex}
      let new_node =
        CharacterSetNode(
          kind: ast_types.Property,
          value: Some("AHex"),
          negate: node.negate,
          variable_length: None,
        )
      #([CharacterSetE(new_node)], state)
    }

    ast_types.Newline -> {
      // \R or \N (newline character set)
      // \N (negated) -> [^\n] - character class with just newline
      // \R (not negated) -> (?>\r\n?|[\n\v\f\x85\u2028\u2029])
      case negate {
        True -> {
          // \N -> [^\n]
          let newline_char = CharacterNode(value: 10)
          let cc =
            CharacterClassNode(kind: ast_types.Union, negate: True, body: [
              CharacterCCE(newline_char),
            ])
          #([CharacterClassE(cc)], state)
        }
        False -> {
          // \R -> atomic group (?>\r\n?|[\n\v\f\x85\u2028\u2029])
          // Create a group with alternatives for newline sequences
          let cr_node = CharacterNode(value: 13)
          // \r (carriage return)
          let lf_node = CharacterNode(value: 10)
          // \n (line feed)
          let vt_node = CharacterNode(value: 11)
          // \v (vertical tab)
          let ff_node = CharacterNode(value: 12)
          // \f (form feed)
          let nel_node = CharacterNode(value: 0x85)
          // \x85 (NEL)
          let ls_node = CharacterNode(value: 0x2028)
          // U+2028 (line separator)
          let ps_node = CharacterNode(value: 0x2029)
          // U+2029 (paragraph separator)

          // Alternative 1: \r\n? (carriage return optionally followed by line feed)
          let cr_with_optional_lf =
            AlternativeNode(body: [
              CharacterE(cr_node),
              QuantifierE(QuantifierNode(
                body: ast_types.CharacterQ(lf_node),
                min: 0,
                max: 1,
                kind: Greedy,
              )),
            ])

          // Alternative 2: [\n\v\f\x85\u2028\u2029]
          let other_newlines =
            CharacterClassNode(kind: ast_types.Union, negate: False, body: [
              CharacterCCE(lf_node),
              CharacterCCE(vt_node),
              CharacterCCE(ff_node),
              CharacterCCE(nel_node),
              CharacterCCE(ls_node),
              CharacterCCE(ps_node),
            ])

          let alt2 = AlternativeNode(body: [CharacterClassE(other_newlines)])

          // Wrap in atomic group
          let group =
            GroupNode(atomic: Some(True), flags: None, body: [
              cr_with_optional_lf,
              alt2,
            ])

          #([GroupE(group)], state)
        }
      }
    }

    ast_types.Posix -> {
      // POSIX class - convert to Unicode property or character class
      let value = option.unwrap(node.value, "")
      convert_posix_to_ast(value, negate, state)
    }

    ast_types.Property -> #([CharacterSetE(node)], state)

    ast_types.TextSegment -> {
      // \X - grapheme cluster
      case state.accuracy {
        StrictAccuracy -> #([CharacterSetE(node)], state)
        DefaultAccuracy -> {
          // Approximate grapheme cluster
          #([CharacterSetE(node)], state)
        }
      }
    }
  }
}

/// Convert POSIX class to appropriate AST structure
/// Based on JS PosixClassMap:
/// - alnum → [\p{Alpha}\p{Nd}]
/// - alpha → \p{Alpha}
/// - ascii → \p{ASCII}
/// - blank → [\p{Zs}\t]
/// - cntrl → \p{Cc}
/// - digit → \p{Nd}
/// - graph → complex intersection (simplified)
/// - lower → \p{Lower}
/// - print → complex (simplified)
/// - punct → [\p{P}\p{S}]
/// - space → \p{space}
/// - upper → \p{Upper}
/// - word → [\p{Alpha}\p{M}\p{Nd}\p{Pc}]
/// - xdigit → \p{AHex}
fn convert_posix_to_ast(
  value: String,
  negate: Bool,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  case value {
    // Simple single-property cases
    "alpha" -> {
      let node = make_property_node("Alpha", negate)
      #([CharacterSetE(node)], state)
    }
    "ascii" -> {
      let node = make_property_node("ASCII", negate)
      #([CharacterSetE(node)], state)
    }
    "cntrl" -> {
      let node = make_property_node("Cc", negate)
      #([CharacterSetE(node)], state)
    }
    "digit" -> {
      let node = make_property_node("Nd", negate)
      #([CharacterSetE(node)], state)
    }
    "lower" -> {
      let node = make_property_node("Lower", negate)
      #([CharacterSetE(node)], state)
    }
    "space" -> {
      let node = make_property_node("space", negate)
      #([CharacterSetE(node)], state)
    }
    "upper" -> {
      let node = make_property_node("Upper", negate)
      #([CharacterSetE(node)], state)
    }
    "xdigit" -> {
      let node = make_property_node("AHex", negate)
      #([CharacterSetE(node)], state)
    }

    // Multi-property cases - create character class
    "alnum" -> {
      // [\p{Alpha}\p{Nd}]
      let cc =
        CharacterClassNode(kind: Union, negate: negate, body: [
          CharacterSetCCE(make_property_node("Alpha", False)),
          CharacterSetCCE(make_property_node("Nd", False)),
        ])
      #([CharacterClassE(cc)], state)
    }
    "blank" -> {
      // [\p{Zs}\t]
      let cc =
        CharacterClassNode(kind: Union, negate: negate, body: [
          CharacterSetCCE(make_property_node("Zs", False)),
          CharacterCCE(CharacterNode(value: 9)),
        ])
      #([CharacterClassE(cc)], state)
    }
    "graph" -> {
      // [\P{space}&&\P{Cc}&&\P{Cn}&&\P{Cs}] - complex intersection
      // Simplified: use intersection if supported, otherwise approximate
      let cc =
        CharacterClassNode(kind: Intersection, negate: negate, body: [
          CharacterSetCCE(make_property_node("space", True)),
          CharacterSetCCE(make_property_node("Cc", True)),
          CharacterSetCCE(make_property_node("Cn", True)),
          CharacterSetCCE(make_property_node("Cs", True)),
        ])
      #([CharacterClassE(cc)], state)
    }
    "print" -> {
      // [[\P{space}&&\P{Cc}&&\P{Cn}&&\P{Cs}]\p{Zs}] - union of graph + Zs
      // Simplified: union with graph intersection and Zs
      let graph_cc =
        CharacterClassNode(kind: Intersection, negate: False, body: [
          CharacterSetCCE(make_property_node("space", True)),
          CharacterSetCCE(make_property_node("Cc", True)),
          CharacterSetCCE(make_property_node("Cn", True)),
          CharacterSetCCE(make_property_node("Cs", True)),
        ])
      let cc =
        CharacterClassNode(kind: Union, negate: negate, body: [
          CharacterClassCCE(graph_cc),
          CharacterSetCCE(make_property_node("Zs", False)),
        ])
      #([CharacterClassE(cc)], state)
    }
    "punct" -> {
      // [\p{P}\p{S}]
      let cc =
        CharacterClassNode(kind: Union, negate: negate, body: [
          CharacterSetCCE(make_property_node("P", False)),
          CharacterSetCCE(make_property_node("S", False)),
        ])
      #([CharacterClassE(cc)], state)
    }
    "word" -> {
      // [\p{Alpha}\p{M}\p{Nd}\p{Pc}]
      let cc =
        CharacterClassNode(kind: Union, negate: negate, body: [
          CharacterSetCCE(make_property_node("Alpha", False)),
          CharacterSetCCE(make_property_node("M", False)),
          CharacterSetCCE(make_property_node("Nd", False)),
          CharacterSetCCE(make_property_node("Pc", False)),
        ])
      #([CharacterClassE(cc)], state)
    }

    // Default - use as property name
    _ -> {
      let node = make_property_node(value, negate)
      #([CharacterSetE(node)], state)
    }
  }
}

/// Helper to create a property CharacterSetNode
fn make_property_node(prop_value: String, negate: Bool) -> CharacterSetNode {
  CharacterSetNode(
    kind: ast_types.Property,
    value: Some(prop_value),
    negate: Some(negate),
    variable_length: None,
  )
}

/// Helper to create the default word character class [\p{L}\p{M}\p{N}\p{Pc}]
fn make_word_char_class() -> CharacterClassNode {
  CharacterClassNode(kind: Union, negate: False, body: [
    CharacterSetCCE(make_property_node("L", False)),
    CharacterSetCCE(make_property_node("M", False)),
    CharacterSetCCE(make_property_node("N", False)),
    CharacterSetCCE(make_property_node("Pc", False)),
  ])
}

/// Transform directive
fn transform_directive(
  node: DirectiveNode,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  case node.kind {
    ast_types.Flags -> {
      case node.flags {
        None -> #([], state)
        Some(flags) -> {
          // Create a flag group for remaining elements
          let group = GroupNode(atomic: None, flags: Some(flags), body: [])
          #([GroupE(group)], state)
        }
      }
    }
    ast_types.Keep -> {
      // \K - convert to lookbehind
      // This is complex and requires context about previous siblings
      #([DirectiveE(node)], state)
    }
  }
}

/// Transform group
fn transform_group(
  node: GroupNode,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  // Clean up flags
  let new_flags = case node.flags {
    None -> None
    Some(flags) -> clean_group_flags(flags)
  }

  // Update current_flags if this group modifies flags
  let state_with_flags = case node.flags {
    None -> state
    Some(flags) -> {
      let new_current_flags = get_new_current_flags(state.current_flags, flags)
      FirstPassState(..state, current_flags: new_current_flags)
    }
  }

  let #(new_body, new_state) =
    transform_alternatives(node.body, state_with_flags, 0, False)

  // Restore the original current_flags after processing children
  let final_state =
    FirstPassState(..new_state, current_flags: state.current_flags)

  let new_node = GroupNode(..node, flags: new_flags, body: new_body)
  #([GroupE(new_node)], final_state)
}

/// Clean up group flags (remove extended flag)
fn clean_group_flags(flags: FlagGroupModifiers) -> Option(FlagGroupModifiers) {
  let clean_enable = case flags.enable {
    None -> None
    Some(switches) -> {
      let cleaned =
        FlagGroupSwitches(
          ignore_case: switches.ignore_case,
          dot_all: switches.dot_all,
          extended: None,
        )
      case cleaned.ignore_case, cleaned.dot_all {
        None, None -> None
        _, _ -> Some(cleaned)
      }
    }
  }

  let clean_disable = case flags.disable {
    None -> None
    Some(switches) -> {
      let cleaned =
        FlagGroupSwitches(
          ignore_case: switches.ignore_case,
          dot_all: switches.dot_all,
          extended: None,
        )
      case cleaned.ignore_case, cleaned.dot_all {
        None, None -> None
        _, _ -> Some(cleaned)
      }
    }
  }

  case clean_enable, clean_disable {
    None, None -> None
    _, _ ->
      Some(FlagGroupModifiers(enable: clean_enable, disable: clean_disable))
  }
}

/// Transform lookaround assertion
fn transform_lookaround_assertion(
  node: LookaroundAssertionNode,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  let state2 = case node.kind {
    Lookbehind -> FirstPassState(..state, passed_lookbehind: True)
    Lookahead -> state
  }

  let #(new_body, final_state) =
    transform_alternatives(node.body, state2, 0, False)
  let new_node = LookaroundAssertionNode(..node, body: new_body)
  #([LookaroundAssertionE(new_node)], final_state)
}

/// Transform named callout
fn transform_named_callout(
  node: NamedCalloutNode,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  case node.kind {
    ast_types.Fail -> {
      // (*FAIL) -> (?!)
      let lookahead =
        LookaroundAssertionNode(kind: Lookahead, negate: True, body: [])
      #([LookaroundAssertionE(lookahead)], state)
    }
    _ -> #([NamedCalloutE(node)], state)
  }
}

/// Transform quantifier
fn transform_quantifier(
  node: QuantifierNode,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  let #(new_body, new_state) = transform_quantifiable(node.body, state)

  // Handle nested quantifiers by wrapping the INNER quantifier in a group
  // The outer quantifier keeps its original min/max
  let final_body = case new_body {
    ast_types.QuantifierQ(inner_q) -> {
      // Wrap the inner quantifier in a group, preserving the outer quantifier's bounds
      let group =
        GroupNode(atomic: None, flags: None, body: [
          AlternativeNode(body: [QuantifierE(inner_q)]),
        ])
      ast_types.GroupQ(group)
    }
    _ -> new_body
  }

  let new_node = QuantifierNode(..node, body: final_body)
  #([QuantifierE(new_node)], new_state)
}

/// Transform quantifiable node
fn transform_quantifiable(
  node: QuantifiableNode,
  state: FirstPassState,
) -> #(QuantifiableNode, FirstPassState) {
  case node {
    ast_types.AbsenceFunctionQ(af) -> {
      let #(elems, new_state) = transform_absence_function(af, state)
      case elems {
        [GroupE(g)] -> #(ast_types.GroupQ(g), new_state)
        _ -> #(node, new_state)
      }
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
      let #(elems, new_state) = transform_character_class(cc, state)
      case elems {
        [CharacterClassE(new_cc)] -> #(
          ast_types.CharacterClassQ(new_cc),
          new_state,
        )
        _ -> #(node, new_state)
      }
    }
    ast_types.CharacterSetQ(cs) -> {
      let #(elems, new_state) = transform_character_set(cs, state)
      case elems {
        [CharacterSetE(new_cs)] -> #(ast_types.CharacterSetQ(new_cs), new_state)
        [CharacterClassE(new_cc)] -> #(
          ast_types.CharacterClassQ(new_cc),
          new_state,
        )
        _ -> #(node, new_state)
      }
    }
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
        _ -> #(node, new_state)
      }
    }
  }
}

/// Transform subroutine
fn transform_subroutine(
  node: SubroutineNode,
  state: FirstPassState,
) -> #(List(AlternativeElement), FirstPassState) {
  case node.ref {
    NamedSubroutineRef(name) -> {
      // Look up the referenced group to get its number
      let state_with_target = case
        dict.get(state.subroutine_ref_map, NamedKey(name))
      {
        Ok(entry) ->
          FirstPassState(
            ..state,
            subroutine_target_numbers: set.insert(
              state.subroutine_target_numbers,
              entry.group.number,
            ),
          )
        Error(_) -> state
      }
      case is_valid_js_group_name(name) {
        True -> #([SubroutineE(node)], state_with_target)
        False -> {
          let #(js_name, new_map) =
            get_or_insert_group_name(name, state_with_target.js_group_name_map)
          let new_node =
            SubroutineNode(
              ref: NamedSubroutineRef(js_name),
              is_recursive: node.is_recursive,
            )
          #(
            [SubroutineE(new_node)],
            FirstPassState(..state_with_target, js_group_name_map: new_map),
          )
        }
      }
    }
    NumberedSubroutineRef(n) -> {
      // Track this group number as a subroutine target
      let new_targets = set.insert(state.subroutine_target_numbers, n)
      #(
        [SubroutineE(node)],
        FirstPassState(..state, subroutine_target_numbers: new_targets),
      )
    }
  }
}

// ============================================================================
// Flag Transformation
// ============================================================================

/// Transform flags node (remove Onig-specific flags)
fn transform_flags(flags: FlagsNode) -> FlagsNode {
  FlagsNode(
    ignore_case: flags.ignore_case,
    dot_all: flags.dot_all,
    extended: False,
    digit_is_ascii: False,
    posix_is_ascii: False,
    space_is_ascii: False,
    word_is_ascii: False,
    text_segment_mode: None,
  )
}
