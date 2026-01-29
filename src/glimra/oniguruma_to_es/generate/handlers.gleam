//// Node handlers for the generate module.
//// Each handler pushes work items to process children, then control items to combine results.

import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/set
import gleam/string
import glimra/oniguruma_parser/parser/ast_types.{
  type AlternativeElement, type AlternativeNode, type AssertionNode,
  type BackreferenceNode, type CapturingGroupNode, type CharacterClassElement,
  type CharacterClassNode, type CharacterNode, type CharacterSetNode,
  type GroupNode, type LookaroundAssertionKind, type LookaroundAssertionNode,
  type QuantifiableNode, type QuantifierNode, type SubroutineNode,
  AbsenceFunctionQ, AssertionE, BackreferenceE, BackreferenceQ, CapturingGroupE,
  CapturingGroupQ, CharacterCCE, CharacterClassCCE, CharacterClassE,
  CharacterClassQ, CharacterClassRangeCCE, CharacterE, CharacterQ,
  CharacterSetCCE, CharacterSetE, CharacterSetQ, Digit, Dot, GroupE, GroupQ,
  Intersection, Lookahead, LookaroundAssertionE, Lookbehind, NamedRef,
  NamedSubroutineRef, NumberedRef, NumberedSubroutineRef, Property, QuantifierE,
  QuantifierQ, StringEnd, StringStart, SubroutineE, SubroutineQ, Union, Word,
  WordBoundary,
}
import glimra/oniguruma_to_es/generate/helpers
import glimra/oniguruma_to_es/generate/types.{
  type CaptureData, type GenWorkItem, type GenerateState, CaptureData,
  CurrentFlags, JoinResults, PopFlags, ProcessCharClassElement, ProcessElement,
  ProcessQuantifierBody, PushFlags, PushResult, RecordCapture, SetInCharClass,
  SetInQuantifierBody, SetLastNodeWasBackref, WrapResult,
}
import glimra/oniguruma_to_es/generate/unicode_case
import glimra/oniguruma_to_es/transform/types as transform_types

// ============================================================================
// Alternative Handler
// ============================================================================

/// Push work items for an alternative node
pub fn push_alternative(
  node: AlternativeNode,
  state: GenerateState,
) -> #(List(GenWorkItem), GenerateState) {
  let body = node.body
  let count = list.length(body)

  case count {
    0 -> {
      // Empty alternative - push empty result
      #([PushResult("")], state)
    }
    _ -> {
      // Push work items for each element, then join them
      // Elements first, then control items
      let element_items =
        body
        |> list.map(fn(el) { ProcessElement(el) })

      let items = list.append(element_items, [JoinResults(count, "")])
      #(items, state)
    }
  }
}

// ============================================================================
// Assertion Handler
// ============================================================================

/// Push work items for an assertion node
pub fn push_assertion(
  node: AssertionNode,
  state: GenerateState,
) -> Result(#(List(GenWorkItem), GenerateState), String) {
  case node.kind {
    StringEnd -> Ok(#([PushResult("$")], state))
    StringStart -> Ok(#([PushResult("^")], state))
    WordBoundary -> {
      let result = case node.negate {
        option.Some(True) -> "\\B"
        _ -> "\\b"
      }
      Ok(#([PushResult(result)], state))
    }
    _ -> {
      // Other assertion kinds are not expected in transformer output
      Error(
        "Unexpected assertion kind: " <> assertion_kind_to_string(node.kind),
      )
    }
  }
}

fn assertion_kind_to_string(kind: ast_types.AssertionKind) -> String {
  case kind {
    ast_types.LineEnd -> "line_end"
    ast_types.LineStart -> "line_start"
    ast_types.SearchStart -> "search_start"
    StringEnd -> "string_end"
    ast_types.StringEndNewline -> "string_end_newline"
    StringStart -> "string_start"
    ast_types.TextSegmentBoundary -> "text_segment_boundary"
    WordBoundary -> "word_boundary"
  }
}

// ============================================================================
// Backreference Handler
// ============================================================================

/// Push work items for a backreference node
pub fn push_backreference(
  node: BackreferenceNode,
  state: GenerateState,
) -> Result(#(List(GenWorkItem), GenerateState), String) {
  case node.ref {
    NamedRef(_) -> Error("Unexpected named backref in transformed AST")
    NumberedRef(ref) -> {
      // Check case sensitivity in strict mode
      case
        !state.use_flag_mods
        && state.accuracy == transform_types.StrictAccuracy
        && state.current_flags.ignore_case
      {
        True -> {
          // Need to check if the capture was case-sensitive
          // If so, and we're in case-insensitive context, error in strict mode
          // This is handled in the main generate loop after capture is recorded
          Ok(#(
            [
              SetLastNodeWasBackref(True),
              PushResult("\\" <> int.to_string(ref)),
            ],
            state,
          ))
        }
        False ->
          Ok(#(
            [
              SetLastNodeWasBackref(True),
              PushResult("\\" <> int.to_string(ref)),
            ],
            state,
          ))
      }
    }
  }
}

// ============================================================================
// Capturing Group Handler
// ============================================================================

/// Push work items for a capturing group node
pub fn push_capturing_group(
  node: CapturingGroupNode,
  state: GenerateState,
) -> #(List(GenWorkItem), GenerateState) {
  let body = node.body
  let count = list.length(body)

  // Build capture data
  let capture_data = build_capture_data(node, state)

  // Build prefix based on name
  let prefix = case node.name {
    option.Some(name) -> "(?<" <> name <> ">"
    option.None -> "("
  }

  // Push work items: record capture, process alternatives, join, wrap
  // Work items are processed in order (first item first), so:
  // 1. RecordCapture first
  // 2. Element items for all alternatives
  // 3. JoinResults for each alternative
  // 4. JoinResults for all alternatives with "|"
  // 5. WrapResult last
  let alt_items =
    body
    |> list.map(fn(alt) {
      // For each alternative, push its elements then join
      let el_count = list.length(alt.body)
      let el_items =
        alt.body
        |> list.map(fn(el) { ProcessElement(el) })
      list.append(el_items, [JoinResults(el_count, "")])
    })
    |> list.flatten

  let items =
    [RecordCapture(node.number, capture_data)]
    |> list.append(alt_items)
    |> list.append([JoinResults(count, "|"), WrapResult(prefix, ")")])

  #(items, state)
}

fn build_capture_data(
  node: CapturingGroupNode,
  state: GenerateState,
) -> CaptureData {
  let ignore_case = state.current_flags.ignore_case

  // Check if this capture is from an expanded subroutine
  let origin = find_origin(node, state.origin_map)

  case origin {
    option.Some(origin_node) -> {
      // All captures from/within expanded subroutines are marked as hidden
      let transfer_to = case node.number > origin_node.number {
        True -> option.Some(origin_node.number)
        False -> option.None
      }
      CaptureData(
        ignore_case: ignore_case,
        hidden: True,
        transfer_to: transfer_to,
      )
    }
    option.None ->
      CaptureData(
        ignore_case: ignore_case,
        hidden: False,
        transfer_to: option.None,
      )
  }
}

fn find_origin(
  node: CapturingGroupNode,
  origin_map: List(#(CapturingGroupNode, CapturingGroupNode)),
) -> Option(CapturingGroupNode) {
  // Find node in origin_map by comparing transform_id
  list.find_map(origin_map, fn(pair) {
    let #(copy, origin) = pair
    case copy.transform_id == node.transform_id {
      True -> Ok(origin)
      False -> Error(Nil)
    }
  })
  |> option.from_result
}

// ============================================================================
// Character Handler
// ============================================================================

/// Push work items for a character node
pub fn push_character(
  node: CharacterNode,
  state: GenerateState,
) -> #(List(GenWorkItem), GenerateState) {
  let escaped =
    helpers.get_char_escape(
      node.value,
      state.last_node_was_backref,
      state.in_char_class,
      state.use_flag_v,
    )

  let char = helpers.code_point_to_string(node.value)

  let result = case escaped != char {
    True -> escaped
    False -> {
      // Handle case expansion if needed (for pre-ES2025 targets)
      case
        state.use_applied_ignore_case
        && state.current_flags.ignore_case
        && unicode_case.char_has_case(char)
      {
        True -> {
          let cases = unicode_case.get_ignore_case_match_chars(char)
          case state.in_char_class, list.length(cases) > 1 {
            True, _ ->
              // In char class, just join variants
              cases |> list.fold("", fn(acc, c) { acc <> c })
            False, True ->
              // Outside char class with multiple variants, wrap in class
              "[" <> list.fold(cases, "", fn(acc, c) { acc <> c }) <> "]"
            False, False ->
              // Single variant
              case cases {
                [c] -> c
                _ -> char
              }
          }
        }
        False -> char
      }
    }
  }

  #([SetLastNodeWasBackref(False), PushResult(result)], state)
}

// ============================================================================
// Character Class Handler
// ============================================================================

/// Push work items for a character class node
pub fn push_character_class(
  node: CharacterClassNode,
  state: GenerateState,
) -> Result(#(List(GenWorkItem), GenerateState), String) {
  // Check for intersection without flag v support
  case node.kind == Intersection && !state.use_flag_v {
    True ->
      Error("Use of character class intersection requires min target ES2024")
    False -> {
      let body = node.body
      let count = list.length(body)

      // Handle WebKit hyphen bug workaround (move literal hyphens to start)
      // This is only needed for flag v mode
      let body = case
        state.use_flag_v && list.any(body, helpers.is_literal_hyphen)
      {
        True -> {
          let hyphens = list.filter(body, helpers.is_literal_hyphen)
          let others =
            list.filter(body, fn(el) { !helpers.is_literal_hyphen(el) })
          list.append(hyphens, others)
        }
        False -> body
      }

      let separator = case node.kind {
        Intersection -> "&&"
        Union -> ""
      }

      let negate_prefix = case node.negate {
        True -> "^"
        False -> ""
      }

      // Check if we should unwrap (optimize away unnecessary nesting)
      case should_unwrap_char_class(node, state) {
        True -> {
          // Unwrap: just process children without wrapping
          let el_items =
            body
            |> list.map(fn(el) { ProcessCharClassElement(el) })
          let items = list.append(el_items, [JoinResults(count, separator)])
          Ok(#(items, state))
        }
        False -> {
          // Normal: wrap with [ ] and set in_char_class state
          let el_items =
            body
            |> list.map(fn(el) { ProcessCharClassElement(el) })

          let items = case state.in_char_class {
            True -> {
              // Already in char class, just wrap
              el_items
              |> list.append([
                JoinResults(count, separator),
                WrapResult("[" <> negate_prefix, "]"),
              ])
            }
            False -> {
              // Entering char class, manage state
              [SetInCharClass(True)]
              |> list.append(el_items)
              |> list.append([
                JoinResults(count, separator),
                WrapResult("[" <> negate_prefix, "]"),
                SetInCharClass(False),
              ])
            }
          }
          Ok(#(items, state))
        }
      }
    }
  }
}

fn should_unwrap_char_class(
  node: CharacterClassNode,
  state: GenerateState,
) -> Bool {
  // Only unwrap non-negated union classes when already inside a char class
  // and when verbose mode is off
  case state.in_char_class, node.kind, node.negate, list.is_empty(node.body) {
    True, Union, False, False -> !state.verbose
    _, _, _, _ -> False
  }
}

// ============================================================================
// Character Class Range Handler
// ============================================================================

/// Push work items for a character class range node
pub fn push_character_class_range(
  node: ast_types.CharacterClassRangeNode,
  state: GenerateState,
) -> #(List(GenWorkItem), GenerateState) {
  let min = node.min.value
  let max = node.max.value

  let min_str = helpers.get_char_escape(min, False, True, state.use_flag_v)
  let max_str = helpers.get_char_escape(max, False, True, state.use_flag_v)

  // Handle case expansion for ranges (pre-ES2025 targets)
  let extra_chars = case
    state.use_applied_ignore_case && state.current_flags.ignore_case
  {
    True -> {
      let chars_outside = unicode_case.get_cases_outside_range(min, max, False)
      // Format as escaped characters or ranges
      chars_outside
      |> list.map(fn(c) {
        let cp = case string_to_code_point(c) {
          Ok(v) -> v
          Error(_) -> 0
        }
        helpers.get_char_escape(cp, False, True, state.use_flag_v)
      })
      |> list.fold("", fn(acc, c) { acc <> c })
    }
    False -> ""
  }

  let result = min_str <> "-" <> max_str <> extra_chars
  #([PushResult(result)], state)
}

fn string_to_code_point(s: String) -> Result(Int, Nil) {
  case string.to_utf_codepoints(s) {
    [cp] -> Ok(string.utf_codepoint_to_int(cp))
    _ -> Error(Nil)
  }
}

// ============================================================================
// Character Set Handler
// ============================================================================

/// Push work items for a character set node
pub fn push_character_set(
  node: CharacterSetNode,
  state: GenerateState,
) -> Result(#(List(GenWorkItem), GenerateState), String) {
  case node.kind {
    Dot -> {
      let result = case state.current_flags.dot_all {
        True ->
          case state.applied_global_flags.dot_all || state.use_flag_mods {
            True -> "."
            False -> "[^]"
          }
        // Onig's only line break char is line feed, unlike JS
        False -> "[^\\n]"
      }
      Ok(#([PushResult(result)], state))
    }
    Digit -> {
      let result = case node.negate {
        option.Some(True) -> "\\D"
        _ -> "\\d"
      }
      Ok(#([PushResult(result)], state))
    }
    Property -> {
      // Check for case-sensitive properties used case-insensitively
      let prop_value = option.unwrap(node.value, "")
      case
        state.use_applied_ignore_case
        && state.current_flags.ignore_case
        && set.contains(
          unicode_case.properties_with_specific_case(),
          prop_value,
        )
      {
        True -> {
          let err =
            "Unicode property \""
            <> prop_value
            <> "\" can't be case-insensitive when other chars have specific case"
          Error(err)
        }
        False -> {
          let prefix = case node.negate {
            option.Some(True) -> "\\P"
            _ -> "\\p"
          }
          // Note: CharacterSetNode doesn't have a key field in our AST
          // The JS uses key for property key like Script= or gc=
          let result = prefix <> "{" <> prop_value <> "}"
          Ok(#([PushResult(result)], state))
        }
      }
    }
    Word -> {
      let result = case node.negate {
        option.Some(True) -> "\\W"
        _ -> "\\w"
      }
      Ok(#([PushResult(result)], state))
    }
    _ -> {
      // Other character set kinds are not expected in transformer output
      Error(
        "Unexpected character set kind: " <> char_set_kind_to_string(node.kind),
      )
    }
  }
}

fn char_set_kind_to_string(kind: ast_types.CharacterSetKind) -> String {
  case kind {
    ast_types.Any -> "any"
    Digit -> "digit"
    Dot -> "dot"
    ast_types.Hex -> "hex"
    ast_types.Newline -> "newline"
    ast_types.Posix -> "posix"
    Property -> "property"
    ast_types.Space -> "space"
    ast_types.TextSegment -> "text_segment"
    Word -> "word"
  }
}

// ============================================================================
// Group Handler
// ============================================================================

/// Push work items for a group node
pub fn push_group(
  node: GroupNode,
  state: GenerateState,
) -> #(List(GenWorkItem), GenerateState) {
  let body = node.body
  let count = list.length(body)
  let atomic = option.unwrap(node.atomic, False)

  // Calculate new flags if this group has flag modifiers
  let new_flags = case node.flags {
    option.Some(f) -> get_new_current_flags(state.current_flags, f)
    option.None -> state.current_flags
  }

  // Check if we can unwrap this group (remove unnecessary grouping)
  // Don't unwrap if we're inside a quantifier body - the quantifier needs the group
  let can_unwrap =
    !state.verbose
    && count == 1
    && !atomic
    && !state.in_quantifier_body
    && { !state.use_flag_mods || option.is_none(node.flags) }

  // Push work items for alternatives
  // Work items are processed in order (first item first)
  let alt_items =
    body
    |> list.map(fn(alt) {
      let el_count = list.length(alt.body)
      let el_items =
        alt.body
        |> list.map(fn(el) { ProcessElement(el) })
      list.append(el_items, [JoinResults(el_count, "")])
    })
    |> list.flatten

  case can_unwrap {
    True -> {
      // Unwrap: just process content
      case option.is_some(node.flags) {
        True -> {
          // Need to push/pop flags even when unwrapping
          let items =
            [PushFlags(new_flags)]
            |> list.append(alt_items)
            |> list.append([JoinResults(count, "|"), PopFlags])
          #(items, state)
        }
        False -> {
          let items = list.append(alt_items, [JoinResults(count, "|")])
          #(items, state)
        }
      }
    }
    False -> {
      // Wrap with group
      let prefix =
        "(?"
        <> helpers.get_group_prefix(atomic, node.flags, state.use_flag_mods)

      case option.is_some(node.flags) {
        True -> {
          let items =
            [PushFlags(new_flags)]
            |> list.append(alt_items)
            |> list.append([
              JoinResults(count, "|"),
              WrapResult(prefix, ")"),
              PopFlags,
            ])
          #(items, state)
        }
        False -> {
          let items =
            alt_items
            |> list.append([JoinResults(count, "|"), WrapResult(prefix, ")")])
          #(items, state)
        }
      }
    }
  }
}

fn get_new_current_flags(
  current: types.CurrentFlags,
  mods: ast_types.FlagGroupModifiers,
) -> types.CurrentFlags {
  let ignore_case = case mods.enable {
    option.Some(e) ->
      case e.ignore_case {
        option.Some(True) -> True
        _ ->
          case mods.disable {
            option.Some(d) ->
              case d.ignore_case {
                option.Some(True) -> False
                _ -> current.ignore_case
              }
            option.None -> current.ignore_case
          }
      }
    option.None ->
      case mods.disable {
        option.Some(d) ->
          case d.ignore_case {
            option.Some(True) -> False
            _ -> current.ignore_case
          }
        option.None -> current.ignore_case
      }
  }

  let dot_all = case mods.enable {
    option.Some(e) ->
      case e.dot_all {
        option.Some(True) -> True
        _ ->
          case mods.disable {
            option.Some(d) ->
              case d.dot_all {
                option.Some(True) -> False
                _ -> current.dot_all
              }
            option.None -> current.dot_all
          }
      }
    option.None ->
      case mods.disable {
        option.Some(d) ->
          case d.dot_all {
            option.Some(True) -> False
            _ -> current.dot_all
          }
        option.None -> current.dot_all
      }
  }

  CurrentFlags(ignore_case: ignore_case, dot_all: dot_all)
}

// ============================================================================
// Lookaround Assertion Handler
// ============================================================================

/// Push work items for a lookaround assertion node
pub fn push_lookaround_assertion(
  node: LookaroundAssertionNode,
  state: GenerateState,
) -> #(List(GenWorkItem), GenerateState) {
  let body = node.body
  let count = list.length(body)

  let prefix = "(?" <> get_lookaround_prefix(node.kind, node.negate)

  // Push work items for alternatives
  // Work items are processed in order (first item first)
  let alt_items =
    body
    |> list.map(fn(alt) {
      let el_count = list.length(alt.body)
      let el_items =
        alt.body
        |> list.map(fn(el) { ProcessElement(el) })
      list.append(el_items, [JoinResults(el_count, "")])
    })
    |> list.flatten

  let items =
    alt_items
    |> list.append([JoinResults(count, "|"), WrapResult(prefix, ")")])

  #(items, state)
}

fn get_lookaround_prefix(kind: LookaroundAssertionKind, negate: Bool) -> String {
  let direction = case kind {
    Lookahead -> ""
    Lookbehind -> "<"
  }
  let assertion = case negate {
    True -> "!"
    False -> "="
  }
  direction <> assertion
}

// ============================================================================
// Quantifier Handler
// ============================================================================

/// Push work items for a quantifier node
pub fn push_quantifier(
  node: QuantifierNode,
  state: GenerateState,
) -> #(List(GenWorkItem), GenerateState) {
  let quantifier_str = helpers.get_quantifier_str(node.kind, node.min, node.max)

  // Push work items: set in_quantifier_body, process body, reset, then wrap
  // This ensures groups inside the quantifier body don't unwrap
  let items = [
    SetInQuantifierBody(True),
    ProcessQuantifierBody(node.body),
    SetInQuantifierBody(False),
    WrapResult("", quantifier_str),
  ]

  #(items, state)
}

// ============================================================================
// Subroutine Handler
// ============================================================================

/// Push work items for a subroutine node
pub fn push_subroutine(
  node: SubroutineNode,
  state: GenerateState,
) -> Result(#(List(GenWorkItem), GenerateState), String) {
  case node.is_recursive {
    option.Some(True) -> {
      let limit = state.recursion_limit
      let result = case node.ref {
        NumberedSubroutineRef(0) -> "(?R=" <> int.to_string(limit) <> ")"
        NumberedSubroutineRef(ref) ->
          "\\g<" <> int.to_string(ref) <> "&R=" <> int.to_string(limit) <> ">"
        NamedSubroutineRef(name) ->
          "\\g<" <> name <> "&R=" <> int.to_string(limit) <> ">"
      }
      Ok(#([PushResult(result)], state))
    }
    _ -> Error("Unexpected non-recursive subroutine in transformed AST")
  }
}

// ============================================================================
// Quantifiable Node Handler
// ============================================================================

/// Push work items for a quantifiable node (body of a quantifier)
pub fn push_quantifiable(
  node: QuantifiableNode,
  state: GenerateState,
) -> Result(#(List(GenWorkItem), GenerateState), String) {
  case node {
    AbsenceFunctionQ(_) -> Error("AbsenceFunction not supported in generate")
    BackreferenceQ(n) ->
      push_backreference(
        ast_types.BackreferenceNode(ref: n.ref, orphan: n.orphan),
        state,
      )
    CapturingGroupQ(n) -> Ok(push_capturing_group(n, state))
    CharacterQ(n) -> Ok(push_character(n, state))
    CharacterClassQ(n) -> push_character_class(n, state)
    CharacterSetQ(n) -> push_character_set(n, state)
    GroupQ(n) -> Ok(push_group(n, state))
    QuantifierQ(n) -> Ok(push_quantifier(n, state))
    SubroutineQ(n) -> push_subroutine(n, state)
  }
}

// ============================================================================
// Element Dispatcher
// ============================================================================

/// Dispatch to the appropriate handler for an alternative element
pub fn push_element(
  element: AlternativeElement,
  state: GenerateState,
) -> Result(#(List(GenWorkItem), GenerateState), String) {
  case element {
    ast_types.AbsenceFunctionE(_) ->
      Error("AbsenceFunction not supported in generate")
    AssertionE(n) -> push_assertion(n, state)
    BackreferenceE(n) -> push_backreference(n, state)
    CapturingGroupE(n) -> Ok(push_capturing_group(n, state))
    CharacterE(n) -> Ok(push_character(n, state))
    CharacterClassE(n) -> push_character_class(n, state)
    CharacterSetE(n) -> push_character_set(n, state)
    ast_types.DirectiveE(_) -> Error("Directive not supported in generate")
    GroupE(n) -> Ok(push_group(n, state))
    LookaroundAssertionE(n) -> Ok(push_lookaround_assertion(n, state))
    ast_types.NamedCalloutE(_) ->
      Error("NamedCallout not supported in generate")
    QuantifierE(n) -> Ok(push_quantifier(n, state))
    SubroutineE(n) -> push_subroutine(n, state)
  }
}

// ============================================================================
// Character Class Element Dispatcher
// ============================================================================

/// Dispatch to the appropriate handler for a character class element
pub fn push_cc_element(
  element: CharacterClassElement,
  state: GenerateState,
) -> Result(#(List(GenWorkItem), GenerateState), String) {
  case element {
    CharacterCCE(n) -> Ok(push_character(n, state))
    CharacterClassCCE(n) -> push_character_class(n, state)
    CharacterClassRangeCCE(n) -> Ok(push_character_class_range(n, state))
    CharacterSetCCE(n) -> push_character_set(n, state)
  }
}
