/// JSON serialization for Oniguruma AST nodes.
/// Used for testing the parser against the JS reference implementation.
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import glimra/oniguruma_parser/parser/ast_types.{
  type AbsenceFunctionKind, type AbsenceFunctionNode, type AlternativeElement,
  type AlternativeNode, type AssertionKind, type AssertionNode,
  type BackreferenceNode, type BackreferenceRef, type CalloutArg,
  type CapturingGroupNode, type CharacterClassElement, type CharacterClassKind,
  type CharacterClassNode, type CharacterClassRangeNode, type CharacterNode,
  type CharacterSetKind, type CharacterSetNode, type DirectiveKind,
  type DirectiveNode, type FlagGroupModifiers, type FlagGroupSwitches,
  type FlagsNode, type GroupNode, type LookaroundAssertionKind,
  type LookaroundAssertionNode, type NamedCalloutKind, type NamedCalloutNode,
  type QuantifierKind, type QuantifierNode, type RegexNode, type SubroutineNode,
  type SubroutineRef, type TextSegmentMode, AbsenceFunctionE, Any, AssertionE,
  BackreferenceE, CapturingGroupE, CharacterCCE, CharacterClassCCE,
  CharacterClassE, CharacterClassRangeCCE, CharacterE, CharacterSetCCE,
  CharacterSetE, Cmp, Count, Custom, Digit, DirectiveE, Dot, ErrorCallout, Fail,
  Flags, Grapheme, Greedy, GroupE, Hex, Intersection, Keep, Lazy, LineEnd,
  LineStart, Lookahead, LookaroundAssertionE, Lookbehind, Max, Mismatch,
  NamedCalloutE, NamedRef, NamedSubroutineRef, Newline, NumberArg, NumberedRef,
  NumberedSubroutineRef, Posix, Possessive, Property, QuantifierE, Repeater,
  SearchStart, Skip, Space, StringArg, StringEnd, StringEndNewline, StringStart,
  SubroutineE, TextSegment, TextSegmentBoundary, Union, Word, WordBoundary,
  WordMode,
}

/// Convert a RegexNode AST to JSON
pub fn regex_to_json(node: RegexNode) -> Json {
  json.object([
    #("type", json.string("Regex")),
    #("body", json.array(node.body, alternative_to_json)),
    #("flags", flags_to_json(node.flags)),
  ])
}

/// Convert an AlternativeNode to JSON
pub fn alternative_to_json(node: AlternativeNode) -> Json {
  json.object([
    #("type", json.string("Alternative")),
    #("body", json.array(node.body, element_to_json)),
  ])
}

/// Convert an AlternativeElement to JSON
pub fn element_to_json(element: AlternativeElement) -> Json {
  case element {
    AbsenceFunctionE(n) -> absence_function_to_json(n)
    AssertionE(n) -> assertion_to_json(n)
    BackreferenceE(n) -> backreference_to_json(n)
    CapturingGroupE(n) -> capturing_group_to_json(n)
    CharacterE(n) -> character_to_json(n)
    CharacterClassE(n) -> character_class_to_json(n)
    CharacterSetE(n) -> character_set_to_json(n)
    DirectiveE(n) -> directive_to_json(n)
    GroupE(n) -> group_to_json(n)
    LookaroundAssertionE(n) -> lookaround_assertion_to_json(n)
    NamedCalloutE(n) -> named_callout_to_json(n)
    QuantifierE(n) -> quantifier_to_json(n)
    SubroutineE(n) -> subroutine_to_json(n)
  }
}

/// Convert an AbsenceFunctionNode to JSON
pub fn absence_function_to_json(node: AbsenceFunctionNode) -> Json {
  json.object([
    #("type", json.string("AbsenceFunction")),
    #("kind", absence_function_kind_to_json(node.kind)),
    #("body", json.array(node.body, alternative_to_json)),
  ])
}

fn absence_function_kind_to_json(kind: AbsenceFunctionKind) -> Json {
  case kind {
    Repeater -> json.string("repeater")
  }
}

/// Convert an AssertionNode to JSON
pub fn assertion_to_json(node: AssertionNode) -> Json {
  let base = [
    #("type", json.string("Assertion")),
    #("kind", assertion_kind_to_json(node.kind)),
  ]
  let with_negate = case node.negate {
    Some(n) -> list.append(base, [#("negate", json.bool(n))])
    None -> base
  }
  json.object(with_negate)
}

fn assertion_kind_to_json(kind: AssertionKind) -> Json {
  case kind {
    LineEnd -> json.string("line_end")
    LineStart -> json.string("line_start")
    SearchStart -> json.string("search_start")
    StringEnd -> json.string("string_end")
    StringEndNewline -> json.string("string_end_newline")
    StringStart -> json.string("string_start")
    TextSegmentBoundary -> json.string("text_segment_boundary")
    WordBoundary -> json.string("word_boundary")
  }
}

/// Convert a BackreferenceNode to JSON
pub fn backreference_to_json(node: BackreferenceNode) -> Json {
  let base = [
    #("type", json.string("Backreference")),
    #("ref", backref_ref_to_json(node.ref)),
  ]
  let with_orphan = case node.orphan {
    Some(o) -> list.append(base, [#("orphan", json.bool(o))])
    None -> base
  }
  json.object(with_orphan)
}

fn backref_ref_to_json(ref: BackreferenceRef) -> Json {
  case ref {
    NumberedRef(n) -> json.int(n)
    NamedRef(name) -> json.string(name)
  }
}

/// Convert a CapturingGroupNode to JSON
pub fn capturing_group_to_json(node: CapturingGroupNode) -> Json {
  // -1 is a sentinel for null (used for dummy captures)
  let number_json = case node.number {
    -1 -> json.null()
    n -> json.int(n)
  }
  let base = [
    #("type", json.string("CapturingGroup")),
    #("number", number_json),
    #("body", json.array(node.body, alternative_to_json)),
  ]
  let with_name = case node.name {
    Some(n) -> list.append(base, [#("name", json.string(n))])
    None -> base
  }
  let with_subroutined = case node.is_subroutined {
    Some(s) -> list.append(with_name, [#("isSubroutined", json.bool(s))])
    None -> with_name
  }
  json.object(with_subroutined)
}

/// Convert a CharacterNode to JSON
pub fn character_to_json(node: CharacterNode) -> Json {
  json.object([
    #("type", json.string("Character")),
    #("value", json.int(node.value)),
  ])
}

/// Convert a CharacterClassNode to JSON
pub fn character_class_to_json(node: CharacterClassNode) -> Json {
  json.object([
    #("type", json.string("CharacterClass")),
    #("kind", character_class_kind_to_json(node.kind)),
    #("negate", json.bool(node.negate)),
    #("body", json.array(node.body, cc_element_to_json)),
  ])
}

fn character_class_kind_to_json(kind: CharacterClassKind) -> Json {
  case kind {
    Union -> json.string("union")
    Intersection -> json.string("intersection")
  }
}

/// Convert a CharacterClassElement to JSON
fn cc_element_to_json(element: CharacterClassElement) -> Json {
  case element {
    CharacterCCE(n) -> character_to_json(n)
    CharacterClassCCE(n) -> character_class_to_json(n)
    CharacterClassRangeCCE(n) -> character_class_range_to_json(n)
    CharacterSetCCE(n) -> character_set_to_json(n)
  }
}

/// Convert a CharacterClassRangeNode to JSON
pub fn character_class_range_to_json(node: CharacterClassRangeNode) -> Json {
  json.object([
    #("type", json.string("CharacterClassRange")),
    #("min", character_to_json(node.min)),
    #("max", character_to_json(node.max)),
  ])
}

/// Convert a CharacterSetNode to JSON
pub fn character_set_to_json(node: CharacterSetNode) -> Json {
  let base = [
    #("type", json.string("CharacterSet")),
    #("kind", character_set_kind_to_json(node.kind)),
  ]
  let with_value = case node.value {
    Some(v) -> list.append(base, [#("value", json.string(v))])
    None -> base
  }
  let with_negate = case node.negate {
    Some(n) -> list.append(with_value, [#("negate", json.bool(n))])
    None -> with_value
  }
  let with_var_len = case node.variable_length {
    Some(vl) -> list.append(with_negate, [#("variableLength", json.bool(vl))])
    None -> with_negate
  }
  json.object(with_var_len)
}

fn character_set_kind_to_json(kind: CharacterSetKind) -> Json {
  case kind {
    Any -> json.string("any")
    Digit -> json.string("digit")
    Dot -> json.string("dot")
    Hex -> json.string("hex")
    Newline -> json.string("newline")
    Posix -> json.string("posix")
    Property -> json.string("property")
    Space -> json.string("space")
    TextSegment -> json.string("text_segment")
    Word -> json.string("word")
  }
}

/// Convert a DirectiveNode to JSON
pub fn directive_to_json(node: DirectiveNode) -> Json {
  let base = [
    #("type", json.string("Directive")),
    #("kind", directive_kind_to_json(node.kind)),
  ]
  let with_flags = case node.flags {
    Some(f) -> list.append(base, [#("flags", flag_group_modifiers_to_json(f))])
    None -> base
  }
  json.object(with_flags)
}

fn directive_kind_to_json(kind: DirectiveKind) -> Json {
  case kind {
    Keep -> json.string("keep")
    Flags -> json.string("flags")
  }
}

/// Convert FlagsNode to JSON
pub fn flags_to_json(node: FlagsNode) -> Json {
  let text_segment_json = case node.text_segment_mode {
    Some(tsm) -> text_segment_mode_to_json(tsm)
    None -> json.null()
  }
  json.object([
    #("type", json.string("Flags")),
    #("ignoreCase", json.bool(node.ignore_case)),
    #("dotAll", json.bool(node.dot_all)),
    #("extended", json.bool(node.extended)),
    #("digitIsAscii", json.bool(node.digit_is_ascii)),
    #("posixIsAscii", json.bool(node.posix_is_ascii)),
    #("spaceIsAscii", json.bool(node.space_is_ascii)),
    #("wordIsAscii", json.bool(node.word_is_ascii)),
    #("textSegmentMode", text_segment_json),
  ])
}

fn text_segment_mode_to_json(mode: TextSegmentMode) -> Json {
  case mode {
    Grapheme -> json.string("grapheme")
    WordMode -> json.string("word")
  }
}

/// Convert FlagGroupModifiers to JSON
fn flag_group_modifiers_to_json(modifiers: FlagGroupModifiers) -> Json {
  let base = []
  let with_enable = case modifiers.enable {
    Some(e) -> list.append(base, [#("enable", flag_group_switches_to_json(e))])
    None -> base
  }
  let with_disable = case modifiers.disable {
    Some(d) ->
      list.append(with_enable, [#("disable", flag_group_switches_to_json(d))])
    None -> with_enable
  }
  json.object(with_disable)
}

fn flag_group_switches_to_json(switches: FlagGroupSwitches) -> Json {
  let base = []
  let with_ic = case switches.ignore_case {
    Some(ic) -> list.append(base, [#("ignoreCase", json.bool(ic))])
    None -> base
  }
  let with_da = case switches.dot_all {
    Some(da) -> list.append(with_ic, [#("dotAll", json.bool(da))])
    None -> with_ic
  }
  let with_ext = case switches.extended {
    Some(ext) -> list.append(with_da, [#("extended", json.bool(ext))])
    None -> with_da
  }
  json.object(with_ext)
}

/// Convert a GroupNode to JSON
pub fn group_to_json(node: GroupNode) -> Json {
  let base = [
    #("type", json.string("Group")),
    #("body", json.array(node.body, alternative_to_json)),
  ]
  let with_atomic = case node.atomic {
    Some(a) -> list.append(base, [#("atomic", json.bool(a))])
    None -> base
  }
  let with_flags = case node.flags {
    Some(f) ->
      list.append(with_atomic, [#("flags", flag_group_modifiers_to_json(f))])
    None -> with_atomic
  }
  json.object(with_flags)
}

/// Convert a LookaroundAssertionNode to JSON
pub fn lookaround_assertion_to_json(node: LookaroundAssertionNode) -> Json {
  json.object([
    #("type", json.string("LookaroundAssertion")),
    #("kind", lookaround_kind_to_json(node.kind)),
    #("negate", json.bool(node.negate)),
    #("body", json.array(node.body, alternative_to_json)),
  ])
}

fn lookaround_kind_to_json(kind: LookaroundAssertionKind) -> Json {
  case kind {
    Lookahead -> json.string("lookahead")
    Lookbehind -> json.string("lookbehind")
  }
}

/// Convert a NamedCalloutNode to JSON
pub fn named_callout_to_json(node: NamedCalloutNode) -> Json {
  let base = [
    #("type", json.string("NamedCallout")),
    #("kind", named_callout_kind_to_json(node.kind)),
  ]
  let with_tag = case node.tag {
    Some(t) -> list.append(base, [#("tag", json.string(t))])
    None -> base
  }
  let with_args = case node.arguments {
    Some(args) ->
      list.append(with_tag, [
        #("arguments", json.array(args, callout_arg_to_json)),
      ])
    None -> with_tag
  }
  json.object(with_args)
}

fn named_callout_kind_to_json(kind: NamedCalloutKind) -> Json {
  case kind {
    Count -> json.string("count")
    Cmp -> json.string("cmp")
    ErrorCallout -> json.string("error")
    Fail -> json.string("fail")
    Max -> json.string("max")
    Mismatch -> json.string("mismatch")
    Skip -> json.string("skip")
    ast_types.TotalCount -> json.string("total_count")
    Custom -> json.string("custom")
  }
}

fn callout_arg_to_json(arg: CalloutArg) -> Json {
  case arg {
    StringArg(s) -> json.string(s)
    NumberArg(n) -> json.int(n)
  }
}

/// Convert a QuantifierNode to JSON
pub fn quantifier_to_json(node: QuantifierNode) -> Json {
  // If max is the infinity constant, serialize as null (matching JS behavior)
  let max_json = case node.max == ast_types.quantifier_max_infinity {
    True -> json.null()
    False -> json.int(node.max)
  }
  json.object([
    #("type", json.string("Quantifier")),
    #("kind", quantifier_kind_to_json(node.kind)),
    #("min", json.int(node.min)),
    #("max", max_json),
    #("body", quantifiable_to_json(node.body)),
  ])
}

fn quantifier_kind_to_json(kind: QuantifierKind) -> Json {
  case kind {
    Greedy -> json.string("greedy")
    Lazy -> json.string("lazy")
    Possessive -> json.string("possessive")
  }
}

fn quantifiable_to_json(node: ast_types.QuantifiableNode) -> Json {
  case node {
    ast_types.AbsenceFunctionQ(n) -> absence_function_to_json(n)
    ast_types.BackreferenceQ(n) -> backreference_to_json(n)
    ast_types.CapturingGroupQ(n) -> capturing_group_to_json(n)
    ast_types.CharacterQ(n) -> character_to_json(n)
    ast_types.CharacterClassQ(n) -> character_class_to_json(n)
    ast_types.CharacterSetQ(n) -> character_set_to_json(n)
    ast_types.GroupQ(n) -> group_to_json(n)
    ast_types.QuantifierQ(n) -> quantifier_to_json(n)
    ast_types.SubroutineQ(n) -> subroutine_to_json(n)
  }
}

/// Convert a SubroutineNode to JSON
pub fn subroutine_to_json(node: SubroutineNode) -> Json {
  json.object([
    #("type", json.string("Subroutine")),
    #("ref", subroutine_ref_to_json(node.ref)),
  ])
}

fn subroutine_ref_to_json(ref: SubroutineRef) -> Json {
  case ref {
    NumberedSubroutineRef(n) -> json.int(n)
    NamedSubroutineRef(name) -> json.string(name)
  }
}

/// Serialize an AST to a JSON string
pub fn ast_to_string(ast: RegexNode) -> String {
  regex_to_json(ast)
  |> json.to_string
}

// ============================================================================
// RegexPlusAst Serialization
// ============================================================================

import gleam/dict
import glimra/oniguruma_to_es/transform/types.{
  type RegexPlusAst, type RegexPlusFlags, type Strategy, type TransformOptions,
  ClipSearch,
}

/// Convert a RegexPlusAst to JSON
pub fn regex_plus_ast_to_json(rpa: RegexPlusAst) -> Json {
  json.object([
    #("type", json.string("Regex")),
    #("body", json.array(rpa.ast.body, alternative_to_json)),
    #("flags", regex_plus_flags_to_json(rpa.flags)),
    #("options", transform_options_to_json(rpa.options)),
    #("_originMap", origin_map_to_json(rpa.origin_map)),
    #("_strategy", strategy_to_json(rpa.strategy)),
  ])
}

/// Convert RegexPlusFlags to JSON
fn regex_plus_flags_to_json(flags: RegexPlusFlags) -> Json {
  json.object([
    #("type", json.string("Flags")),
    #("ignoreCase", json.bool(flags.ignore_case)),
    #("dotAll", json.bool(flags.dot_all)),
    #("global", json.bool(flags.global)),
    #("hasIndices", json.bool(flags.has_indices)),
    #("multiline", json.bool(flags.multiline)),
    #("sticky", json.bool(flags.sticky)),
  ])
}

/// Convert TransformOptions to JSON
fn transform_options_to_json(options: TransformOptions) -> Json {
  json.object([
    #(
      "disable",
      json.object([
        #("x", json.bool(options.disable.x)),
        #("n", json.bool(options.disable.n)),
      ]),
    ),
    #("force", json.object([#("v", json.bool(options.force.v))])),
  ])
}

/// Convert origin map to JSON (as array of pairs)
fn origin_map_to_json(origin_map: dict.Dict(Int, Int)) -> Json {
  let pairs =
    dict.to_list(origin_map)
    |> list.map(fn(pair) {
      let #(copy_num, origin_num) = pair
      json.array([json.int(copy_num), json.int(origin_num)], fn(x) { x })
    })
  json.preprocessed_array(pairs)
}

/// Convert Strategy to JSON
fn strategy_to_json(strategy: Option(Strategy)) -> Json {
  case strategy {
    None -> json.null()
    Some(ClipSearch) -> json.string("clip_search")
  }
}

/// Serialize a RegexPlusAst to a JSON string
pub fn regex_plus_ast_to_string(rpa: RegexPlusAst) -> String {
  regex_plus_ast_to_json(rpa)
  |> json.to_string
}
