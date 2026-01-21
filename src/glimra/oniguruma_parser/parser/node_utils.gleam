/// Node utility functions for working with AST nodes.
/// These functions help with AST traversal and manipulation.
import gleam/list
import gleam/option.{type Option, None, Some}
import glimra/oniguruma_parser/parser/ast_types.{
  type AlternativeElement, type CharacterClassElement, type Node,
  type ParentNode, type QuantifiableNode, AbsenceFunctionAC, AbsenceFunctionE,
  AbsenceFunctionN, AbsenceFunctionQ, AlternativeContainerP, AlternativeN,
  AlternativeP, AssertionE, AssertionN, BackreferenceE, BackreferenceN,
  BackreferenceQ, CapturingGroupAC, CapturingGroupE, CapturingGroupN,
  CapturingGroupQ, CharacterCCE, CharacterClassCCE, CharacterClassE,
  CharacterClassN, CharacterClassP, CharacterClassQ, CharacterClassRangeCCE,
  CharacterClassRangeN, CharacterClassRangeP, CharacterE, CharacterN, CharacterQ,
  CharacterSetCCE, CharacterSetE, CharacterSetN, CharacterSetQ, DirectiveE,
  DirectiveN, FlagsN, GroupAC, GroupE, GroupN, GroupQ, LookaroundAssertionAC,
  LookaroundAssertionE, LookaroundAssertionN, NamedCalloutE, NamedCalloutN,
  QuantifierE, QuantifierN, QuantifierP, QuantifierQ, RegexAC, RegexN, Repeater,
  SubroutineE, SubroutineN, SubroutineQ,
}

/// Check if a parent node has exactly one child matching the given predicate
pub fn has_only_child(
  node: ParentNode,
  predicate: Option(fn(Node) -> Bool),
) -> Bool {
  let body = get_parent_body(node)
  case body, predicate {
    [_single], None -> True
    [single], Some(pred) -> pred(single)
    _, _ -> False
  }
}

/// Get the body of a parent node
fn get_parent_body(node: ParentNode) -> List(Node) {
  case node {
    AlternativeContainerP(ac) ->
      case ac {
        AbsenceFunctionAC(n) -> list.map(n.body, AlternativeN)
        CapturingGroupAC(n) -> list.map(n.body, AlternativeN)
        GroupAC(n) -> list.map(n.body, AlternativeN)
        LookaroundAssertionAC(n) -> list.map(n.body, AlternativeN)
        RegexAC(n) -> list.map(n.body, AlternativeN)
      }
    AlternativeP(alt) -> list.map(alt.body, element_to_node)
    CharacterClassP(cc) -> list.map(cc.body, cc_element_to_node)
    CharacterClassRangeP(range) -> [
      CharacterN(range.min),
      CharacterN(range.max),
    ]
    QuantifierP(q) -> [quantifiable_to_node(q.body)]
  }
}

/// Convert an alternative element to a node
fn element_to_node(element: AlternativeElement) -> Node {
  case element {
    AbsenceFunctionE(n) -> AbsenceFunctionN(n)
    AssertionE(n) -> AssertionN(n)
    BackreferenceE(n) -> BackreferenceN(n)
    CapturingGroupE(n) -> CapturingGroupN(n)
    CharacterE(n) -> CharacterN(n)
    CharacterClassE(n) -> CharacterClassN(n)
    CharacterSetE(n) -> CharacterSetN(n)
    DirectiveE(n) -> DirectiveN(n)
    GroupE(n) -> GroupN(n)
    LookaroundAssertionE(n) -> LookaroundAssertionN(n)
    NamedCalloutE(n) -> NamedCalloutN(n)
    QuantifierE(n) -> QuantifierN(n)
    SubroutineE(n) -> SubroutineN(n)
  }
}

/// Convert a character class element to a node
fn cc_element_to_node(element: CharacterClassElement) -> Node {
  case element {
    CharacterCCE(n) -> CharacterN(n)
    CharacterClassCCE(n) -> CharacterClassN(n)
    CharacterClassRangeCCE(n) -> CharacterClassRangeN(n)
    CharacterSetCCE(n) -> CharacterSetN(n)
  }
}

/// Convert a quantifiable node to a node
fn quantifiable_to_node(node: QuantifiableNode) -> Node {
  case node {
    AbsenceFunctionQ(n) -> AbsenceFunctionN(n)
    BackreferenceQ(n) -> BackreferenceN(n)
    CapturingGroupQ(n) -> CapturingGroupN(n)
    CharacterQ(n) -> CharacterN(n)
    CharacterClassQ(n) -> CharacterClassN(n)
    CharacterSetQ(n) -> CharacterSetN(n)
    GroupQ(n) -> GroupN(n)
    QuantifierQ(n) -> QuantifierN(n)
    SubroutineQ(n) -> SubroutineN(n)
  }
}

/// Check if a node can contain alternatives
pub fn is_alternative_container(node: Node) -> Bool {
  case node {
    AbsenceFunctionN(n) -> n.kind == Repeater
    CapturingGroupN(_) -> True
    GroupN(_) -> True
    LookaroundAssertionN(_) -> True
    RegexN(_) -> True
    _ -> False
  }
}

/// Check if a node can be quantified
pub fn is_quantifiable(node: Node) -> Bool {
  case node {
    AbsenceFunctionN(_) -> True
    BackreferenceN(_) -> True
    CapturingGroupN(_) -> True
    CharacterN(_) -> True
    CharacterClassN(_) -> True
    CharacterSetN(_) -> True
    GroupN(_) -> True
    QuantifierN(_) -> True
    SubroutineN(_) -> True
    _ -> False
  }
}

/// Check if an alternative element can be quantified
pub fn is_element_quantifiable(element: AlternativeElement) -> Bool {
  case element {
    AbsenceFunctionE(_) -> True
    BackreferenceE(_) -> True
    CapturingGroupE(_) -> True
    CharacterE(_) -> True
    CharacterClassE(_) -> True
    CharacterSetE(_) -> True
    GroupE(_) -> True
    QuantifierE(_) -> True
    SubroutineE(_) -> True
    _ -> False
  }
}

/// Get the type name of a node
pub fn node_type(node: Node) -> String {
  case node {
    AbsenceFunctionN(_) -> "AbsenceFunction"
    AlternativeN(_) -> "Alternative"
    AssertionN(_) -> "Assertion"
    BackreferenceN(_) -> "Backreference"
    CapturingGroupN(_) -> "CapturingGroup"
    CharacterN(_) -> "Character"
    CharacterClassN(_) -> "CharacterClass"
    CharacterClassRangeN(_) -> "CharacterClassRange"
    CharacterSetN(_) -> "CharacterSet"
    DirectiveN(_) -> "Directive"
    FlagsN(_) -> "Flags"
    GroupN(_) -> "Group"
    LookaroundAssertionN(_) -> "LookaroundAssertion"
    NamedCalloutN(_) -> "NamedCallout"
    QuantifierN(_) -> "Quantifier"
    RegexN(_) -> "Regex"
    SubroutineN(_) -> "Subroutine"
  }
}
