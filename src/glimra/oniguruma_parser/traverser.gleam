//// AST traverser for Oniguruma regex patterns.
//// Provides visitor-based traversal and modification of the AST.

import gleam/list
import gleam/option.{type Option, None, Some}
import glimra/oniguruma_parser/parser/ast_types.{
  type AbsenceFunctionNode, type AlternativeElement, type AlternativeNode,
  type AssertionNode, type BackreferenceNode, type CapturingGroupNode,
  type CharacterClassElement, type CharacterClassNode,
  type CharacterClassRangeNode, type CharacterNode, type CharacterSetNode,
  type DirectiveNode, type FlagsNode, type GroupNode,
  type LookaroundAssertionNode, type NamedCalloutNode, type Node,
  type QuantifiableNode, type QuantifierNode, type RegexNode,
  type SubroutineNode, AbsenceFunctionE, AbsenceFunctionN, AbsenceFunctionNode,
  AbsenceFunctionQ, AlternativeN, AlternativeNode, AssertionE, BackreferenceE,
  BackreferenceQ, CapturingGroupE, CapturingGroupN, CapturingGroupNode,
  CapturingGroupQ, CharacterCCE, CharacterClassCCE, CharacterClassE,
  CharacterClassN, CharacterClassNode, CharacterClassQ, CharacterClassRangeCCE,
  CharacterClassRangeN, CharacterClassRangeNode, CharacterE, CharacterQ,
  CharacterSetCCE, CharacterSetE, CharacterSetQ, DirectiveE, GroupE, GroupN,
  GroupNode, GroupQ, LookaroundAssertionE, LookaroundAssertionN,
  LookaroundAssertionNode, NamedCalloutE, QuantifierE, QuantifierN,
  QuantifierNode, QuantifierQ, RegexN, RegexNode, SubroutineE, SubroutineQ,
}

// ============================================================================
// Path Type
// ============================================================================

/// Path information for a node during traversal
pub type Path(n, root, state) {
  Path(
    node: n,
    parent: Option(Node),
    key: Option(PathKey),
    root: root,
    state: state,
  )
}

/// Key to identify a node's position in its parent
pub type PathKey {
  IndexKey(Int)
  StringKey(String)
}

// ============================================================================
// Visitor Type
// ============================================================================

/// Visitor for traversing the AST
/// Each field is a function that takes a path and returns an action
pub type Visitor(state) {
  Visitor(
    on_absence_function: Option(VisitorFn(AbsenceFunctionNode, state)),
    on_alternative: Option(VisitorFn(AlternativeNode, state)),
    on_assertion: Option(VisitorFn(AssertionNode, state)),
    on_backreference: Option(VisitorFn(BackreferenceNode, state)),
    on_capturing_group: Option(VisitorFn(CapturingGroupNode, state)),
    on_character: Option(VisitorFn(CharacterNode, state)),
    on_character_class: Option(VisitorFn(CharacterClassNode, state)),
    on_character_class_range: Option(VisitorFn(CharacterClassRangeNode, state)),
    on_character_set: Option(VisitorFn(CharacterSetNode, state)),
    on_directive: Option(VisitorFn(DirectiveNode, state)),
    on_flags: Option(VisitorFn(FlagsNode, state)),
    on_group: Option(VisitorFn(GroupNode, state)),
    on_lookaround_assertion: Option(VisitorFn(LookaroundAssertionNode, state)),
    on_named_callout: Option(VisitorFn(NamedCalloutNode, state)),
    on_quantifier: Option(VisitorFn(QuantifierNode, state)),
    on_regex: Option(VisitorFn(RegexNode, state)),
    on_subroutine: Option(VisitorFn(SubroutineNode, state)),
    on_any: Option(VisitorFn(Node, state)),
  )
}

/// A visitor function that processes a node and returns a result
pub type VisitorFn(n, state) =
  fn(n, state) -> #(VisitResult(n), state)

/// Result of visiting a node
pub type VisitResult(n) {
  /// Continue traversal with (potentially modified) node
  Continue(n)
  /// Skip traversing children of this node
  Skip(n)
  /// Remove this node from its parent
  Remove
  /// Replace this node with another
  Replace(n)
}

// ============================================================================
// Traverse Functions
// ============================================================================

/// Traverse a regex AST with the given visitor
pub fn traverse(
  ast: RegexNode,
  visitor: Visitor(state),
  initial_state: state,
) -> #(RegexNode, state) {
  traverse_regex(ast, visitor, initial_state, None)
}

/// Traverse a regex node
fn traverse_regex(
  node: RegexNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(RegexNode, state) {
  // Call on_any first
  let #(result1, state1) = case visitor.on_any {
    Some(f) -> {
      let #(r, s) = f(RegexN(node), state)
      case r {
        Continue(RegexN(n)) -> #(Continue(n), s)
        Skip(RegexN(n)) -> #(Skip(n), s)
        Remove -> #(Remove, s)
        Replace(RegexN(n)) -> #(Replace(n), s)
        _ -> #(Continue(node), s)
      }
    }
    None -> #(Continue(node), state)
  }

  // Check if we should continue
  case result1 {
    Remove -> #(node, state1)
    // Can't really remove root
    Skip(n) -> #(n, state1)
    Continue(n) | Replace(n) -> {
      // Call on_regex
      let #(result2, state2) = case visitor.on_regex {
        Some(f) -> f(n, state1)
        None -> #(Continue(n), state1)
      }

      case result2 {
        Remove -> #(n, state2)
        Skip(n2) -> #(n2, state2)
        Continue(n2) | Replace(n2) -> {
          // Traverse children
          let #(new_body, state3) =
            traverse_alternatives(n2.body, visitor, state2, Some(RegexN(n2)))
          let #(new_flags, state4) =
            traverse_flags(n2.flags, visitor, state3, Some(RegexN(n2)))
          #(RegexNode(body: new_body, flags: new_flags), state4)
        }
      }
    }
  }
}

/// Traverse a list of alternatives
fn traverse_alternatives(
  alts: List(AlternativeNode),
  visitor: Visitor(state),
  state: state,
  parent: Option(Node),
) -> #(List(AlternativeNode), state) {
  let #(new_state, new_list) =
    list.map_fold(alts, state, fn(s, alt) {
      let #(new_alt, new_s) = traverse_alternative(alt, visitor, s, parent)
      #(new_s, new_alt)
    })
  #(new_list, new_state)
}

/// Traverse an alternative
fn traverse_alternative(
  node: AlternativeNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(AlternativeNode, state) {
  // Call on_alternative
  let #(result, state1) = case visitor.on_alternative {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }

  case result {
    Remove -> #(AlternativeNode(body: []), state1)
    Skip(n) -> #(n, state1)
    Continue(n) | Replace(n) -> {
      // Traverse children
      let #(new_body, state2) =
        traverse_elements(n.body, visitor, state1, Some(AlternativeN(n)))
      #(AlternativeNode(body: new_body), state2)
    }
  }
}

/// Traverse a list of alternative elements
fn traverse_elements(
  elements: List(AlternativeElement),
  visitor: Visitor(state),
  state: state,
  parent: Option(Node),
) -> #(List(AlternativeElement), state) {
  let #(new_state, new_list) =
    list.map_fold(elements, state, fn(s, elem) {
      let #(new_elem, new_s) = traverse_element(elem, visitor, s, parent)
      #(new_s, new_elem)
    })
  #(new_list, new_state)
}

/// Traverse an alternative element
fn traverse_element(
  element: AlternativeElement,
  visitor: Visitor(state),
  state: state,
  parent: Option(Node),
) -> #(AlternativeElement, state) {
  case element {
    AbsenceFunctionE(n) -> {
      let #(new_n, new_state) =
        traverse_absence_function(n, visitor, state, parent)
      #(AbsenceFunctionE(new_n), new_state)
    }
    AssertionE(n) -> {
      let #(new_n, new_state) = traverse_assertion(n, visitor, state, parent)
      #(AssertionE(new_n), new_state)
    }
    BackreferenceE(n) -> {
      let #(new_n, new_state) =
        traverse_backreference(n, visitor, state, parent)
      #(BackreferenceE(new_n), new_state)
    }
    CapturingGroupE(n) -> {
      let #(new_n, new_state) =
        traverse_capturing_group(n, visitor, state, parent)
      #(CapturingGroupE(new_n), new_state)
    }
    CharacterE(n) -> {
      let #(new_n, new_state) = traverse_character(n, visitor, state, parent)
      #(CharacterE(new_n), new_state)
    }
    CharacterClassE(n) -> {
      let #(new_n, new_state) =
        traverse_character_class(n, visitor, state, parent)
      #(CharacterClassE(new_n), new_state)
    }
    CharacterSetE(n) -> {
      let #(new_n, new_state) =
        traverse_character_set(n, visitor, state, parent)
      #(CharacterSetE(new_n), new_state)
    }
    DirectiveE(n) -> {
      let #(new_n, new_state) = traverse_directive(n, visitor, state, parent)
      #(DirectiveE(new_n), new_state)
    }
    GroupE(n) -> {
      let #(new_n, new_state) = traverse_group(n, visitor, state, parent)
      #(GroupE(new_n), new_state)
    }
    LookaroundAssertionE(n) -> {
      let #(new_n, new_state) =
        traverse_lookaround_assertion(n, visitor, state, parent)
      #(LookaroundAssertionE(new_n), new_state)
    }
    NamedCalloutE(n) -> {
      let #(new_n, new_state) =
        traverse_named_callout(n, visitor, state, parent)
      #(NamedCalloutE(new_n), new_state)
    }
    QuantifierE(n) -> {
      let #(new_n, new_state) = traverse_quantifier(n, visitor, state, parent)
      #(QuantifierE(new_n), new_state)
    }
    SubroutineE(n) -> {
      let #(new_n, new_state) = traverse_subroutine(n, visitor, state, parent)
      #(SubroutineE(new_n), new_state)
    }
  }
}

/// Traverse an absence function
fn traverse_absence_function(
  node: AbsenceFunctionNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(AbsenceFunctionNode, state) {
  let #(result, state1) = case visitor.on_absence_function {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }

  case result {
    Skip(n) -> #(n, state1)
    Remove -> #(node, state1)
    Continue(n) | Replace(n) -> {
      let #(new_body, state2) =
        traverse_alternatives(
          n.body,
          visitor,
          state1,
          Some(AbsenceFunctionN(n)),
        )
      #(AbsenceFunctionNode(..n, body: new_body), state2)
    }
  }
}

/// Traverse an assertion
fn traverse_assertion(
  node: AssertionNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(AssertionNode, state) {
  let #(result, state1) = case visitor.on_assertion {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }
  case result {
    Continue(n) | Skip(n) | Replace(n) -> #(n, state1)
    Remove -> #(node, state1)
  }
}

/// Traverse a backreference
fn traverse_backreference(
  node: BackreferenceNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(BackreferenceNode, state) {
  let #(result, state1) = case visitor.on_backreference {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }
  case result {
    Continue(n) | Skip(n) | Replace(n) -> #(n, state1)
    Remove -> #(node, state1)
  }
}

/// Traverse a capturing group
fn traverse_capturing_group(
  node: CapturingGroupNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(CapturingGroupNode, state) {
  let #(result, state1) = case visitor.on_capturing_group {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }

  case result {
    Skip(n) -> #(n, state1)
    Remove -> #(node, state1)
    Continue(n) | Replace(n) -> {
      let #(new_body, state2) =
        traverse_alternatives(n.body, visitor, state1, Some(CapturingGroupN(n)))
      #(CapturingGroupNode(..n, body: new_body), state2)
    }
  }
}

/// Traverse a character
fn traverse_character(
  node: CharacterNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(CharacterNode, state) {
  let #(result, state1) = case visitor.on_character {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }
  case result {
    Continue(n) | Skip(n) | Replace(n) -> #(n, state1)
    Remove -> #(node, state1)
  }
}

/// Traverse a character class
fn traverse_character_class(
  node: CharacterClassNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(CharacterClassNode, state) {
  let #(result, state1) = case visitor.on_character_class {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }

  case result {
    Skip(n) -> #(n, state1)
    Remove -> #(node, state1)
    Continue(n) | Replace(n) -> {
      let #(new_body, state2) =
        traverse_cc_elements(n.body, visitor, state1, Some(CharacterClassN(n)))
      #(CharacterClassNode(..n, body: new_body), state2)
    }
  }
}

/// Traverse character class elements
fn traverse_cc_elements(
  elements: List(CharacterClassElement),
  visitor: Visitor(state),
  state: state,
  parent: Option(Node),
) -> #(List(CharacterClassElement), state) {
  let #(new_state, new_list) =
    list.map_fold(elements, state, fn(s, elem) {
      let #(new_elem, new_s) = traverse_cc_element(elem, visitor, s, parent)
      #(new_s, new_elem)
    })
  #(new_list, new_state)
}

/// Traverse a character class element
fn traverse_cc_element(
  element: CharacterClassElement,
  visitor: Visitor(state),
  state: state,
  parent: Option(Node),
) -> #(CharacterClassElement, state) {
  case element {
    CharacterCCE(n) -> {
      let #(new_n, new_state) = traverse_character(n, visitor, state, parent)
      #(CharacterCCE(new_n), new_state)
    }
    CharacterClassCCE(n) -> {
      let #(new_n, new_state) =
        traverse_character_class(n, visitor, state, parent)
      #(CharacterClassCCE(new_n), new_state)
    }
    CharacterClassRangeCCE(n) -> {
      let #(new_n, new_state) =
        traverse_character_class_range(n, visitor, state, parent)
      #(CharacterClassRangeCCE(new_n), new_state)
    }
    CharacterSetCCE(n) -> {
      let #(new_n, new_state) =
        traverse_character_set(n, visitor, state, parent)
      #(CharacterSetCCE(new_n), new_state)
    }
  }
}

/// Traverse a character class range
fn traverse_character_class_range(
  node: CharacterClassRangeNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(CharacterClassRangeNode, state) {
  let #(result, state1) = case visitor.on_character_class_range {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }

  case result {
    Skip(n) -> #(n, state1)
    Remove -> #(node, state1)
    Continue(n) | Replace(n) -> {
      let #(new_min, state2) =
        traverse_character(
          n.min,
          visitor,
          state1,
          Some(CharacterClassRangeN(n)),
        )
      let #(new_max, state3) =
        traverse_character(
          n.max,
          visitor,
          state2,
          Some(CharacterClassRangeN(n)),
        )
      #(CharacterClassRangeNode(min: new_min, max: new_max), state3)
    }
  }
}

/// Traverse a character set
fn traverse_character_set(
  node: CharacterSetNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(CharacterSetNode, state) {
  let #(result, state1) = case visitor.on_character_set {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }
  case result {
    Continue(n) | Skip(n) | Replace(n) -> #(n, state1)
    Remove -> #(node, state1)
  }
}

/// Traverse a directive
fn traverse_directive(
  node: DirectiveNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(DirectiveNode, state) {
  let #(result, state1) = case visitor.on_directive {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }
  case result {
    Continue(n) | Skip(n) | Replace(n) -> #(n, state1)
    Remove -> #(node, state1)
  }
}

/// Traverse flags
fn traverse_flags(
  node: FlagsNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(FlagsNode, state) {
  let #(result, state1) = case visitor.on_flags {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }
  case result {
    Continue(n) | Skip(n) | Replace(n) -> #(n, state1)
    Remove -> #(node, state1)
  }
}

/// Traverse a group
fn traverse_group(
  node: GroupNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(GroupNode, state) {
  let #(result, state1) = case visitor.on_group {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }

  case result {
    Skip(n) -> #(n, state1)
    Remove -> #(node, state1)
    Continue(n) | Replace(n) -> {
      let #(new_body, state2) =
        traverse_alternatives(n.body, visitor, state1, Some(GroupN(n)))
      #(GroupNode(..n, body: new_body), state2)
    }
  }
}

/// Traverse a lookaround assertion
fn traverse_lookaround_assertion(
  node: LookaroundAssertionNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(LookaroundAssertionNode, state) {
  let #(result, state1) = case visitor.on_lookaround_assertion {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }

  case result {
    Skip(n) -> #(n, state1)
    Remove -> #(node, state1)
    Continue(n) | Replace(n) -> {
      let #(new_body, state2) =
        traverse_alternatives(
          n.body,
          visitor,
          state1,
          Some(LookaroundAssertionN(n)),
        )
      #(LookaroundAssertionNode(..n, body: new_body), state2)
    }
  }
}

/// Traverse a named callout
fn traverse_named_callout(
  node: NamedCalloutNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(NamedCalloutNode, state) {
  let #(result, state1) = case visitor.on_named_callout {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }
  case result {
    Continue(n) | Skip(n) | Replace(n) -> #(n, state1)
    Remove -> #(node, state1)
  }
}

/// Traverse a quantifier
fn traverse_quantifier(
  node: QuantifierNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(QuantifierNode, state) {
  let #(result, state1) = case visitor.on_quantifier {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }

  case result {
    Skip(n) -> #(n, state1)
    Remove -> #(node, state1)
    Continue(n) | Replace(n) -> {
      let #(new_body, state2) =
        traverse_quantifiable(n.body, visitor, state1, Some(QuantifierN(n)))
      #(QuantifierNode(..n, body: new_body), state2)
    }
  }
}

/// Traverse a quantifiable node
fn traverse_quantifiable(
  node: QuantifiableNode,
  visitor: Visitor(state),
  state: state,
  parent: Option(Node),
) -> #(QuantifiableNode, state) {
  case node {
    AbsenceFunctionQ(n) -> {
      let #(new_n, new_state) =
        traverse_absence_function(n, visitor, state, parent)
      #(AbsenceFunctionQ(new_n), new_state)
    }
    BackreferenceQ(n) -> {
      let #(new_n, new_state) =
        traverse_backreference(n, visitor, state, parent)
      #(BackreferenceQ(new_n), new_state)
    }
    CapturingGroupQ(n) -> {
      let #(new_n, new_state) =
        traverse_capturing_group(n, visitor, state, parent)
      #(CapturingGroupQ(new_n), new_state)
    }
    CharacterQ(n) -> {
      let #(new_n, new_state) = traverse_character(n, visitor, state, parent)
      #(CharacterQ(new_n), new_state)
    }
    CharacterClassQ(n) -> {
      let #(new_n, new_state) =
        traverse_character_class(n, visitor, state, parent)
      #(CharacterClassQ(new_n), new_state)
    }
    CharacterSetQ(n) -> {
      let #(new_n, new_state) =
        traverse_character_set(n, visitor, state, parent)
      #(CharacterSetQ(new_n), new_state)
    }
    GroupQ(n) -> {
      let #(new_n, new_state) = traverse_group(n, visitor, state, parent)
      #(GroupQ(new_n), new_state)
    }
    QuantifierQ(n) -> {
      let #(new_n, new_state) = traverse_quantifier(n, visitor, state, parent)
      #(QuantifierQ(new_n), new_state)
    }
    SubroutineQ(n) -> {
      let #(new_n, new_state) = traverse_subroutine(n, visitor, state, parent)
      #(SubroutineQ(new_n), new_state)
    }
  }
}

/// Traverse a subroutine
fn traverse_subroutine(
  node: SubroutineNode,
  visitor: Visitor(state),
  state: state,
  _parent: Option(Node),
) -> #(SubroutineNode, state) {
  let #(result, state1) = case visitor.on_subroutine {
    Some(f) -> f(node, state)
    None -> #(Continue(node), state)
  }
  case result {
    Continue(n) | Skip(n) | Replace(n) -> #(n, state1)
    Remove -> #(node, state1)
  }
}
