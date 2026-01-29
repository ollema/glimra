//// AST node types for the Oniguruma regex parser.
//// These types mirror the JS oniguruma-parser library's AST structure.

import gleam/option.{type Option}

// ============================================================================
// Kind enums
// ============================================================================

/// Assertion kinds for boundary/position assertions
pub type AssertionKind {
  LineEnd
  LineStart
  SearchStart
  StringEnd
  StringEndNewline
  StringStart
  TextSegmentBoundary
  WordBoundary
}

/// Absence function kinds (only 'repeater' is currently supported)
pub type AbsenceFunctionKind {
  Repeater
}

/// Character class kinds
pub type CharacterClassKind {
  Union
  Intersection
}

/// Character set kinds
pub type CharacterSetKind {
  Any
  Digit
  Dot
  Hex
  Newline
  Posix
  Property
  Space
  TextSegment
  Word
}

/// Directive kinds
pub type DirectiveKind {
  Keep
  Flags
}

/// Lookaround assertion kinds
pub type LookaroundAssertionKind {
  Lookahead
  Lookbehind
}

/// Quantifier kinds
pub type QuantifierKind {
  Greedy
  Lazy
  Possessive
}

/// Named callout kinds
pub type NamedCalloutKind {
  Count
  Cmp
  ErrorCallout
  Fail
  Max
  Mismatch
  Skip
  TotalCount
  Custom
}

// ============================================================================
// AST Node Types
// ============================================================================

/// The top-level AST node, representing a complete regex pattern
pub type RegexNode {
  RegexNode(body: List(AlternativeNode), flags: FlagsNode)
}

/// The OnigurumaAst is just a RegexNode
pub type OnigurumaAst =
  RegexNode

/// An alternative within a pattern (separated by |)
pub type AlternativeNode {
  AlternativeNode(body: List(AlternativeElement))
}

/// A simple boundary/position assertion like ^, $, \b
pub type AssertionNode {
  AssertionNode(kind: AssertionKind, negate: Option(Bool))
}

/// A backreference like \1, \k<name>
pub type BackreferenceNode {
  BackreferenceNode(ref: BackreferenceRef, orphan: Option(Bool))
}

/// A backreference can refer to a group by number or name
pub type BackreferenceRef {
  NumberedRef(Int)
  NamedRef(String)
}

/// A capturing group like (pattern) or (?<name>pattern)
pub type CapturingGroupNode {
  CapturingGroupNode(
    number: Int,
    name: Option(String),
    is_subroutined: Option(Bool),
    body: List(AlternativeNode),
    /// Optional unique ID for tracking through transforms (set during transform, not parsing)
    transform_id: Option(Int),
  )
}

/// A single character with its code point value
pub type CharacterNode {
  CharacterNode(value: Int)
}

/// A character class like [abc] or [^abc]
pub type CharacterClassNode {
  CharacterClassNode(
    kind: CharacterClassKind,
    negate: Bool,
    body: List(CharacterClassElement),
  )
}

/// A range within a character class like a-z
pub type CharacterClassRangeNode {
  CharacterClassRangeNode(min: CharacterNode, max: CharacterNode)
}

/// A character set like \d, \w, \p{Letter}
pub type CharacterSetNode {
  CharacterSetNode(
    kind: CharacterSetKind,
    value: Option(String),
    negate: Option(Bool),
    variable_length: Option(Bool),
  )
}

/// A directive like \K (keep) or flag modifiers
pub type DirectiveNode {
  DirectiveNode(kind: DirectiveKind, flags: Option(FlagGroupModifiers))
}

/// Flags for the regex pattern
pub type FlagsNode {
  FlagsNode(
    ignore_case: Bool,
    dot_all: Bool,
    extended: Bool,
    digit_is_ascii: Bool,
    posix_is_ascii: Bool,
    space_is_ascii: Bool,
    word_is_ascii: Bool,
    text_segment_mode: Option(TextSegmentMode),
  )
}

/// Text segment mode for flag y{g} or y{w}
pub type TextSegmentMode {
  Grapheme
  WordMode
}

/// A non-capturing group, optionally with flags
pub type GroupNode {
  GroupNode(
    atomic: Option(Bool),
    flags: Option(FlagGroupModifiers),
    body: List(AlternativeNode),
  )
}

/// A lookaround assertion like (?=), (?!), (?<=), (?<!)
pub type LookaroundAssertionNode {
  LookaroundAssertionNode(
    kind: LookaroundAssertionKind,
    negate: Bool,
    body: List(AlternativeNode),
  )
}

/// A named callout like (*FAIL), (*SKIP), etc.
pub type NamedCalloutNode {
  NamedCalloutNode(
    kind: NamedCalloutKind,
    tag: Option(String),
    arguments: Option(List(CalloutArg)),
  )
}

/// A callout argument can be a string or number
pub type CalloutArg {
  StringArg(String)
  NumberArg(Int)
}

/// An absence function like (?~pattern)
pub type AbsenceFunctionNode {
  AbsenceFunctionNode(kind: AbsenceFunctionKind, body: List(AlternativeNode))
}

/// A quantifier like *, +, ?, {n,m}
pub type QuantifierNode {
  QuantifierNode(
    kind: QuantifierKind,
    min: Int,
    max: Int,
    body: QuantifiableNode,
  )
}

/// Max value for unbounded quantifiers
pub const quantifier_max_infinity: Int = 2_147_483_647

/// A subroutine call like \g<1>, \g<name>
pub type SubroutineNode {
  SubroutineNode(ref: SubroutineRef, is_recursive: Option(Bool))
}

/// A subroutine can refer to a group by number or name
pub type SubroutineRef {
  NumberedSubroutineRef(Int)
  NamedSubroutineRef(String)
}

// ============================================================================
// Union Types for AST Traversal
// ============================================================================

/// Any node type in the AST
pub type Node {
  AbsenceFunctionN(AbsenceFunctionNode)
  AlternativeN(AlternativeNode)
  AssertionN(AssertionNode)
  BackreferenceN(BackreferenceNode)
  CapturingGroupN(CapturingGroupNode)
  CharacterN(CharacterNode)
  CharacterClassN(CharacterClassNode)
  CharacterClassRangeN(CharacterClassRangeNode)
  CharacterSetN(CharacterSetNode)
  DirectiveN(DirectiveNode)
  FlagsN(FlagsNode)
  GroupN(GroupNode)
  LookaroundAssertionN(LookaroundAssertionNode)
  NamedCalloutN(NamedCalloutNode)
  QuantifierN(QuantifierNode)
  RegexN(RegexNode)
  SubroutineN(SubroutineNode)
}

/// Elements that can appear in an alternative
pub type AlternativeElement {
  AbsenceFunctionE(AbsenceFunctionNode)
  AssertionE(AssertionNode)
  BackreferenceE(BackreferenceNode)
  CapturingGroupE(CapturingGroupNode)
  CharacterE(CharacterNode)
  CharacterClassE(CharacterClassNode)
  CharacterSetE(CharacterSetNode)
  DirectiveE(DirectiveNode)
  GroupE(GroupNode)
  LookaroundAssertionE(LookaroundAssertionNode)
  NamedCalloutE(NamedCalloutNode)
  QuantifierE(QuantifierNode)
  SubroutineE(SubroutineNode)
}

/// Elements that can appear in a character class
pub type CharacterClassElement {
  CharacterCCE(CharacterNode)
  CharacterClassCCE(CharacterClassNode)
  CharacterClassRangeCCE(CharacterClassRangeNode)
  CharacterSetCCE(CharacterSetNode)
}

/// Nodes that can be quantified
pub type QuantifiableNode {
  AbsenceFunctionQ(AbsenceFunctionNode)
  BackreferenceQ(BackreferenceNode)
  CapturingGroupQ(CapturingGroupNode)
  CharacterQ(CharacterNode)
  CharacterClassQ(CharacterClassNode)
  CharacterSetQ(CharacterSetNode)
  GroupQ(GroupNode)
  QuantifierQ(QuantifierNode)
  SubroutineQ(SubroutineNode)
}

// ============================================================================
// Flag Group Modifiers
// ============================================================================

/// Switches that can be enabled/disabled in a flag group
pub type FlagGroupSwitches {
  FlagGroupSwitches(
    ignore_case: Option(Bool),
    dot_all: Option(Bool),
    extended: Option(Bool),
  )
}

/// Modifiers for a flag group (?imx-imx:...)
pub type FlagGroupModifiers {
  FlagGroupModifiers(
    enable: Option(FlagGroupSwitches),
    disable: Option(FlagGroupSwitches),
  )
}

// ============================================================================
// Helper Functions for Working with Union Types
// ============================================================================

/// Convert an AlternativeElement to a QuantifiableNode if possible
pub fn element_to_quantifiable(
  element: AlternativeElement,
) -> Result(QuantifiableNode, Nil) {
  case element {
    AbsenceFunctionE(n) -> Ok(AbsenceFunctionQ(n))
    BackreferenceE(n) -> Ok(BackreferenceQ(n))
    CapturingGroupE(n) -> Ok(CapturingGroupQ(n))
    CharacterE(n) -> Ok(CharacterQ(n))
    CharacterClassE(n) -> Ok(CharacterClassQ(n))
    CharacterSetE(n) -> Ok(CharacterSetQ(n))
    GroupE(n) -> Ok(GroupQ(n))
    QuantifierE(n) -> Ok(QuantifierQ(n))
    SubroutineE(n) -> Ok(SubroutineQ(n))
    // Not quantifiable
    AssertionE(_) -> Error(Nil)
    DirectiveE(_) -> Error(Nil)
    LookaroundAssertionE(_) -> Error(Nil)
    NamedCalloutE(_) -> Error(Nil)
  }
}
