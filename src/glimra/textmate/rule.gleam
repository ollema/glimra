//// Pure Gleam implementation of vscode-textmate's Rule types
////
//// TextMate grammars are compiled into a tree of rules. Each rule type handles
//// a different pattern-matching strategy:
//// - MatchRule: Single pattern match with optional captures
//// - BeginEndRule: Paired patterns (begin/end) that create a scope
//// - BeginWhileRule: Begin pattern with while condition for continuation
//// - IncludeOnlyRule: Container for included rules (no pattern of its own)
////
//// CaptureRule is a separate type used for captures within other rules.
////
//// Rules are identified by integer IDs and looked up in a rule registry.

import gleam/option.{type Option, None}

/// Type alias for rule IDs - used throughout the grammar system
pub type RuleId =
  Int

/// Special rule ID representing "no rule" or "unset"
pub const rule_id_none: RuleId = 0

/// The first valid rule ID (rules are assigned IDs starting from 1)
pub const rule_id_first: RuleId = 1

/// Capture index information from regex match
pub type CaptureIndex {
  CaptureIndex(
    /// Start position in the string
    start: Int,
    /// End position in the string
    end: Int,
  )
}

/// A capture rule for sub-tokenizing captured text.
/// This is a separate type because it's referenced from captures arrays
/// in other rule types.
pub type CaptureRule {
  CaptureRule(
    /// Unique identifier for this rule
    id: RuleId,
    /// Optional scope name to apply to the capture
    name: Option(String),
    /// If non-zero, re-tokenize the captured text with this rule
    retokenize_rule_id: RuleId,
  )
}

/// Create a new CaptureRule
pub fn capture_rule(
  id: RuleId,
  name: Option(String),
  retokenize_rule_id: RuleId,
) -> CaptureRule {
  CaptureRule(id: id, name: name, retokenize_rule_id: retokenize_rule_id)
}

/// Represents a grammar rule. Each variant handles a different
/// pattern-matching strategy used in TextMate grammars.
pub type Rule {
  /// A simple match rule that matches a single pattern.
  /// Used for patterns like keywords, operators, etc.
  MatchRule(
    /// Unique identifier for this rule
    id: RuleId,
    /// Optional scope name to apply to the match
    name: Option(String),
    /// The regex pattern source
    match_pattern: String,
    /// Whether the pattern has anchors (\A, \G, \z)
    match_has_anchors: Bool,
    /// Capture rules for named groups
    captures: List(Option(CaptureRule)),
  )
  /// A begin/end rule that creates a scope between two patterns.
  /// Used for constructs like strings, comments, function bodies, etc.
  BeginEndRule(
    /// Unique identifier for this rule
    id: RuleId,
    /// Optional scope name for the entire matched region
    name: Option(String),
    /// Optional scope name for the content between begin and end
    content_name: Option(String),
    /// The begin pattern source
    begin_pattern: String,
    /// Whether the begin pattern has anchors
    begin_has_anchors: Bool,
    /// Capture rules for the begin pattern
    begin_captures: List(Option(CaptureRule)),
    /// The end pattern source (may contain back-references to begin captures)
    end_pattern: String,
    /// Whether the end pattern has back-references (\1, \2, etc.)
    end_has_back_refs: Bool,
    /// Whether the end pattern has anchors
    end_has_anchors: Bool,
    /// Capture rules for the end pattern
    end_captures: List(Option(CaptureRule)),
    /// IDs of child rules to match within begin/end region
    patterns: List(RuleId),
    /// If true, try to match end pattern after child patterns
    apply_end_pattern_last: Bool,
    /// Whether any included patterns are missing (unresolved includes)
    has_missing_patterns: Bool,
  )
  /// A begin/while rule that continues matching while a condition holds.
  /// Used for line-continuation patterns like heredocs, multi-line strings.
  BeginWhileRule(
    /// Unique identifier for this rule
    id: RuleId,
    /// Optional scope name for the entire matched region
    name: Option(String),
    /// Optional scope name for the content
    content_name: Option(String),
    /// The begin pattern source
    begin_pattern: String,
    /// Whether the begin pattern has anchors
    begin_has_anchors: Bool,
    /// Capture rules for the begin pattern
    begin_captures: List(Option(CaptureRule)),
    /// The while pattern source (checked at start of each line)
    while_pattern: String,
    /// Whether the while pattern has back-references
    while_has_back_refs: Bool,
    /// Whether the while pattern has anchors
    while_has_anchors: Bool,
    /// Capture rules for the while pattern
    while_captures: List(Option(CaptureRule)),
    /// IDs of child rules to match within the region
    patterns: List(RuleId),
    /// Whether any included patterns are missing
    has_missing_patterns: Bool,
  )
  /// A rule that only contains included patterns (no match of its own).
  /// Used for grouping rules and implementing "include" references.
  IncludeOnlyRule(
    /// Unique identifier for this rule
    id: RuleId,
    /// Optional scope name (typically None for include-only)
    name: Option(String),
    /// IDs of child rules
    patterns: List(RuleId),
    /// Whether any included patterns are missing
    has_missing_patterns: Bool,
  )
}

/// Get the rule ID
pub fn get_id(rule: Rule) -> RuleId {
  case rule {
    MatchRule(id, ..) -> id
    BeginEndRule(id, ..) -> id
    BeginWhileRule(id, ..) -> id
    IncludeOnlyRule(id, ..) -> id
  }
}

/// Update the rule ID (used for include resolution with pre-allocated IDs)
pub fn update_id(rule: Rule, new_id: RuleId) -> Rule {
  case rule {
    MatchRule(
      id: _,
      name: n,
      match_pattern: mp,
      match_has_anchors: mha,
      captures: c,
    ) ->
      MatchRule(
        id: new_id,
        name: n,
        match_pattern: mp,
        match_has_anchors: mha,
        captures: c,
      )
    BeginEndRule(
      id: _,
      name: n,
      content_name: cn,
      begin_pattern: bp,
      begin_has_anchors: bha,
      begin_captures: bc,
      end_pattern: ep,
      end_has_back_refs: ehbr,
      end_has_anchors: eha,
      end_captures: ec,
      patterns: p,
      apply_end_pattern_last: aepl,
      has_missing_patterns: hmp,
    ) ->
      BeginEndRule(
        id: new_id,
        name: n,
        content_name: cn,
        begin_pattern: bp,
        begin_has_anchors: bha,
        begin_captures: bc,
        end_pattern: ep,
        end_has_back_refs: ehbr,
        end_has_anchors: eha,
        end_captures: ec,
        patterns: p,
        apply_end_pattern_last: aepl,
        has_missing_patterns: hmp,
      )
    BeginWhileRule(
      id: _,
      name: n,
      content_name: cn,
      begin_pattern: bp,
      begin_has_anchors: bha,
      begin_captures: bc,
      while_pattern: wp,
      while_has_back_refs: whbr,
      while_has_anchors: wha,
      while_captures: wc,
      patterns: p,
      has_missing_patterns: hmp,
    ) ->
      BeginWhileRule(
        id: new_id,
        name: n,
        content_name: cn,
        begin_pattern: bp,
        begin_has_anchors: bha,
        begin_captures: bc,
        while_pattern: wp,
        while_has_back_refs: whbr,
        while_has_anchors: wha,
        while_captures: wc,
        patterns: p,
        has_missing_patterns: hmp,
      )
    IncludeOnlyRule(id: _, name: n, patterns: p, has_missing_patterns: hmp) ->
      IncludeOnlyRule(
        id: new_id,
        name: n,
        patterns: p,
        has_missing_patterns: hmp,
      )
  }
}

/// Get the name scope of a rule (if any).
/// For most rules this is a simple lookup. For rules with captures,
/// the name may contain references like $1, $2 that need resolution.
pub fn get_name(rule: Rule) -> Option(String) {
  case rule {
    MatchRule(name: n, ..) -> n
    BeginEndRule(name: n, ..) -> n
    BeginWhileRule(name: n, ..) -> n
    IncludeOnlyRule(name: n, ..) -> n
  }
}

/// Get the content name scope of a rule (if any).
/// Only BeginEndRule and BeginWhileRule have content names.
pub fn get_content_name(rule: Rule) -> Option(String) {
  case rule {
    BeginEndRule(content_name: n, ..) -> n
    BeginWhileRule(content_name: n, ..) -> n
    _ -> None
  }
}

/// Get the captures list for a MatchRule
pub fn get_match_captures(rule: Rule) -> List(Option(CaptureRule)) {
  case rule {
    MatchRule(captures: c, ..) -> c
    _ -> []
  }
}

/// Get the begin captures for a BeginEndRule or BeginWhileRule
pub fn get_begin_captures(rule: Rule) -> List(Option(CaptureRule)) {
  case rule {
    BeginEndRule(begin_captures: c, ..) -> c
    BeginWhileRule(begin_captures: c, ..) -> c
    _ -> []
  }
}

/// Get the end captures for a BeginEndRule
pub fn get_end_captures(rule: Rule) -> List(Option(CaptureRule)) {
  case rule {
    BeginEndRule(end_captures: c, ..) -> c
    _ -> []
  }
}

/// Get the while captures for a BeginWhileRule
pub fn get_while_captures(rule: Rule) -> List(Option(CaptureRule)) {
  case rule {
    BeginWhileRule(while_captures: c, ..) -> c
    _ -> []
  }
}

/// Check if this is a BeginEndRule
pub fn is_begin_end_rule(rule: Rule) -> Bool {
  case rule {
    BeginEndRule(..) -> True
    _ -> False
  }
}

/// Check if this is a BeginWhileRule
pub fn is_begin_while_rule(rule: Rule) -> Bool {
  case rule {
    BeginWhileRule(..) -> True
    _ -> False
  }
}

/// Check if this is a MatchRule
pub fn is_match_rule(rule: Rule) -> Bool {
  case rule {
    MatchRule(..) -> True
    _ -> False
  }
}

/// Check if this is an IncludeOnlyRule
pub fn is_include_only_rule(rule: Rule) -> Bool {
  case rule {
    IncludeOnlyRule(..) -> True
    _ -> False
  }
}

/// Create a new MatchRule
pub fn match_rule(
  id: RuleId,
  name: Option(String),
  match_pattern: String,
  match_has_anchors: Bool,
  captures: List(Option(CaptureRule)),
) -> Rule {
  MatchRule(
    id: id,
    name: name,
    match_pattern: match_pattern,
    match_has_anchors: match_has_anchors,
    captures: captures,
  )
}

/// Create a new IncludeOnlyRule
pub fn include_only_rule(
  id: RuleId,
  name: Option(String),
  patterns: List(RuleId),
  has_missing_patterns: Bool,
) -> Rule {
  IncludeOnlyRule(
    id: id,
    name: name,
    patterns: patterns,
    has_missing_patterns: has_missing_patterns,
  )
}

/// Create a new BeginEndRule
pub fn begin_end_rule(
  id: RuleId,
  name: Option(String),
  content_name: Option(String),
  begin_pattern: String,
  begin_has_anchors: Bool,
  begin_captures: List(Option(CaptureRule)),
  end_pattern: String,
  end_has_back_refs: Bool,
  end_has_anchors: Bool,
  end_captures: List(Option(CaptureRule)),
  patterns: List(RuleId),
  apply_end_pattern_last: Bool,
  has_missing_patterns: Bool,
) -> Rule {
  BeginEndRule(
    id: id,
    name: name,
    content_name: content_name,
    begin_pattern: begin_pattern,
    begin_has_anchors: begin_has_anchors,
    begin_captures: begin_captures,
    end_pattern: end_pattern,
    end_has_back_refs: end_has_back_refs,
    end_has_anchors: end_has_anchors,
    end_captures: end_captures,
    patterns: patterns,
    apply_end_pattern_last: apply_end_pattern_last,
    has_missing_patterns: has_missing_patterns,
  )
}

/// Create a new BeginWhileRule
pub fn begin_while_rule(
  id: RuleId,
  name: Option(String),
  content_name: Option(String),
  begin_pattern: String,
  begin_has_anchors: Bool,
  begin_captures: List(Option(CaptureRule)),
  while_pattern: String,
  while_has_back_refs: Bool,
  while_has_anchors: Bool,
  while_captures: List(Option(CaptureRule)),
  patterns: List(RuleId),
  has_missing_patterns: Bool,
) -> Rule {
  BeginWhileRule(
    id: id,
    name: name,
    content_name: content_name,
    begin_pattern: begin_pattern,
    begin_has_anchors: begin_has_anchors,
    begin_captures: begin_captures,
    while_pattern: while_pattern,
    while_has_back_refs: while_has_back_refs,
    while_has_anchors: while_has_anchors,
    while_captures: while_captures,
    patterns: patterns,
    has_missing_patterns: has_missing_patterns,
  )
}
