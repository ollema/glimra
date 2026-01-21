//// Raw grammar types representing TextMate grammar JSON structure
////
//// These types represent the intermediate format after parsing grammar JSON
//// and before compilation into the internal rule types. They closely mirror
//// the IRawGrammar/IRawRule interfaces from vscode-textmate.

import gleam/dict.{type Dict}
import gleam/option.{type Option}
import gleam/string

/// Raw grammar structure as parsed from JSON
pub type RawGrammar {
  RawGrammar(
    /// Root scope name (e.g., "source.javascript")
    scope_name: String,
    /// Display name of the grammar
    name: Option(String),
    /// Top-level patterns
    patterns: List(RawRule),
    /// Repository of named rules
    repository: Dict(String, RawRule),
    /// Injections from this grammar into scope selectors
    injections: Dict(String, RawRule),
    /// Selector for when to apply as injection
    injection_selector: Option(String),
    /// Associated file extensions
    file_types: List(String),
    /// First line match pattern (for detection)
    first_line_match: Option(String),
  )
}

/// Create a new empty raw grammar
pub fn new_raw_grammar(scope_name: String) -> RawGrammar {
  RawGrammar(
    scope_name: scope_name,
    name: option.None,
    patterns: [],
    repository: dict.new(),
    injections: dict.new(),
    injection_selector: option.None,
    file_types: [],
    first_line_match: option.None,
  )
}

/// Raw rule structure from grammar JSON
pub type RawRule {
  RawRule(
    /// Internal ID (assigned during compilation)
    id: Option(Int),
    /// Include directive (e.g., "#name", "$self", "$base", "source.js")
    include: Option(String),
    /// Scope name to apply to the match
    name: Option(String),
    /// Scope name for content between begin/end
    content_name: Option(String),
    /// Single match pattern
    match_pattern: Option(String),
    /// Captures for match pattern
    captures: Dict(String, RawRule),
    /// Begin pattern (for begin/end or begin/while rules)
    begin: Option(String),
    /// Captures for begin pattern
    begin_captures: Dict(String, RawRule),
    /// End pattern (for begin/end rules)
    end: Option(String),
    /// Captures for end pattern
    end_captures: Dict(String, RawRule),
    /// While pattern (for begin/while rules)
    while_pattern: Option(String),
    /// Captures for while pattern
    while_captures: Dict(String, RawRule),
    /// Nested patterns
    patterns: List(RawRule),
    /// Local repository (scoped to this rule)
    repository: Dict(String, RawRule),
    /// Whether to apply end pattern after child patterns
    apply_end_pattern_last: Bool,
  )
}

/// Create a new empty raw rule
pub fn new_raw_rule() -> RawRule {
  RawRule(
    id: option.None,
    include: option.None,
    name: option.None,
    content_name: option.None,
    match_pattern: option.None,
    captures: dict.new(),
    begin: option.None,
    begin_captures: dict.new(),
    end: option.None,
    end_captures: dict.new(),
    while_pattern: option.None,
    while_captures: dict.new(),
    patterns: [],
    repository: dict.new(),
    apply_end_pattern_last: False,
  )
}

/// Create a raw rule with just an include directive
pub fn include_rule(include: String) -> RawRule {
  RawRule(..new_raw_rule(), include: option.Some(include))
}

/// Create a raw rule with a match pattern
pub fn match_rule(
  pattern: String,
  name: Option(String),
  captures: Dict(String, RawRule),
) -> RawRule {
  RawRule(
    ..new_raw_rule(),
    match_pattern: option.Some(pattern),
    name: name,
    captures: captures,
  )
}

/// Create a raw begin/end rule
pub fn begin_end_rule(
  begin: String,
  end: String,
  name: Option(String),
  content_name: Option(String),
  begin_captures: Dict(String, RawRule),
  end_captures: Dict(String, RawRule),
  patterns: List(RawRule),
) -> RawRule {
  RawRule(
    ..new_raw_rule(),
    begin: option.Some(begin),
    end: option.Some(end),
    name: name,
    content_name: content_name,
    begin_captures: begin_captures,
    end_captures: end_captures,
    patterns: patterns,
  )
}

/// Create a raw begin/while rule
pub fn begin_while_rule(
  begin: String,
  while_pat: String,
  name: Option(String),
  content_name: Option(String),
  begin_captures: Dict(String, RawRule),
  while_captures: Dict(String, RawRule),
  patterns: List(RawRule),
) -> RawRule {
  RawRule(
    ..new_raw_rule(),
    begin: option.Some(begin),
    while_pattern: option.Some(while_pat),
    name: name,
    content_name: content_name,
    begin_captures: begin_captures,
    while_captures: while_captures,
    patterns: patterns,
  )
}

/// Types of include references
pub type IncludeReference {
  /// Reference to $self (current grammar root)
  SelfReference
  /// Reference to $base (parent grammar root)
  BaseReference
  /// Reference to local repository rule (e.g., "#name")
  RelativeReference(rule_name: String)
  /// Reference to external grammar root (e.g., "source.js")
  TopLevelReference(scope_name: String)
  /// Reference to external grammar repository (e.g., "source.js#name")
  TopLevelRepositoryReference(scope_name: String, rule_name: String)
}

/// Parse an include string into a typed reference
pub fn parse_include(include: String) -> IncludeReference {
  case include {
    "$self" -> SelfReference
    "$base" -> BaseReference
    _ -> {
      // Check for # character
      case find_hash_index(include, 0) {
        option.None ->
          // No #, this is a top-level reference
          TopLevelReference(include)
        option.Some(0) ->
          // # at position 0, this is a relative reference
          RelativeReference(string_slice_from(include, 1))
        option.Some(index) -> {
          // # in middle, this is a top-level repository reference
          let scope_name = string_slice(include, 0, index)
          let rule_name = string_slice_from(include, index + 1)
          TopLevelRepositoryReference(scope_name, rule_name)
        }
      }
    }
  }
}

/// Find the index of '#' in a string
fn find_hash_index(s: String, start: Int) -> Option(Int) {
  find_hash_index_impl(s, start, 0)
}

fn find_hash_index_impl(s: String, start: Int, current: Int) -> Option(Int) {
  case string_char_at(s, current) {
    option.None -> option.None
    option.Some("#") ->
      case current >= start {
        True -> option.Some(current)
        False -> find_hash_index_impl(s, start, current + 1)
      }
    option.Some(_) -> find_hash_index_impl(s, start, current + 1)
  }
}

/// Get character at index
fn string_char_at(s: String, index: Int) -> Option(String) {
  case string_slice(s, index, index + 1) {
    "" -> option.None
    char -> option.Some(char)
  }
}

/// String slice helper - extracts substring from start index to end index
fn string_slice(s: String, start: Int, end: Int) -> String {
  string.slice(s, start, end - start)
}

/// String slice from start index to end of string
fn string_slice_from(s: String, start: Int) -> String {
  string.drop_start(s, start)
}

/// Initialize a grammar by adding $self and $base repository entries
pub fn init_grammar(
  grammar: RawGrammar,
  base_grammar: Option(RawGrammar),
) -> RawGrammar {
  // Create $self entry pointing to top-level patterns
  let self_rule =
    RawRule(
      ..new_raw_rule(),
      name: option.Some(grammar.scope_name),
      patterns: grammar.patterns,
    )

  // Create $base entry (same as $self if no base)
  let base_rule = case base_grammar {
    option.None -> self_rule
    option.Some(base) ->
      RawRule(
        ..new_raw_rule(),
        name: option.Some(base.scope_name),
        patterns: base.patterns,
      )
  }

  // Add both to repository
  let repository =
    grammar.repository
    |> dict.insert("$self", self_rule)
    |> dict.insert("$base", base_rule)

  RawGrammar(..grammar, repository: repository)
}

/// Merge two repositories (local overrides parent)
pub fn merge_repositories(
  parent: Dict(String, RawRule),
  local: Dict(String, RawRule),
) -> Dict(String, RawRule) {
  dict.merge(parent, local)
}

/// Check if a rule is a match rule (has match pattern, no begin)
pub fn is_match_rule(rule: RawRule) -> Bool {
  option.is_some(rule.match_pattern)
}

/// Check if a rule is a begin/end rule
pub fn is_begin_end_rule(rule: RawRule) -> Bool {
  option.is_some(rule.begin) && option.is_some(rule.end)
}

/// Check if a rule is a begin/while rule
pub fn is_begin_while_rule(rule: RawRule) -> Bool {
  option.is_some(rule.begin) && option.is_some(rule.while_pattern)
}

/// Check if a rule is an include-only rule
pub fn is_include_only_rule(rule: RawRule) -> Bool {
  option.is_some(rule.include)
  || {
    option.is_none(rule.match_pattern)
    && option.is_none(rule.begin)
    && rule.patterns != []
  }
}

/// Get the effective captures for begin pattern
/// Falls back to captures if begin_captures not defined
pub fn get_effective_begin_captures(rule: RawRule) -> Dict(String, RawRule) {
  case dict.size(rule.begin_captures) > 0 {
    True -> rule.begin_captures
    False -> rule.captures
  }
}

/// Get the effective captures for end pattern
/// Falls back to captures if end_captures not defined
pub fn get_effective_end_captures(rule: RawRule) -> Dict(String, RawRule) {
  case dict.size(rule.end_captures) > 0 {
    True -> rule.end_captures
    False -> rule.captures
  }
}

/// Get the effective captures for while pattern
/// Falls back to captures if while_captures not defined
pub fn get_effective_while_captures(rule: RawRule) -> Dict(String, RawRule) {
  case dict.size(rule.while_captures) > 0 {
    True -> rule.while_captures
    False -> rule.captures
  }
}
