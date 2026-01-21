//// Grammar structure and rule registry for TextMate tokenization
////
//// The Grammar holds all compiled rules and provides methods for
//// rule lookup and pattern matching during tokenization.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import glimra/ffi/cache
import glimra/ffi/onig_scanner.{type OnigScanner, MatchResult}
import glimra/textmate/regexp_source.{resolve_anchors, resolve_back_references}
import glimra/textmate/rule.{
  type CaptureIndex, type Rule, type RuleId, BeginEndRule, BeginWhileRule,
  IncludeOnlyRule, MatchRule,
}

/// Special rule ID used to indicate end pattern matching
pub const end_rule_id: Int = -1

/// A compiled grammar containing all rules and metadata
pub type Grammar {
  Grammar(
    /// The root scope name (e.g., "source.javascript")
    scope_name: String,
    /// The ID of the root rule (entry point for tokenization)
    root_rule_id: RuleId,
    /// Registry of all rules by ID
    rules: Dict(RuleId, Rule),
    /// Injections from other grammars
    injections: List(Injection),
    /// Factory function for creating OnigScanner instances
    scanner_factory: fn(List(String)) -> OnigScanner,
  )
}

/// An injection that adds rules from one grammar into another
pub type Injection {
  Injection(
    /// Selector expression for when this injection applies
    selector: String,
    /// Function to test if scopes match the selector
    matcher: fn(List(String)) -> Bool,
    /// Priority: -1 for high priority, 0 for normal, 1 for low
    priority: Int,
    /// The rule ID to inject
    rule_id: RuleId,
  )
}

/// Get a rule by its ID from the grammar
pub fn get_rule(grammar: Grammar, rule_id: RuleId) -> Option(Rule) {
  dict.get(grammar.rules, rule_id)
  |> option.from_result
}

/// A compiled pattern set ready for matching
/// This combines multiple regex patterns into a single scanner
pub type CompiledRule {
  CompiledRule(
    /// The OnigScanner instance for pattern matching
    scanner: OnigScanner,
    /// The rule IDs corresponding to each pattern (in order)
    rule_ids: List(Int),
    /// The original patterns (for debugging)
    patterns: List(String),
  )
}

/// Result of finding the next match
pub type FindMatchResult {
  FindMatchResult(
    /// The rule ID that matched (or end_rule_id for end patterns)
    rule_id: Int,
    /// Capture indices from the match
    capture_indices: List(CaptureIndex),
  )
}

/// Find the next match using a compiled rule
pub fn find_next_match(
  compiled: CompiledRule,
  text: String,
  start_pos: Int,
  allow_a: Bool,
  allow_g: Bool,
) -> Option(FindMatchResult) {
  // Compute find options based on anchor permissions
  let options = compute_find_options(start_pos, allow_a, allow_g)

  case
    onig_scanner.find_next_match(compiled.scanner, text, start_pos, options)
  {
    None -> None
    Some(MatchResult(index, captures)) -> {
      // Map the pattern index to a rule ID
      case list_at(compiled.rule_ids, index) {
        Error(Nil) -> None
        Ok(rule_id) -> Some(FindMatchResult(rule_id, captures))
      }
    }
  }
}

/// Compute OnigScanner find options from anchor permissions
fn compute_find_options(_start_pos: Int, allow_a: Bool, allow_g: Bool) -> Int {
  let not_begin_string = case allow_a {
    True -> 0
    False -> 1
  }
  // NotBeginPosition is set when we're not at anchor position (allow_g=false)
  let not_begin_position = case allow_g {
    True -> 0
    False -> 4
  }
  not_begin_string + not_begin_position
}

/// Helper to get element at index from a list
fn list_at(lst: List(a), index: Int) -> Result(a, Nil) {
  lst
  |> list.drop(index)
  |> list.first
}

/// Get the ID from any Rule variant
fn get_rule_id(rule: Rule) -> RuleId {
  case rule {
    MatchRule(id: id, ..) -> id
    BeginEndRule(id: id, ..) -> id
    BeginWhileRule(id: id, ..) -> id
    IncludeOnlyRule(id: id, ..) -> id
  }
}

/// Create a cache key for compiled patterns
/// Includes grammar scope_name to disambiguate between grammars with same rule IDs
fn make_cache_key(
  grammar_scope: String,
  rule_id: RuleId,
  end_regex: Option(String),
  allow_a: Bool,
  allow_g: Bool,
) -> String {
  let end_str = case end_regex {
    None -> ""
    Some(s) -> s
  }
  let a_str = case allow_a {
    True -> "1"
    False -> "0"
  }
  let g_str = case allow_g {
    True -> "1"
    False -> "0"
  }
  grammar_scope
  <> ":"
  <> int.to_string(rule_id)
  <> ":"
  <> end_str
  <> ":"
  <> a_str
  <> ":"
  <> g_str
}

/// Compile patterns for a rule, resolving anchors based on context
/// Uses caching to avoid recompilation of the same patterns
pub fn compile_rule_patterns(
  grammar: Grammar,
  rule: Rule,
  end_regex_source: Option(String),
  allow_a: Bool,
  allow_g: Bool,
) -> CompiledRule {
  // Check cache first (include grammar scope to disambiguate between grammars)
  let cache_key =
    make_cache_key(
      grammar.scope_name,
      get_rule_id(rule),
      end_regex_source,
      allow_a,
      allow_g,
    )
  case cache.get(cache_key) {
    Some(cached) -> cached
    None -> {
      // Not in cache, compile and cache the result
      let #(patterns, rule_ids) =
        collect_patterns(grammar, rule, end_regex_source, allow_a, allow_g)
      let scanner = grammar.scanner_factory(patterns)
      let compiled =
        CompiledRule(scanner: scanner, rule_ids: rule_ids, patterns: patterns)
      cache.put(cache_key, compiled)
      compiled
    }
  }
}

/// Collect all patterns and their rule IDs for a rule
fn collect_patterns(
  grammar: Grammar,
  rule: Rule,
  end_regex_source: Option(String),
  allow_a: Bool,
  allow_g: Bool,
) -> #(List(String), List(Int)) {
  case rule {
    BeginEndRule(patterns: pattern_ids, apply_end_pattern_last: apply_last, ..) -> {
      // Collect child patterns
      let child_patterns =
        collect_child_patterns(grammar, pattern_ids, allow_a, allow_g)

      // Get end pattern if provided
      case end_regex_source {
        None -> child_patterns
        Some(end_pattern) -> {
          let resolved_end = resolve_anchors(end_pattern, allow_a, allow_g)
          case apply_last {
            // End pattern comes first (default)
            False -> {
              let #(patterns, ids) = child_patterns
              #([resolved_end, ..patterns], [end_rule_id, ..ids])
            }
            // End pattern comes last
            True -> {
              let #(patterns, ids) = child_patterns
              #(
                list.append(patterns, [resolved_end]),
                list.append(ids, [end_rule_id]),
              )
            }
          }
        }
      }
    }

    BeginWhileRule(patterns: pattern_ids, ..) -> {
      // BeginWhileRule only has child patterns (while is checked separately)
      collect_child_patterns(grammar, pattern_ids, allow_a, allow_g)
    }

    IncludeOnlyRule(patterns: pattern_ids, ..) -> {
      collect_child_patterns(grammar, pattern_ids, allow_a, allow_g)
    }

    MatchRule(
      match_pattern: pattern,
      id: rule_id,
      match_has_anchors: has_anchors,
      ..,
    ) -> {
      let resolved = case has_anchors {
        True -> resolve_anchors(pattern, allow_a, allow_g)
        False -> pattern
      }
      #([resolved], [rule_id])
    }
  }
}

/// Collect patterns from child rules
fn collect_child_patterns(
  grammar: Grammar,
  pattern_ids: List(RuleId),
  allow_a: Bool,
  allow_g: Bool,
) -> #(List(String), List(Int)) {
  collect_child_patterns_impl(grammar, pattern_ids, allow_a, allow_g, [], [])
}

fn collect_child_patterns_impl(
  grammar: Grammar,
  pattern_ids: List(RuleId),
  allow_a: Bool,
  allow_g: Bool,
  patterns_acc: List(String),
  ids_acc: List(Int),
) -> #(List(String), List(Int)) {
  case pattern_ids {
    [] -> #(list.reverse(patterns_acc), list.reverse(ids_acc))
    [rule_id, ..rest] -> {
      case get_rule(grammar, rule_id) {
        None ->
          collect_child_patterns_impl(
            grammar,
            rest,
            allow_a,
            allow_g,
            patterns_acc,
            ids_acc,
          )
        Some(rule) -> {
          let #(new_patterns, new_ids) =
            get_rule_patterns(grammar, rule, rule_id, allow_a, allow_g)
          collect_child_patterns_impl(
            grammar,
            rest,
            allow_a,
            allow_g,
            list.append(list.reverse(new_patterns), patterns_acc),
            list.append(list.reverse(new_ids), ids_acc),
          )
        }
      }
    }
  }
}

/// Get the patterns for a single rule (the begin pattern or match pattern)
/// For IncludeOnlyRules, this recurses into their children
fn get_rule_patterns(
  grammar: Grammar,
  rule: Rule,
  rule_id: RuleId,
  allow_a: Bool,
  allow_g: Bool,
) -> #(List(String), List(Int)) {
  case rule {
    MatchRule(match_pattern: pattern, match_has_anchors: has_anchors, ..) -> {
      let resolved = case has_anchors {
        True -> resolve_anchors(pattern, allow_a, allow_g)
        False -> pattern
      }
      #([resolved], [rule_id])
    }

    BeginEndRule(begin_pattern: pattern, begin_has_anchors: has_anchors, ..) -> {
      let resolved = case has_anchors {
        True -> resolve_anchors(pattern, allow_a, allow_g)
        False -> pattern
      }
      #([resolved], [rule_id])
    }

    BeginWhileRule(begin_pattern: pattern, begin_has_anchors: has_anchors, ..) -> {
      let resolved = case has_anchors {
        True -> resolve_anchors(pattern, allow_a, allow_g)
        False -> pattern
      }
      #([resolved], [rule_id])
    }

    IncludeOnlyRule(patterns: child_pattern_ids, ..) -> {
      // Recursively collect patterns from child rules
      collect_child_patterns(grammar, child_pattern_ids, allow_a, allow_g)
    }
  }
}

/// Compile while pattern for a BeginWhileRule
pub fn compile_while_pattern(
  grammar: Grammar,
  rule: Rule,
  resolved_while: String,
  allow_a: Bool,
  allow_g: Bool,
) -> CompiledRule {
  case rule {
    BeginWhileRule(id: rule_id, while_has_anchors: has_anchors, ..) -> {
      let pattern = case has_anchors {
        True -> resolve_anchors(resolved_while, allow_a, allow_g)
        False -> resolved_while
      }
      let scanner = grammar.scanner_factory([pattern])
      CompiledRule(scanner: scanner, rule_ids: [rule_id], patterns: [pattern])
    }
    _ -> {
      // Invalid - not a BeginWhileRule
      let scanner = grammar.scanner_factory([])
      CompiledRule(scanner: scanner, rule_ids: [], patterns: [])
    }
  }
}

/// Resolve back-references in an end pattern using captures from the begin match
pub fn resolve_end_pattern(
  end_pattern: String,
  end_has_back_refs: Bool,
  line_text: String,
  capture_indices: List(CaptureIndex),
) -> String {
  case end_has_back_refs {
    False -> end_pattern
    True -> resolve_back_references(end_pattern, line_text, capture_indices)
  }
}

/// Resolve back-references in a while pattern using captures from the begin match
pub fn resolve_while_pattern(
  while_pattern: String,
  while_has_back_refs: Bool,
  line_text: String,
  capture_indices: List(CaptureIndex),
) -> String {
  case while_has_back_refs {
    False -> while_pattern
    True -> resolve_back_references(while_pattern, line_text, capture_indices)
  }
}
