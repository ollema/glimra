//// Grammar compilation - transforms raw grammar into compiled rules
////
//// This module implements the RuleFactory pattern from vscode-textmate,
//// compiling IRawRule structures into the internal Rule types with
//// proper ID assignment and include resolution.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import glimra/ffi/onig_scanner.{type OnigScanner}
import glimra/textmate/grammar.{type Grammar, type Injection, Grammar, Injection}
import glimra/textmate/raw_grammar.{
  type RawGrammar, type RawRule, BaseReference, RelativeReference, SelfReference,
  TopLevelReference, TopLevelRepositoryReference, get_effective_begin_captures,
  get_effective_end_captures, get_effective_while_captures, init_grammar,
  is_begin_end_rule, is_begin_while_rule, is_match_rule, merge_repositories,
  parse_include,
}
import glimra/textmate/regexp_source.{
  detect_anchors, has_any_anchor, has_back_references,
}
import glimra/textmate/rule.{
  type CaptureRule, type Rule, type RuleId, begin_end_rule, begin_while_rule,
  capture_rule, include_only_rule, match_rule, rule_id_first, rule_id_none,
  update_id as update_rule_id,
}
import glimra/textmate/scope_selector

/// Compilation state tracking IDs and rules
type CompilerState {
  CompilerState(
    /// Next rule ID to assign
    next_id: Int,
    /// Registry of compiled rules
    rules: Dict(Int, Rule),
    /// External grammar loader (optional)
    external_grammars: Dict(String, RawGrammar),
    /// Track compiled includes to prevent infinite recursion
    /// Maps include reference string to its rule ID
    compiled_includes: Dict(String, RuleId),
    /// Track includes currently being compiled to detect cycles
    compiling_includes: List(String),
    /// Current grammar scope for namespacing include keys
    /// e.g., "text.html.basic" or "source.js"
    scope_context: String,
  )
}

/// Create a new compiler state
fn new_compiler_state() -> CompilerState {
  CompilerState(
    next_id: rule_id_first,
    rules: dict.new(),
    external_grammars: dict.new(),
    compiled_includes: dict.new(),
    compiling_includes: [],
    scope_context: "",
  )
}

/// Register a new rule and return its ID
fn register_rule(state: CompilerState, rule: Rule) -> #(CompilerState, RuleId) {
  let id = state.next_id
  let rules = dict.insert(state.rules, id, rule)
  let new_state = CompilerState(..state, next_id: id + 1, rules: rules)
  #(new_state, id)
}

/// Allocate a rule ID without registering a rule yet
/// Used for handling cyclic includes
fn allocate_rule_id(state: CompilerState) -> #(CompilerState, RuleId) {
  let id = state.next_id
  let new_state = CompilerState(..state, next_id: id + 1)
  #(new_state, id)
}

/// Register a rule with a specific pre-allocated ID
fn register_rule_with_id(
  state: CompilerState,
  id: RuleId,
  rule: Rule,
) -> CompilerState {
  CompilerState(..state, rules: dict.insert(state.rules, id, rule))
}

/// Compile a raw grammar into a Grammar structure
pub fn compile_grammar(
  raw: RawGrammar,
  external_grammars: Dict(String, RawGrammar),
  scanner_factory: fn(List(String)) -> OnigScanner,
) -> Grammar {
  // Initialize grammar with $self and $base
  let raw = init_grammar(raw, None)

  // Start compilation with external grammars and scope context
  let state =
    CompilerState(
      ..new_compiler_state(),
      external_grammars: external_grammars,
      scope_context: raw.scope_name,
    )

  // Compile the root rule (patterns at top level)
  let #(state, root_rule_id) =
    compile_patterns_rule(
      state,
      raw.patterns,
      raw.repository,
      Some(raw.scope_name),
      None,
    )

  // Compile injections
  let #(state, injections) = compile_injections(state, raw, raw.repository)

  Grammar(
    scope_name: raw.scope_name,
    root_rule_id: root_rule_id,
    rules: state.rules,
    injections: injections,
    scanner_factory: scanner_factory,
  )
}

/// Compile a patterns-only rule (IncludeOnlyRule)
fn compile_patterns_rule(
  state: CompilerState,
  patterns: List(RawRule),
  repository: Dict(String, RawRule),
  name: Option(String),
  _content_name: Option(String),
) -> #(CompilerState, RuleId) {
  // Compile child patterns
  let #(state, pattern_ids, has_missing) =
    compile_patterns(state, patterns, repository)

  // Create the rule
  let rule =
    include_only_rule(
      rule_id_none,
      // Will be replaced
      name,
      pattern_ids,
      has_missing,
    )

  // Register and get real ID
  let #(state, id) = register_rule(state, rule)

  // Update rule with correct ID
  let updated_rule = include_only_rule(id, name, pattern_ids, has_missing)
  let state =
    CompilerState(..state, rules: dict.insert(state.rules, id, updated_rule))

  #(state, id)
}

/// Compile a raw rule into a Rule, returning its ID
fn compile_rule(
  state: CompilerState,
  raw: RawRule,
  repository: Dict(String, RawRule),
) -> #(CompilerState, RuleId) {
  // Check for include directive first
  case raw.include {
    Some(include_str) -> resolve_include(state, include_str, repository)
    None -> compile_rule_by_type(state, raw, repository)
  }
}

/// Compile a rule based on its type (match, begin/end, begin/while, include-only)
fn compile_rule_by_type(
  state: CompilerState,
  raw: RawRule,
  repository: Dict(String, RawRule),
) -> #(CompilerState, RuleId) {
  // Merge local repository with parent
  let merged_repo = merge_repositories(repository, raw.repository)

  case is_match_rule(raw) {
    True -> compile_match_rule(state, raw, merged_repo)
    False ->
      case is_begin_while_rule(raw) {
        True -> compile_begin_while_rule(state, raw, merged_repo)
        False ->
          case is_begin_end_rule(raw) {
            True -> compile_begin_end_rule(state, raw, merged_repo)
            False -> compile_include_only_rule(state, raw, merged_repo)
          }
      }
  }
}

/// Compile a MatchRule
fn compile_match_rule(
  state: CompilerState,
  raw: RawRule,
  repository: Dict(String, RawRule),
) -> #(CompilerState, RuleId) {
  let pattern = option.unwrap(raw.match_pattern, "")

  // Compile captures
  let #(state, captures) = compile_captures(state, raw.captures, repository)

  // Check for anchors
  let anchors = detect_anchors(pattern)
  let has_anchors = has_any_anchor(anchors)

  // Create rule with placeholder ID
  let rule = match_rule(rule_id_none, raw.name, pattern, has_anchors, captures)

  // Register and get real ID
  let #(state, id) = register_rule(state, rule)

  // Update rule with correct ID
  let updated_rule = match_rule(id, raw.name, pattern, has_anchors, captures)
  let state =
    CompilerState(..state, rules: dict.insert(state.rules, id, updated_rule))

  #(state, id)
}

/// Compile a BeginEndRule
fn compile_begin_end_rule(
  state: CompilerState,
  raw: RawRule,
  repository: Dict(String, RawRule),
) -> #(CompilerState, RuleId) {
  let begin_pattern = option.unwrap(raw.begin, "")
  let end_pattern = option.unwrap(raw.end, "")

  // Compile captures
  let begin_caps = get_effective_begin_captures(raw)
  let end_caps = get_effective_end_captures(raw)
  let #(state, begin_captures) = compile_captures(state, begin_caps, repository)
  let #(state, end_captures) = compile_captures(state, end_caps, repository)

  // Compile child patterns
  let #(state, pattern_ids, has_missing) =
    compile_patterns(state, raw.patterns, repository)

  // Check anchors and back-references
  let begin_anchors = detect_anchors(begin_pattern)
  let end_anchors = detect_anchors(end_pattern)
  let begin_has_anchors = has_any_anchor(begin_anchors)
  let end_has_anchors = has_any_anchor(end_anchors)
  let end_has_back_refs = has_back_references(end_pattern)

  // Create rule
  let rule =
    begin_end_rule(
      rule_id_none,
      raw.name,
      raw.content_name,
      begin_pattern,
      begin_has_anchors,
      begin_captures,
      end_pattern,
      end_has_back_refs,
      end_has_anchors,
      end_captures,
      pattern_ids,
      raw.apply_end_pattern_last,
      has_missing,
    )

  // Register and update
  let #(state, id) = register_rule(state, rule)
  let updated_rule =
    begin_end_rule(
      id,
      raw.name,
      raw.content_name,
      begin_pattern,
      begin_has_anchors,
      begin_captures,
      end_pattern,
      end_has_back_refs,
      end_has_anchors,
      end_captures,
      pattern_ids,
      raw.apply_end_pattern_last,
      has_missing,
    )
  let state =
    CompilerState(..state, rules: dict.insert(state.rules, id, updated_rule))

  #(state, id)
}

/// Compile a BeginWhileRule
fn compile_begin_while_rule(
  state: CompilerState,
  raw: RawRule,
  repository: Dict(String, RawRule),
) -> #(CompilerState, RuleId) {
  let begin_pattern = option.unwrap(raw.begin, "")
  let while_pattern = option.unwrap(raw.while_pattern, "")

  // Compile captures
  let begin_caps = get_effective_begin_captures(raw)
  let while_caps = get_effective_while_captures(raw)
  let #(state, begin_captures) = compile_captures(state, begin_caps, repository)
  let #(state, while_captures) = compile_captures(state, while_caps, repository)

  // Compile child patterns
  let #(state, pattern_ids, has_missing) =
    compile_patterns(state, raw.patterns, repository)

  // Check anchors and back-references
  let begin_anchors = detect_anchors(begin_pattern)
  let while_anchors = detect_anchors(while_pattern)
  let begin_has_anchors = has_any_anchor(begin_anchors)
  let while_has_anchors = has_any_anchor(while_anchors)
  let while_has_back_refs = has_back_references(while_pattern)

  // Create rule
  let rule =
    begin_while_rule(
      rule_id_none,
      raw.name,
      raw.content_name,
      begin_pattern,
      begin_has_anchors,
      begin_captures,
      while_pattern,
      while_has_back_refs,
      while_has_anchors,
      while_captures,
      pattern_ids,
      has_missing,
    )

  // Register and update
  let #(state, id) = register_rule(state, rule)
  let updated_rule =
    begin_while_rule(
      id,
      raw.name,
      raw.content_name,
      begin_pattern,
      begin_has_anchors,
      begin_captures,
      while_pattern,
      while_has_back_refs,
      while_has_anchors,
      while_captures,
      pattern_ids,
      has_missing,
    )
  let state =
    CompilerState(..state, rules: dict.insert(state.rules, id, updated_rule))

  #(state, id)
}

/// Compile an IncludeOnlyRule (patterns container)
fn compile_include_only_rule(
  state: CompilerState,
  raw: RawRule,
  repository: Dict(String, RawRule),
) -> #(CompilerState, RuleId) {
  // Compile child patterns
  let #(state, pattern_ids, has_missing) =
    compile_patterns(state, raw.patterns, repository)

  // Create rule
  let rule = include_only_rule(rule_id_none, raw.name, pattern_ids, has_missing)

  // Register and update
  let #(state, id) = register_rule(state, rule)
  let updated_rule = include_only_rule(id, raw.name, pattern_ids, has_missing)
  let state =
    CompilerState(..state, rules: dict.insert(state.rules, id, updated_rule))

  #(state, id)
}

/// Compile a list of patterns into rule IDs
fn compile_patterns(
  state: CompilerState,
  patterns: List(RawRule),
  repository: Dict(String, RawRule),
) -> #(CompilerState, List(RuleId), Bool) {
  compile_patterns_impl(state, patterns, repository, [], False)
}

fn compile_patterns_impl(
  state: CompilerState,
  patterns: List(RawRule),
  repository: Dict(String, RawRule),
  acc: List(RuleId),
  has_missing: Bool,
) -> #(CompilerState, List(RuleId), Bool) {
  case patterns {
    [] -> #(state, list.reverse(acc), has_missing)
    [pattern, ..rest] -> {
      let #(state, rule_id) = compile_rule(state, pattern, repository)
      let new_has_missing = has_missing || rule_id == rule_id_none
      let new_acc = case rule_id == rule_id_none {
        True -> acc
        False -> [rule_id, ..acc]
      }
      compile_patterns_impl(state, rest, repository, new_acc, new_has_missing)
    }
  }
}

/// Resolve an include reference
///
/// This handles cyclic includes by pre-allocating a rule ID before compiling
/// the include's content. When a cyclic reference is encountered, it returns
/// the pre-allocated ID instead of failing.
fn resolve_include(
  state: CompilerState,
  include_str: String,
  repository: Dict(String, RawRule),
) -> #(CompilerState, RuleId) {
  // Create a namespaced cache key to avoid collisions between grammars
  // e.g., HTML's "#comment" and JS's "#comment" get different keys:
  //   "text.html.basic::#comment" vs "source.js::#comment"
  let cache_key = state.scope_context <> "::" <> include_str

  // Check if this include was already compiled (or is being compiled - cyclic reference)
  case dict.get(state.compiled_includes, cache_key) {
    Ok(existing_id) -> #(state, existing_id)
    Error(Nil) -> {
      // Pre-allocate an ID for this include before compiling
      // This allows cyclic references to find this ID
      let #(state, placeholder_id) = allocate_rule_id(state)

      // Store the pre-allocated ID so cyclic references can find it
      let state =
        CompilerState(
          ..state,
          compiled_includes: dict.insert(
            state.compiled_includes,
            cache_key,
            placeholder_id,
          ),
        )

      // Now compile the actual include with the pre-allocated ID
      // Any cyclic references will find placeholder_id in compiled_includes
      resolve_include_impl_with_id(
        state,
        include_str,
        repository,
        placeholder_id,
      )
    }
  }
}

/// Internal helper to resolve an include reference with a pre-allocated ID
/// This compiles the rule and copies it to the pre-allocated ID slot
fn resolve_include_impl_with_id(
  state: CompilerState,
  include_str: String,
  repository: Dict(String, RawRule),
  preallocated_id: RuleId,
) -> #(CompilerState, RuleId) {
  let reference = parse_include(include_str)

  case reference {
    SelfReference -> {
      // $self references repository.$self
      case dict.get(repository, "$self") {
        Ok(rule) ->
          compile_rule_with_id(state, rule, repository, preallocated_id)
        Error(Nil) -> #(state, rule_id_none)
      }
    }

    BaseReference -> {
      // $base references repository.$base
      case dict.get(repository, "$base") {
        Ok(rule) ->
          compile_rule_with_id(state, rule, repository, preallocated_id)
        Error(Nil) -> #(state, rule_id_none)
      }
    }

    RelativeReference(rule_name) -> {
      // #name references repository[name]
      case dict.get(repository, rule_name) {
        Ok(rule) ->
          compile_rule_with_id(state, rule, repository, preallocated_id)
        Error(Nil) -> #(state, rule_id_none)
      }
    }

    TopLevelReference(scope_name) -> {
      // External grammar reference - look up by scope name
      case dict.get(state.external_grammars, scope_name) {
        Ok(external_grammar) -> {
          // Initialize the external grammar with $self/$base in its repository
          let initialized = init_grammar(external_grammar, None)
          case dict.get(initialized.repository, "$self") {
            Ok(self_rule) -> {
              // Switch scope context to external grammar to namespace its includes
              let parent_scope = state.scope_context
              let state = CompilerState(..state, scope_context: scope_name)

              // Compile the external grammar's $self rule
              let #(state, rule_id) =
                compile_rule_with_id(
                  state,
                  self_rule,
                  initialized.repository,
                  preallocated_id,
                )

              // Restore parent scope context
              let state = CompilerState(..state, scope_context: parent_scope)
              #(state, rule_id)
            }
            Error(Nil) -> #(state, rule_id_none)
          }
        }
        Error(Nil) -> #(state, rule_id_none)
      }
    }

    TopLevelRepositoryReference(scope_name, rule_name) -> {
      // External grammar repository reference
      case dict.get(state.external_grammars, scope_name) {
        Ok(external_grammar) -> {
          let initialized = init_grammar(external_grammar, None)
          case dict.get(initialized.repository, rule_name) {
            Ok(rule) -> {
              // Switch scope context to external grammar to namespace its includes
              let parent_scope = state.scope_context
              let state = CompilerState(..state, scope_context: scope_name)

              // Compile the external grammar rule
              let #(state, rule_id) =
                compile_rule_with_id(
                  state,
                  rule,
                  initialized.repository,
                  preallocated_id,
                )

              // Restore parent scope context
              let state = CompilerState(..state, scope_context: parent_scope)
              #(state, rule_id)
            }
            Error(Nil) -> #(state, rule_id_none)
          }
        }
        Error(Nil) -> #(state, rule_id_none)
      }
    }
  }
}

/// Compile a rule with a pre-allocated ID
/// This is used for include resolution where we need to handle cycles
fn compile_rule_with_id(
  state: CompilerState,
  raw: RawRule,
  repository: Dict(String, RawRule),
  preallocated_id: RuleId,
) -> #(CompilerState, RuleId) {
  // If this raw rule has its own include, resolve it normally
  // (the include resolution already handles pre-allocation)
  case raw.include {
    Some(include_str) -> resolve_include(state, include_str, repository)
    None -> {
      // Compile the rule normally, then copy to pre-allocated ID
      let #(state, compiled_id) = compile_rule_by_type(state, raw, repository)
      case compiled_id == rule_id_none {
        True -> #(state, rule_id_none)
        False -> {
          // Copy the compiled rule to the pre-allocated ID slot
          case dict.get(state.rules, compiled_id) {
            Ok(rule) -> {
              // Update the rule's internal ID to match the pre-allocated ID
              let updated_rule = update_rule_id(rule, preallocated_id)
              let state =
                register_rule_with_id(state, preallocated_id, updated_rule)
              #(state, preallocated_id)
            }
            Error(Nil) -> #(state, compiled_id)
          }
        }
      }
    }
  }
}

/// Compile captures from a captures dictionary
fn compile_captures(
  state: CompilerState,
  captures: Dict(String, RawRule),
  repository: Dict(String, RawRule),
) -> #(CompilerState, List(Option(CaptureRule))) {
  case dict.size(captures) {
    0 -> #(state, [])
    _ -> {
      // Find maximum capture ID
      let max_id = find_max_capture_id(dict.keys(captures), 0)

      // Build sparse array
      compile_captures_impl(state, captures, repository, 0, max_id, [])
    }
  }
}

fn find_max_capture_id(keys: List(String), max: Int) -> Int {
  case keys {
    [] -> max
    [key, ..rest] -> {
      case int.parse(key) {
        Ok(id) ->
          case id > max {
            True -> find_max_capture_id(rest, id)
            False -> find_max_capture_id(rest, max)
          }
        Error(Nil) -> find_max_capture_id(rest, max)
      }
    }
  }
}

fn compile_captures_impl(
  state: CompilerState,
  captures: Dict(String, RawRule),
  repository: Dict(String, RawRule),
  current: Int,
  max: Int,
  acc: List(Option(CaptureRule)),
) -> #(CompilerState, List(Option(CaptureRule))) {
  case current > max {
    True -> #(state, list.reverse(acc))
    False -> {
      let key = int.to_string(current)
      case dict.get(captures, key) {
        Error(Nil) -> {
          // No capture at this index
          compile_captures_impl(state, captures, repository, current + 1, max, [
            None,
            ..acc
          ])
        }
        Ok(raw_capture) -> {
          // Compile the capture rule
          let #(state, cap_rule) =
            compile_capture_rule(state, raw_capture, repository)
          compile_captures_impl(state, captures, repository, current + 1, max, [
            Some(cap_rule),
            ..acc
          ])
        }
      }
    }
  }
}

/// Compile a single capture rule
fn compile_capture_rule(
  state: CompilerState,
  raw: RawRule,
  repository: Dict(String, RawRule),
) -> #(CompilerState, CaptureRule) {
  // Check if capture has nested patterns (requires retokenization)
  let #(state, retokenize_id) = case raw.patterns != [] {
    False -> #(state, rule_id_none)
    True -> compile_rule(state, raw, repository)
  }

  // Create capture rule
  let rule = capture_rule(state.next_id, raw.name, retokenize_id)

  // Register (even though CaptureRule isn't stored in main registry)
  let new_state = CompilerState(..state, next_id: state.next_id + 1)

  #(new_state, rule)
}

/// Compile injections from a grammar
fn compile_injections(
  state: CompilerState,
  raw: RawGrammar,
  repository: Dict(String, RawRule),
) -> #(CompilerState, List(Injection)) {
  let injection_pairs = dict.to_list(raw.injections)
  compile_injections_impl(state, injection_pairs, repository, [])
}

fn compile_injections_impl(
  state: CompilerState,
  injections: List(#(String, RawRule)),
  repository: Dict(String, RawRule),
  acc: List(Injection),
) -> #(CompilerState, List(Injection)) {
  case injections {
    [] -> #(state, list.reverse(acc))
    [#(selector, raw_rule), ..rest] -> {
      let #(state, rule_id) = compile_rule(state, raw_rule, repository)
      case rule_id == rule_id_none {
        True -> compile_injections_impl(state, rest, repository, acc)
        False -> {
          // Parse selector and create matcher using the full scope selector parser
          let matchers = scope_selector.create_matchers(selector)

          // Create injections from all matchers (usually just one)
          let new_injections =
            list.map(matchers, fn(m) {
              Injection(
                selector: selector,
                matcher: m.matcher,
                priority: m.priority,
                rule_id: rule_id,
              )
            })

          compile_injections_impl(
            state,
            rest,
            repository,
            list.append(new_injections, acc),
          )
        }
      }
    }
  }
}
