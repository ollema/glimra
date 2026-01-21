//// Main tokenization loop for TextMate grammar processing
////
//// This module implements the core tokenization algorithm that processes
//// a line of text using a TextMate grammar, producing tokens with scope
//// information for syntax highlighting.

import gleam/option.{type Option, None, Some}
import gleam/string
import glimra/textmate/grammar.{
  type FindMatchResult, type Grammar, type Injection, FindMatchResult,
  compile_rule_patterns, compile_while_pattern, end_rule_id, find_next_match,
  get_rule, resolve_end_pattern, resolve_while_pattern,
}
import glimra/textmate/line_tokens.{
  type LineTokens, type LocalStackElement, type Token, LocalStackElement,
  get_current_scopes, pop_local_stack, produce, produce_from_scopes,
}
import glimra/textmate/regexp_source.{resolve_capture_refs}
import glimra/textmate/rule.{
  type CaptureIndex, type CaptureRule, type Rule, type RuleId, BeginEndRule,
  BeginWhileRule, CaptureIndex, IncludeOnlyRule, MatchRule, rule_id_none,
}
import glimra/textmate/state_stack.{
  type StateStack, StateStackFrame, StateStackNull, attributed_get_scope_names,
  attributed_push_scope, create_root_state, get_anchor_pos,
  get_begin_rule_captured_eol, get_content_name_scopes, get_end_rule,
  get_enter_pos, get_name_scopes, get_rule_id, has_same_rule_as, pop, push,
  safe_pop, set_content_name_scopes,
}

/// Result of tokenizing a line
pub type TokenizeResult {
  TokenizeResult(
    /// The tokens produced
    tokens: List(Token),
    /// The updated state stack for the next line
    rule_stack: StateStack,
    /// Whether the tokenizer stopped early (e.g., time limit)
    stopped_early: Bool,
  )
}

/// Internal state during tokenization
type TokenizeState {
  TokenizeState(
    /// Current position in the line
    line_pos: Int,
    /// Anchor position for \G matching (-1 if not set)
    anchor_pos: Int,
    /// Whether this is the first line (affects \A anchor)
    is_first_line: Bool,
    /// Current state stack
    stack: StateStack,
    /// Token accumulator
    line_tokens: LineTokens,
    /// Whether to stop the loop
    stop: Bool,
  )
}

/// Tokenize a line of text
pub fn tokenize_line(
  grammar: Grammar,
  line_text: String,
  is_first_line: Bool,
  prev_state: StateStack,
  check_while_conditions: Bool,
) -> TokenizeResult {
  // Original line length (without newline) for token production
  let line_length = string.length(line_text)
  let line_tokens = line_tokens.new(False, line_length)

  // Append newline for pattern matching (allows end patterns like (?=\n) to match)
  // This matches vscode-textmate's behavior
  let matching_line_text = line_text <> "\n"

  // Reset enter_pos for all stack frames to prevent cross-line
  // endless loop false positives
  let prev_state = state_stack.reset_enter_positions(prev_state)

  let result =
    tokenize_string(
      grammar,
      matching_line_text,
      line_length,
      is_first_line,
      prev_state,
      line_tokens,
      check_while_conditions,
    )

  let tokens = line_tokens.get_result(result.line_tokens, result.stack)

  TokenizeResult(
    tokens: tokens,
    rule_stack: result.stack,
    stopped_early: result.stop,
  )
}

/// Main tokenization implementation
///
/// Note: line_text should include a trailing newline for pattern matching,
/// but line_length should be the original length without the newline.
fn tokenize_string(
  grammar: Grammar,
  line_text: String,
  line_length: Int,
  is_first_line: Bool,
  prev_state: StateStack,
  line_tokens: LineTokens,
  check_while_conditions: Bool,
) -> TokenizeState {
  // Initialize state - if null, create root state with grammar's root scope
  let prev_state = case prev_state {
    StateStackNull ->
      create_root_state(grammar.scope_name, grammar.root_rule_id)
    _ -> prev_state
  }

  // Set initial anchor position based on whether the previous rule captured EOL.
  // If the rule captured at end of line, \G should match at position 0 of the new line.
  let initial_anchor_pos = case get_begin_rule_captured_eol(prev_state) {
    True -> 0
    False -> -1
  }

  // Check while conditions if needed (for BeginWhileRules)
  let #(stack, line_pos, anchor_pos, is_first_line, line_tokens) = case
    check_while_conditions
  {
    True ->
      check_while_conditions_loop(
        grammar,
        line_text,
        line_length,
        prev_state,
        is_first_line,
        line_tokens,
      )
    False -> #(prev_state, 0, initial_anchor_pos, is_first_line, line_tokens)
  }

  let state =
    TokenizeState(
      line_pos: line_pos,
      anchor_pos: anchor_pos,
      is_first_line: is_first_line,
      stack: stack,
      line_tokens: line_tokens,
      stop: False,
    )

  // Main tokenization loop
  tokenize_loop(grammar, line_text, line_length, state)
}

/// Tokenization implementation starting from a specific position
///
/// Used for retokenizing captured text. Unlike tokenize_string, this:
/// - Starts at the specified position instead of 0
/// - Does not check while conditions
/// - Does not initialize null states (expects a valid state)
fn tokenize_string_from(
  grammar: Grammar,
  line_text: String,
  line_length: Int,
  start_pos: Int,
  is_first_line: Bool,
  stack: StateStack,
  line_tokens: LineTokens,
) -> TokenizeState {
  let state =
    TokenizeState(
      line_pos: start_pos,
      anchor_pos: -1,
      is_first_line: is_first_line,
      stack: stack,
      line_tokens: line_tokens,
      stop: False,
    )

  // Main tokenization loop
  tokenize_loop(grammar, line_text, line_length, state)
}

/// Main tokenization loop
fn tokenize_loop(
  grammar: Grammar,
  line_text: String,
  line_length: Int,
  state: TokenizeState,
) -> TokenizeState {
  case state.stop {
    True -> state
    False -> {
      let new_state = scan_next(grammar, line_text, line_length, state)
      tokenize_loop(grammar, line_text, line_length, new_state)
    }
  }
}

/// Result of matching rules or injections
type MatchResult {
  MatchResult(
    rule_id: RuleId,
    capture_indices: List(CaptureIndex),
    priority_match: Bool,
  )
}

/// Scan for the next match and process it
fn scan_next(
  grammar: Grammar,
  line_text: String,
  line_length: Int,
  state: TokenizeState,
) -> TokenizeState {
  // Get the current rule from the stack
  let current_rule = case get_rule_id(state.stack) {
    None -> get_rule(grammar, grammar.root_rule_id)
    Some(rule_id) -> get_rule(grammar, rule_id)
  }

  case current_rule {
    None -> {
      // No rule found - produce final token and stop
      let line_tokens = produce(state.line_tokens, state.stack, line_length)
      TokenizeState(..state, line_tokens: line_tokens, stop: True)
    }

    Some(rule) -> {
      // Get end pattern if this is a BeginEndRule
      let end_regex = get_end_rule(state.stack)

      // Compile patterns for matching
      let allow_a = state.is_first_line && state.line_pos == 0
      let allow_g = state.line_pos == state.anchor_pos

      // Find best match (regular rule or injection)
      let match_result =
        match_rule_or_injections(
          grammar,
          rule,
          end_regex,
          line_text,
          state.line_pos,
          state.stack,
          allow_a,
          allow_g,
        )

      case match_result {
        None -> {
          // No match - produce final token and stop
          let line_tokens = produce(state.line_tokens, state.stack, line_length)
          TokenizeState(..state, line_tokens: line_tokens, stop: True)
        }

        Some(MatchResult(rule_id, capture_indices, _priority_match)) -> {
          // Check if we've made progress
          let has_advanced = case capture_indices {
            [CaptureIndex(start: _, end: end_pos), ..] ->
              end_pos > state.line_pos
            [] -> False
          }

          // Handle the match based on whether it's an end pattern or a rule
          let result = case rule_id == end_rule_id {
            True -> {
              let r =
                handle_end_match(
                  grammar,
                  line_text,
                  line_length,
                  state,
                  capture_indices,
                  has_advanced,
                )
              r
            }
            False -> {
              let r =
                handle_rule_match(
                  grammar,
                  line_text,
                  line_length,
                  state,
                  rule_id,
                  capture_indices,
                  has_advanced,
                )
              r
            }
          }
          result
        }
      }
    }
  }
}

/// Match against normal rule patterns and injections, returning the best match
fn match_rule_or_injections(
  grammar: Grammar,
  rule: Rule,
  end_regex: Option(String),
  line_text: String,
  line_pos: Int,
  stack: StateStack,
  allow_a: Bool,
  allow_g: Bool,
) -> Option(MatchResult) {
  // Try normal rule match
  let compiled =
    compile_rule_patterns(grammar, rule, end_regex, allow_a, allow_g)

  let rule_match =
    find_next_match(compiled, line_text, line_pos, allow_a, allow_g)

  // If no injections, return rule match directly
  case grammar.injections {
    [] ->
      case rule_match {
        None -> None
        Some(FindMatchResult(rule_id, captures)) ->
          Some(MatchResult(rule_id, captures, False))
      }
    injections -> {
      // Get current scopes for injection matching
      let scopes = get_scope_names(stack)

      // Try injection match
      let injection_match =
        match_injections(
          grammar,
          injections,
          scopes,
          line_text,
          line_pos,
          allow_a,
          allow_g,
        )

      // Decide which match wins
      pick_best_match(rule_match, injection_match)
    }
  }
}

/// Get scope names from the current stack for injection matching
/// Returns the full scope path from the content_name_scopes of the top frame
fn get_scope_names(stack: StateStack) -> List(String) {
  case get_content_name_scopes(stack) {
    Some(attributed_stack) -> attributed_get_scope_names(attributed_stack)
    None ->
      case get_name_scopes(stack) {
        Some(attributed_stack) -> attributed_get_scope_names(attributed_stack)
        None -> []
      }
  }
}

/// Match against all injections that match the current scope
fn match_injections(
  grammar: Grammar,
  injections: List(Injection),
  scopes: List(String),
  line_text: String,
  line_pos: Int,
  allow_a: Bool,
  allow_g: Bool,
) -> Option(MatchResult) {
  match_injections_impl(
    grammar,
    injections,
    scopes,
    line_text,
    line_pos,
    allow_a,
    allow_g,
    None,
  )
}

fn match_injections_impl(
  grammar: Grammar,
  injections: List(Injection),
  scopes: List(String),
  line_text: String,
  line_pos: Int,
  allow_a: Bool,
  allow_g: Bool,
  best: Option(#(Int, MatchResult)),
) -> Option(MatchResult) {
  case injections {
    [] ->
      case best {
        None -> None
        Some(#(_, result)) -> Some(result)
      }
    [injection, ..rest] -> {
      // Check if injection selector matches current scopes
      case injection.matcher(scopes) {
        False ->
          match_injections_impl(
            grammar,
            rest,
            scopes,
            line_text,
            line_pos,
            allow_a,
            allow_g,
            best,
          )
        True -> {
          // Injection matches - try to find a pattern match
          case get_rule(grammar, injection.rule_id) {
            None ->
              match_injections_impl(
                grammar,
                rest,
                scopes,
                line_text,
                line_pos,
                allow_a,
                allow_g,
                best,
              )
            Some(injection_rule) -> {
              let compiled =
                compile_rule_patterns(
                  grammar,
                  injection_rule,
                  None,
                  allow_a,
                  allow_g,
                )
              case
                find_next_match(compiled, line_text, line_pos, allow_a, allow_g)
              {
                None ->
                  match_injections_impl(
                    grammar,
                    rest,
                    scopes,
                    line_text,
                    line_pos,
                    allow_a,
                    allow_g,
                    best,
                  )
                Some(FindMatchResult(rule_id, captures)) -> {
                  let match_start = case captures {
                    [CaptureIndex(start: s, ..), ..] -> s
                    [] -> line_pos
                  }
                  let is_priority = injection.priority == -1
                  let result = MatchResult(rule_id, captures, is_priority)

                  // Check if this is better than current best
                  let new_best = case best {
                    None -> Some(#(match_start, result))
                    Some(#(best_start, _)) -> {
                      case match_start < best_start {
                        True -> Some(#(match_start, result))
                        False -> best
                      }
                    }
                  }

                  // If we matched at line_pos, no need to check more injections
                  case match_start == line_pos {
                    True ->
                      case new_best {
                        None -> None
                        Some(#(_, r)) -> Some(r)
                      }
                    False ->
                      match_injections_impl(
                        grammar,
                        rest,
                        scopes,
                        line_text,
                        line_pos,
                        allow_a,
                        allow_g,
                        new_best,
                      )
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Pick the best match between a rule match and an injection match
fn pick_best_match(
  rule_match: Option(FindMatchResult),
  injection_match: Option(MatchResult),
) -> Option(MatchResult) {
  case rule_match, injection_match {
    None, None -> None
    None, Some(inj) -> Some(inj)
    Some(FindMatchResult(rule_id, captures)), None ->
      Some(MatchResult(rule_id, captures, False))
    Some(FindMatchResult(rule_id, rule_captures)),
      Some(MatchResult(_, inj_captures, priority_match) as inj)
    -> {
      let rule_start = case rule_captures {
        [CaptureIndex(start: s, ..), ..] -> s
        [] -> 999_999_999
      }
      let inj_start = case inj_captures {
        [CaptureIndex(start: s, ..), ..] -> s
        [] -> 999_999_999
      }

      // Injection wins if it starts earlier, or at same position with high priority
      case
        inj_start < rule_start || { inj_start == rule_start && priority_match }
      {
        True -> Some(inj)
        False -> Some(MatchResult(rule_id, rule_captures, False))
      }
    }
  }
}

/// Handle an end pattern match (pop the current BeginEndRule)
fn handle_end_match(
  grammar: Grammar,
  line_text: String,
  line_length: Int,
  state: TokenizeState,
  capture_indices: List(CaptureIndex),
  has_advanced: Bool,
) -> TokenizeState {
  let #(match_start, match_end) = get_match_bounds(capture_indices)

  // Get the rule being ended
  case get_rule_id(state.stack) {
    None -> TokenizeState(..state, stop: True)
    Some(rule_id) ->
      case get_rule(grammar, rule_id) {
        None -> TokenizeState(..state, stop: True)
        Some(popped_rule) -> {
          // Produce token before the end match
          let line_tokens = produce(state.line_tokens, state.stack, match_start)

          // Switch to name scopes for end captures
          let stack =
            set_content_name_scopes(state.stack, get_name_scopes(state.stack))

          // Handle end captures
          let line_tokens =
            handle_captures(
              grammar,
              line_text,
              state.is_first_line,
              stack,
              line_tokens,
              get_end_captures(popped_rule),
              capture_indices,
            )

          // Produce token after end captures
          let line_tokens = produce(line_tokens, stack, match_end)

          // Pop the stack
          let popped_stack = state.stack
          let stack = pop(state.stack)
          let anchor_pos = get_anchor_pos(popped_stack)

          // Check for endless loop: pushed and popped without advancing
          case !has_advanced && get_enter_pos(popped_stack) == state.line_pos {
            True -> {
              // Revert pop and stop
              let line_tokens = produce(line_tokens, popped_stack, line_length)
              TokenizeState(
                ..state,
                stack: popped_stack,
                line_tokens: line_tokens,
                stop: True,
              )
            }
            False ->
              TokenizeState(
                ..state,
                line_pos: match_end,
                anchor_pos: anchor_pos,
                stack: stack,
                line_tokens: line_tokens,
              )
          }
        }
      }
  }
}

/// Handle a rule match (MatchRule, BeginEndRule, or BeginWhileRule)
fn handle_rule_match(
  grammar: Grammar,
  line_text: String,
  line_length: Int,
  state: TokenizeState,
  rule_id: RuleId,
  capture_indices: List(CaptureIndex),
  has_advanced: Bool,
) -> TokenizeState {
  case get_rule(grammar, rule_id) {
    None -> TokenizeState(..state, stop: True)
    Some(rule) ->
      case rule {
        MatchRule(..) ->
          handle_match_rule(
            grammar,
            line_text,
            line_length,
            state,
            rule,
            capture_indices,
            has_advanced,
          )
        BeginEndRule(..) ->
          handle_begin_end_rule(
            grammar,
            line_text,
            line_length,
            state,
            rule,
            rule_id,
            capture_indices,
            has_advanced,
          )
        BeginWhileRule(..) ->
          handle_begin_while_rule(
            grammar,
            line_text,
            line_length,
            state,
            rule,
            rule_id,
            capture_indices,
            has_advanced,
          )
        IncludeOnlyRule(..) ->
          // Include-only rules shouldn't match directly
          TokenizeState(..state, stop: True)
      }
  }
}

/// Handle a MatchRule match
fn handle_match_rule(
  grammar: Grammar,
  line_text: String,
  line_length: Int,
  state: TokenizeState,
  rule: Rule,
  capture_indices: List(CaptureIndex),
  has_advanced: Bool,
) -> TokenizeState {
  let #(match_start, match_end) = get_match_bounds(capture_indices)

  // Produce token before the match
  let line_tokens = produce(state.line_tokens, state.stack, match_start)

  // Get the scope name and push it
  let scope_name = get_resolved_name(rule, line_text, capture_indices)

  let stack = push_scope_for_match(state.stack, scope_name, rule_id_none)

  // Handle captures
  let line_tokens =
    handle_captures(
      grammar,
      line_text,
      state.is_first_line,
      stack,
      line_tokens,
      get_match_captures(rule),
      capture_indices,
    )

  // Produce token after match
  let line_tokens = produce(line_tokens, stack, match_end)

  // MatchRules don't nest - pop immediately
  let stack = pop(stack)

  // Check for endless loop
  case !has_advanced {
    True -> {
      let stack = safe_pop(stack)
      let line_tokens = produce(line_tokens, stack, line_length)
      TokenizeState(..state, stack: stack, line_tokens: line_tokens, stop: True)
    }
    False ->
      TokenizeState(
        ..state,
        line_pos: match_end,
        stack: stack,
        line_tokens: line_tokens,
      )
  }
}

/// Handle a BeginEndRule match (push onto stack)
fn handle_begin_end_rule(
  grammar: Grammar,
  line_text: String,
  line_length: Int,
  state: TokenizeState,
  rule: Rule,
  rule_id: RuleId,
  capture_indices: List(CaptureIndex),
  has_advanced: Bool,
) -> TokenizeState {
  let #(match_start, match_end) = get_match_bounds(capture_indices)

  // Produce token before the begin match
  let line_tokens = produce(state.line_tokens, state.stack, match_start)

  let before_push = state.stack

  // Get scope name and push new state
  let scope_name = get_resolved_name(rule, line_text, capture_indices)
  let captured_eol = match_end >= line_length

  // Get end pattern, resolving back-references if needed
  let end_rule = case rule {
    BeginEndRule(end_pattern: end_pat, end_has_back_refs: has_back_refs, ..) ->
      Some(resolve_end_pattern(
        end_pat,
        has_back_refs,
        line_text,
        capture_indices,
      ))
    _ -> None
  }

  let stack =
    push_begin_end_state(
      state.stack,
      rule_id,
      match_start,
      state.anchor_pos,
      captured_eol,
      end_rule,
      scope_name,
    )

  // Handle begin captures
  let line_tokens =
    handle_captures(
      grammar,
      line_text,
      state.is_first_line,
      stack,
      line_tokens,
      get_begin_captures(rule),
      capture_indices,
    )

  // Produce token after begin match
  let line_tokens = produce(line_tokens, stack, match_end)
  let anchor_pos = match_end

  // Add content name scope
  let content_name = get_resolved_content_name(rule, line_text, capture_indices)
  let stack = push_content_name_scope(stack, content_name)

  // Check for endless loop: pushed same rule without advancing
  case !has_advanced && has_same_rule_as(before_push, stack) {
    True -> {
      let stack = pop(stack)
      let line_tokens = produce(line_tokens, stack, line_length)
      TokenizeState(..state, stack: stack, line_tokens: line_tokens, stop: True)
    }
    False ->
      TokenizeState(
        ..state,
        line_pos: match_end,
        anchor_pos: anchor_pos,
        stack: stack,
        line_tokens: line_tokens,
      )
  }
}

/// Handle a BeginWhileRule match (push onto stack)
fn handle_begin_while_rule(
  grammar: Grammar,
  line_text: String,
  line_length: Int,
  state: TokenizeState,
  rule: Rule,
  rule_id: RuleId,
  capture_indices: List(CaptureIndex),
  has_advanced: Bool,
) -> TokenizeState {
  let #(match_start, match_end) = get_match_bounds(capture_indices)

  // Produce token before the begin match
  let line_tokens = produce(state.line_tokens, state.stack, match_start)

  let before_push = state.stack

  // Get scope name and push new state
  let scope_name = get_resolved_name(rule, line_text, capture_indices)
  let captured_eol = match_end >= line_length

  // Get while pattern, resolving back-references if needed
  let while_rule = case rule {
    BeginWhileRule(
      while_pattern: while_pat,
      while_has_back_refs: has_back_refs,
      ..,
    ) ->
      Some(resolve_while_pattern(
        while_pat,
        has_back_refs,
        line_text,
        capture_indices,
      ))
    _ -> None
  }

  let stack =
    push_begin_while_state(
      state.stack,
      rule_id,
      match_start,
      state.anchor_pos,
      captured_eol,
      while_rule,
      scope_name,
    )

  // Handle begin captures
  let line_tokens =
    handle_captures(
      grammar,
      line_text,
      state.is_first_line,
      stack,
      line_tokens,
      get_begin_captures(rule),
      capture_indices,
    )

  // Produce token after begin match
  let line_tokens = produce(line_tokens, stack, match_end)
  let anchor_pos = match_end

  // Add content name scope
  let content_name = get_resolved_content_name(rule, line_text, capture_indices)
  let stack = push_content_name_scope(stack, content_name)

  // Check for endless loop
  case !has_advanced && has_same_rule_as(before_push, stack) {
    True -> {
      let stack = pop(stack)
      let line_tokens = produce(line_tokens, stack, line_length)
      TokenizeState(..state, stack: stack, line_tokens: line_tokens, stop: True)
    }
    False ->
      TokenizeState(
        ..state,
        line_pos: match_end,
        anchor_pos: anchor_pos,
        stack: stack,
        line_tokens: line_tokens,
      )
  }
}

/// Handle captures within a match
fn handle_captures(
  grammar: Grammar,
  line_text: String,
  is_first_line: Bool,
  stack: StateStack,
  line_tokens: LineTokens,
  captures: List(Option(CaptureRule)),
  capture_indices: List(CaptureIndex),
) -> LineTokens {
  let result = case captures {
    [] -> line_tokens
    _ ->
      handle_captures_impl(
        grammar,
        line_text,
        is_first_line,
        stack,
        line_tokens,
        captures,
        capture_indices,
        capture_indices,
        [],
      )
  }
  result
}

fn handle_captures_impl(
  grammar: Grammar,
  line_text: String,
  is_first_line: Bool,
  stack: StateStack,
  line_tokens: LineTokens,
  captures_remaining: List(Option(CaptureRule)),
  indices_remaining: List(CaptureIndex),
  all_capture_indices: List(CaptureIndex),
  local_stack: List(LocalStackElement),
) -> LineTokens {
  // Note: is_first_line is passed through to recursive calls but not used in this function's logic
  let _ = is_first_line
  // Walk through both lists in parallel (O(1) per step instead of O(n))
  case captures_remaining, indices_remaining {
    [], _ -> {
      // Done with captures - pop remaining local stack
      finalize_local_stack(local_stack, line_tokens)
    }
    _, [] -> {
      finalize_local_stack(local_stack, line_tokens)
    }
    [None, ..rest_captures], [_, ..rest_indices] -> {
      // No capture rule for this index - continue
      handle_captures_impl(
        grammar,
        line_text,
        is_first_line,
        stack,
        line_tokens,
        rest_captures,
        rest_indices,
        all_capture_indices,
        local_stack,
      )
    }
    [Some(capture_rule), ..rest_captures],
      [CaptureIndex(start: cap_start, end: cap_end), ..rest_indices]
    -> {
      // Skip empty captures
      case cap_end <= cap_start {
        True ->
          handle_captures_impl(
            grammar,
            line_text,
            is_first_line,
            stack,
            line_tokens,
            rest_captures,
            rest_indices,
            all_capture_indices,
            local_stack,
          )
        False -> {
          // Pop local stack elements that have ended
          let #(local_stack, line_tokens) =
            pop_local_stack(local_stack, cap_start, line_tokens)

          // Produce token at capture start using current scope
          let line_tokens = case get_current_scopes(local_stack, stack) {
            None -> line_tokens
            Some(scopes) -> produce_from_scopes(line_tokens, scopes, cap_start)
          }

          // Check for retokenization
          let retokenize_id = capture_rule.retokenize_rule_id
          case retokenize_id != rule_id_none {
            True -> {
              // Need to retokenize the captured text
              // First, get the current scopes to use as base
              let base_scopes = case get_current_scopes(local_stack, stack) {
                None -> None
                Some(s) -> Some(s)
              }

              // Apply capture name if present
              let scopes_with_name = case capture_rule.name, base_scopes {
                Some(name), Some(scopes) -> {
                  let resolved_name =
                    resolve_capture_refs(name, line_text, all_capture_indices)
                  Some(attributed_push_scope(scopes, resolved_name, 0))
                }
                _, other -> other
              }

              // Retokenize the captured text
              let line_tokens =
                retokenize_capture(
                  grammar,
                  line_text,
                  cap_start,
                  cap_end,
                  retokenize_id,
                  scopes_with_name,
                  line_tokens,
                  stack,
                )

              handle_captures_impl(
                grammar,
                line_text,
                is_first_line,
                stack,
                line_tokens,
                rest_captures,
                rest_indices,
                all_capture_indices,
                local_stack,
              )
            }
            False -> {
              // No retokenization needed - just handle scope name
              let cap_name = capture_rule.name
              case cap_name {
                None ->
                  handle_captures_impl(
                    grammar,
                    line_text,
                    is_first_line,
                    stack,
                    line_tokens,
                    rest_captures,
                    rest_indices,
                    all_capture_indices,
                    local_stack,
                  )
                Some(name) -> {
                  let resolved_name =
                    resolve_capture_refs(name, line_text, all_capture_indices)
                  case get_current_scopes(local_stack, stack) {
                    None ->
                      handle_captures_impl(
                        grammar,
                        line_text,
                        is_first_line,
                        stack,
                        line_tokens,
                        rest_captures,
                        rest_indices,
                        all_capture_indices,
                        local_stack,
                      )
                    Some(scopes) -> {
                      // Create new scope with capture name
                      let new_scopes =
                        attributed_push_scope(scopes, resolved_name, 0)
                      let local_elem =
                        LocalStackElement(scopes: new_scopes, end_pos: cap_end)
                      handle_captures_impl(
                        grammar,
                        line_text,
                        is_first_line,
                        stack,
                        line_tokens,
                        rest_captures,
                        rest_indices,
                        all_capture_indices,
                        [local_elem, ..local_stack],
                      )
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Retokenize a captured text range using a rule's patterns
///
/// This implements vscode-textmate's retokenization logic for captures that
/// have nested patterns (retokenizeCapturedWithRuleId). The approach:
/// 1. Create a new stack with the retokenize rule pushed
/// 2. Create a substring from 0 to cap_end (to limit tokenization scope)
/// 3. Call tokenize_string_from starting at cap_start
///
/// This is used for grammars like C's numbers pattern where capture 0 has
/// nested patterns to further classify the number type.
fn retokenize_capture(
  grammar: Grammar,
  line_text: String,
  cap_start: Int,
  cap_end: Int,
  retokenize_id: RuleId,
  base_scopes: Option(state_stack.AttributedScopeStack),
  line_tokens: LineTokens,
  stack: StateStack,
) -> LineTokens {
  // Create a substring from 0 to cap_end to limit tokenization scope
  // This matches vscode-textmate: grammar.createOnigString(lineTextContent.substring(0, captureIndex.end))
  let sub_line_text = string.slice(line_text, 0, cap_end)

  // Create a new stack with the retokenize rule pushed
  // vscode-textmate: stack.push(captureRule.retokenizeCapturedWithRuleId, ...)
  let retokenize_stack =
    push(
      stack,
      retokenize_id,
      cap_start,
      // enter_pos
      -1,
      // anchor_pos
      False,
      // begin_rule_captured_eol
      None,
      // end_rule - none needed for retokenization
      base_scopes,
      // name_scopes
      base_scopes,
      // content_name_scopes
    )

  // Call tokenize_string_from starting at cap_start
  // vscode-textmate: _tokenizeString(grammar, onigSubStr, isFirstLine && captureIndex.start === 0, captureIndex.start, stackClone, lineTokens, false, 0)
  let result =
    tokenize_string_from(
      grammar,
      sub_line_text,
      cap_end,
      // line_length is cap_end since we're using substring
      cap_start,
      // start_pos
      False,
      // is_first_line - false for retokenization
      retokenize_stack,
      line_tokens,
    )

  // Return the updated line_tokens
  result.line_tokens
}

/// Finalize local stack by popping all remaining elements
fn finalize_local_stack(
  local_stack: List(LocalStackElement),
  line_tokens: LineTokens,
) -> LineTokens {
  case local_stack {
    [] -> line_tokens
    [LocalStackElement(scopes: scopes, end_pos: end_pos), ..rest] -> {
      let line_tokens = produce_from_scopes(line_tokens, scopes, end_pos)
      finalize_local_stack(rest, line_tokens)
    }
  }
}

/// Check while conditions at the start of a line
fn check_while_conditions_loop(
  grammar: Grammar,
  line_text: String,
  line_length: Int,
  stack: StateStack,
  is_first_line: Bool,
  line_tokens: LineTokens,
) -> #(StateStack, Int, Int, Bool, LineTokens) {
  // Determine initial anchor position based on whether begin captured EOL
  let anchor_pos = case get_begin_rule_captured_eol(stack) {
    True -> 0
    False -> -1
  }

  // Collect BeginWhileRules from stack (bottom to top)
  let while_rules = collect_while_rules(grammar, stack, [])

  // Check each while rule (top to bottom, which is list order)
  check_while_rules(
    grammar,
    line_text,
    line_length,
    stack,
    while_rules,
    0,
    anchor_pos,
    is_first_line,
    line_tokens,
  )
}

/// Collect BeginWhileRules from stack
fn collect_while_rules(
  grammar: Grammar,
  stack: StateStack,
  acc: List(#(Rule, StateStack)),
) -> List(#(Rule, StateStack)) {
  case stack {
    StateStackNull -> acc
    StateStackFrame(..) -> {
      case get_rule_id(stack) {
        None -> collect_while_rules(grammar, pop(stack), acc)
        Some(rule_id) ->
          case get_rule(grammar, rule_id) {
            None -> collect_while_rules(grammar, pop(stack), acc)
            Some(rule) ->
              case rule {
                BeginWhileRule(..) ->
                  collect_while_rules(grammar, pop(stack), [
                    #(rule, stack),
                    ..acc
                  ])
                _ -> collect_while_rules(grammar, pop(stack), acc)
              }
          }
      }
    }
  }
}

/// Check each while rule
fn check_while_rules(
  grammar: Grammar,
  line_text: String,
  line_length: Int,
  stack: StateStack,
  while_rules: List(#(Rule, StateStack)),
  line_pos: Int,
  anchor_pos: Int,
  is_first_line: Bool,
  line_tokens: LineTokens,
) -> #(StateStack, Int, Int, Bool, LineTokens) {
  // Note: line_length reserved for future use
  let _ = line_length

  case while_rules {
    [] -> #(stack, line_pos, anchor_pos, is_first_line, line_tokens)
    [#(rule, rule_stack), ..rest] -> {
      // Get the while pattern (may have back-references resolved)
      let while_pattern = case get_end_rule(rule_stack) {
        Some(resolved) -> resolved
        None ->
          case rule {
            BeginWhileRule(while_pattern: pat, ..) -> pat
            _ -> ""
          }
      }

      // Compile while pattern
      let allow_a = is_first_line && line_pos == 0
      let allow_g = line_pos == anchor_pos
      let compiled =
        compile_while_pattern(grammar, rule, while_pattern, allow_a, allow_g)

      // Try to match
      case find_next_match(compiled, line_text, line_pos, allow_a, allow_g) {
        None -> {
          // While condition failed - pop stack to this point
          #(pop(rule_stack), line_pos, anchor_pos, is_first_line, line_tokens)
        }
        Some(FindMatchResult(matched_rule_id, capture_indices)) -> {
          let #(match_start, match_end) = get_match_bounds(capture_indices)

          // Check if it's the while rule that matched (not something else)
          case get_rule_id(rule_stack) {
            Some(expected_id) if matched_rule_id == expected_id -> {
              // While matched - produce tokens from captures
              // This mirrors vscode-textmate's _checkWhileConditions behavior
              let line_tokens = produce(line_tokens, rule_stack, match_start)

              // Handle while captures
              let while_captures = rule.get_while_captures(rule)
              let line_tokens =
                handle_captures(
                  grammar,
                  line_text,
                  is_first_line,
                  rule_stack,
                  line_tokens,
                  while_captures,
                  capture_indices,
                )

              let line_tokens = produce(line_tokens, rule_stack, match_end)

              // Update position and continue
              let new_anchor_pos = match_end
              let new_line_pos = match_end
              let new_is_first_line = case match_end > line_pos {
                True -> False
                False -> is_first_line
              }
              check_while_rules(
                grammar,
                line_text,
                line_length,
                stack,
                rest,
                new_line_pos,
                new_anchor_pos,
                new_is_first_line,
                line_tokens,
              )
            }
            _ -> {
              // Wrong rule matched - while condition failed
              #(
                pop(rule_stack),
                line_pos,
                anchor_pos,
                is_first_line,
                line_tokens,
              )
            }
          }
        }
      }
    }
  }
}

// Helper functions

/// Get match start and end from capture indices
fn get_match_bounds(capture_indices: List(CaptureIndex)) -> #(Int, Int) {
  case capture_indices {
    [CaptureIndex(start: s, end: e), ..] -> #(s, e)
    [] -> #(0, 0)
  }
}

/// Get resolved scope name from a rule
fn get_resolved_name(
  rule: Rule,
  line_text: String,
  capture_indices: List(CaptureIndex),
) -> Option(String) {
  case rule.get_name(rule) {
    None -> None
    Some(name) -> Some(resolve_capture_refs(name, line_text, capture_indices))
  }
}

/// Get resolved content name from a rule
fn get_resolved_content_name(
  rule: Rule,
  line_text: String,
  capture_indices: List(CaptureIndex),
) -> Option(String) {
  case rule.get_content_name(rule) {
    None -> None
    Some(name) -> Some(resolve_capture_refs(name, line_text, capture_indices))
  }
}

/// Get captures for different rule types
fn get_match_captures(rule: Rule) -> List(Option(CaptureRule)) {
  rule.get_match_captures(rule)
}

fn get_begin_captures(rule: Rule) -> List(Option(CaptureRule)) {
  rule.get_begin_captures(rule)
}

fn get_end_captures(rule: Rule) -> List(Option(CaptureRule)) {
  rule.get_end_captures(rule)
}

/// Push a scope for a match
fn push_scope_for_match(
  stack: StateStack,
  scope_name: Option(String),
  rule_id: RuleId,
) -> StateStack {
  // Create new stack frame with scope
  case get_content_name_scopes(stack) {
    None -> stack
    Some(scopes) -> {
      let new_scopes = case scope_name {
        None -> scopes
        Some(name) -> attributed_push_scope(scopes, name, 0)
      }
      push(
        stack,
        rule_id,
        0,
        -1,
        False,
        None,
        Some(new_scopes),
        Some(new_scopes),
      )
    }
  }
}

/// Push state for a BeginEndRule
fn push_begin_end_state(
  stack: StateStack,
  rule_id: RuleId,
  enter_pos: Int,
  anchor_pos: Int,
  captured_eol: Bool,
  end_rule: Option(String),
  scope_name: Option(String),
) -> StateStack {
  case get_content_name_scopes(stack) {
    None ->
      push(
        stack,
        rule_id,
        enter_pos,
        anchor_pos,
        captured_eol,
        end_rule,
        None,
        None,
      )
    Some(scopes) -> {
      let new_scopes = case scope_name {
        None -> scopes
        Some(name) -> attributed_push_scope(scopes, name, 0)
      }
      push(
        stack,
        rule_id,
        enter_pos,
        anchor_pos,
        captured_eol,
        end_rule,
        Some(new_scopes),
        Some(new_scopes),
      )
    }
  }
}

/// Push state for a BeginWhileRule
fn push_begin_while_state(
  stack: StateStack,
  rule_id: RuleId,
  enter_pos: Int,
  anchor_pos: Int,
  captured_eol: Bool,
  while_rule: Option(String),
  scope_name: Option(String),
) -> StateStack {
  // BeginWhileRule uses end_rule field to store the while pattern
  push_begin_end_state(
    stack,
    rule_id,
    enter_pos,
    anchor_pos,
    captured_eol,
    while_rule,
    scope_name,
  )
}

/// Push content name scope
fn push_content_name_scope(
  stack: StateStack,
  content_name: Option(String),
) -> StateStack {
  case content_name {
    None -> stack
    Some(name) ->
      case get_name_scopes(stack) {
        None -> stack
        Some(name_scopes) -> {
          let content_scopes = attributed_push_scope(name_scopes, name, 0)
          set_content_name_scopes(stack, Some(content_scopes))
        }
      }
  }
}
