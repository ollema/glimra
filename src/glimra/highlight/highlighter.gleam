//// Highlighter - Main entry point for syntax highlighting
////
//// This module provides the main API for highlighting code.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/string
import glimra/highlight/registry.{type Registry}
import glimra/textmate/color_map
import glimra/textmate/grammar.{type Grammar}
import glimra/textmate/scope_matcher
import glimra/textmate/state_stack.{type StateStack}
import glimra/textmate/textmate
import glimra/textmate/theme.{type Theme}
import glimra/types/token.{
  type RawThemeSetting, type ThemedToken, type ThemedTokenExplanation,
  type ThemedTokenScopeExplanation, ThemedToken, ThemedTokenExplanation,
  ThemedTokenScopeExplanation,
}
import glimra/utils/strings.{type Line, split_lines}

/// Options for highlighting
pub type HighlightOptions {
  HighlightOptions(
    /// Language to use for tokenization
    lang: String,
    /// Theme to use for coloring
    theme: String,
    /// Color replacements (color -> replacement)
    color_replacements: Dict(String, String),
    /// Whether to include explanations
    include_explanation: ExplanationMode,
    /// Maximum line length before skipping tokenization
    tokenize_max_line_length: Int,
  )
}

/// Explanation mode for tokens
pub type ExplanationMode {
  /// No explanation
  NoExplanation
  /// Only scope names
  ScopeNameOnly
  /// Full explanation with theme matches
  FullExplanation
}

/// Result of highlighting
pub type HighlightResult {
  HighlightResult(
    /// Themed tokens per line
    tokens: List(List(ThemedToken)),
    /// Default foreground color
    fg: String,
    /// Default background color
    bg: String,
    /// Theme name
    theme_name: String,
  )
}

/// Highlight code
pub fn highlight(
  registry: Registry,
  code: String,
  options: HighlightOptions,
) -> Result(#(Registry, HighlightResult), String) {
  let resolved_lang = registry.resolve_alias(registry, options.lang)

  // Handle plain text or none theme
  let is_plain = case resolved_lang {
    "text" | "plaintext" | "txt" | "plain" -> True
    _ -> False
  }
  let is_none_theme = case options.theme {
    "none" | "" -> True
    _ -> False
  }
  case is_plain || is_none_theme {
    True -> {
      // Return unstyled tokens
      let tokens =
        split_lines(code)
        |> list.map(fn(line: Line) {
          let #(content, offset) = line
          [token.simple_token(content, offset)]
        })
      Ok(#(
        registry,
        HighlightResult(tokens: tokens, fg: "", bg: "", theme_name: ""),
      ))
    }
    False -> {
      // Get grammar
      case registry.get_grammar(registry, resolved_lang) {
        Error(e) -> Error(e)
        Ok(#(reg1, grammar)) -> {
          // Get theme
          case registry.get_theme(reg1, options.theme) {
            Error(e) -> Error(e)
            Ok(theme) -> {
              let result = tokenize_with_theme(code, grammar, theme, options)
              Ok(#(
                reg1,
                HighlightResult(
                  tokens: result,
                  fg: theme.fg,
                  bg: theme.bg,
                  theme_name: theme.name,
                ),
              ))
            }
          }
        }
      }
    }
  }
}

/// Tokenize code with theme
fn tokenize_with_theme(
  code: String,
  grammar: Grammar,
  theme: Theme,
  options: HighlightOptions,
) -> List(List(ThemedToken)) {
  let lines = split_lines(code)

  // Tokenize all lines
  let #(tokens, _final_state) =
    tokenize_lines(
      lines,
      grammar,
      theme,
      options.color_replacements,
      options.include_explanation,
      options.tokenize_max_line_length,
      textmate.initial(),
      [],
    )

  list.reverse(tokens)
}

/// Tokenize all lines
fn tokenize_lines(
  lines: List(Line),
  grammar: Grammar,
  theme: Theme,
  color_replacements: Dict(String, String),
  include_explanation: ExplanationMode,
  tokenize_max_line_length: Int,
  state: StateStack,
  acc: List(List(ThemedToken)),
) -> #(List(List(ThemedToken)), StateStack) {
  case lines {
    [] -> #(acc, state)
    [#(line, line_offset), ..rest] -> {
      let #(line_tokens, new_state) =
        tokenize_single_line(
          line,
          line_offset,
          grammar,
          theme,
          color_replacements,
          include_explanation,
          tokenize_max_line_length,
          state,
        )

      tokenize_lines(
        rest,
        grammar,
        theme,
        color_replacements,
        include_explanation,
        tokenize_max_line_length,
        new_state,
        [line_tokens, ..acc],
      )
    }
  }
}

/// Tokenize a single line
fn tokenize_single_line(
  line: String,
  line_offset: Int,
  grammar: Grammar,
  theme: Theme,
  color_replacements: Dict(String, String),
  include_explanation: ExplanationMode,
  tokenize_max_line_length: Int,
  state: StateStack,
) -> #(List(ThemedToken), StateStack) {
  // Check max line length (skip tokenization for very long lines)
  case
    tokenize_max_line_length > 0
    && string.length(line) >= tokenize_max_line_length
  {
    True -> {
      // Return single unstyled token for long lines
      #(
        [
          ThemedToken(
            content: line,
            offset: line_offset,
            color: "",
            font_style: 0,
            explanation: [],
          ),
        ],
        state,
      )
    }
    False -> {
      // Tokenize using textmate
      // We MUST tokenize even empty lines to properly handle state transitions
      // (e.g., while conditions that fail on empty lines)
      let result = textmate.tokenize_line(grammar, line, state)

      // For empty lines, return empty tokens but use the updated state
      case string.is_empty(line) {
        True -> #([], result.rule_stack)
        False -> {
          // Convert tokens with scopes to themed tokens
          let themed_tokens =
            theme_tokens(
              result.tokens,
              line,
              line_offset,
              theme,
              color_replacements,
              include_explanation,
            )

          #(themed_tokens, result.rule_stack)
        }
      }
    }
  }
}

/// Standard token types (matches vscode-textmate)
const token_type_other = 0

const token_type_comment = 1

const token_type_string = 2

const token_type_regex = 3

/// Convert tokens with scopes to themed tokens
fn theme_tokens(
  tokens: List(textmate.TokenWithScopes),
  line: String,
  line_offset: Int,
  theme: Theme,
  color_replacements: Dict(String, String),
  include_explanation: ExplanationMode,
) -> List(ThemedToken) {
  // Convert tokens and track their token types for proper merging
  let themed_with_types =
    list.map(tokens, fn(token) {
      let themed =
        theme_single_token(
          token,
          line,
          line_offset,
          theme,
          color_replacements,
          include_explanation,
        )
      let token_type = get_token_type(token.scopes)
      #(themed, token_type)
    })

  // Merge adjacent tokens with the same style AND token type
  // This matches vscode-textmate's tokenizeLine2 behavior
  merge_tokens_by_metadata(themed_with_types)
}

/// Determine the standard token type from scopes (matches vscode-textmate)
/// vscode-textmate computes token type per-scope during stack push, with later
/// scopes overriding earlier ones. We simulate this by iterating through all
/// scopes and letting later matches override.
fn get_token_type(scopes: List(String)) -> Int {
  get_token_type_from_scopes_acc(scopes, token_type_other)
}

/// Iterate through all scopes, accumulating token type.
/// Later scopes override earlier ones (e.g., meta.embedded overrides string).
fn get_token_type_from_scopes_acc(
  scopes: List(String),
  current_type: Int,
) -> Int {
  case scopes {
    [] -> current_type
    [scope, ..rest] -> {
      let new_type = get_scope_token_type(scope, current_type)
      get_token_type_from_scopes_acc(rest, new_type)
    }
  }
}

/// Get token type for a single scope, or keep current if no match.
/// Matches vscode-textmate's _toStandardTokenType + mergeAttributes logic.
fn get_scope_token_type(scope: String, current_type: Int) -> Int {
  // Match vscode-textmate's STANDARD_TOKEN_TYPE_REGEXP: /\b(comment|string|regex|meta\.embedded)\b/
  // Uses word boundary matching - the word can appear anywhere in the scope,
  // not just at the start. "punctuation.definition.string.begin" matches "string".
  case has_word_boundary_match(scope, "meta.embedded") {
    True -> token_type_other
    False ->
      case has_word_boundary_match(scope, "comment") {
        True -> token_type_comment
        False ->
          case has_word_boundary_match(scope, "string") {
            True -> token_type_string
            False ->
              case has_word_boundary_match(scope, "regex") {
                True -> token_type_regex
                False -> current_type
              }
          }
      }
  }
}

/// Check if a word appears at a word boundary in the scope.
/// Word boundaries are: start of string, end of string, or "." character.
/// This matches the JavaScript regex \b for scope names.
fn has_word_boundary_match(scope: String, word: String) -> Bool {
  // Fast path: check if word is even contained
  case string.contains(scope, word) {
    False -> False
    True -> {
      // Check all occurrences to find one at a word boundary
      has_word_at_boundary(scope, word, 0)
    }
  }
}

/// Recursively check for word at a boundary position
fn has_word_at_boundary(scope: String, word: String, start_pos: Int) -> Bool {
  let scope_len = string.length(scope)
  let word_len = string.length(word)

  // Find the word in the remaining string
  let remaining = string.drop_start(scope, start_pos)
  case find_substring_position(remaining, word) {
    Error(Nil) -> False
    Ok(rel_pos) -> {
      let pos = start_pos + rel_pos
      let end_pos = pos + word_len

      // Check if this occurrence is at word boundaries
      let at_start_boundary = pos == 0 || is_word_boundary_char(scope, pos - 1)
      let at_end_boundary =
        end_pos >= scope_len || is_word_boundary_char(scope, end_pos)

      case at_start_boundary && at_end_boundary {
        True -> True
        False -> has_word_at_boundary(scope, word, pos + 1)
      }
    }
  }
}

/// Check if character at position is a word boundary (. or string edge)
fn is_word_boundary_char(s: String, pos: Int) -> Bool {
  case string.drop_start(s, pos) |> string.first {
    Ok(".") -> True
    _ -> False
  }
}

/// Find position of substring in string
fn find_substring_position(haystack: String, needle: String) -> Result(Int, Nil) {
  find_substring_impl(haystack, needle, 0)
}

fn find_substring_impl(
  haystack: String,
  needle: String,
  pos: Int,
) -> Result(Int, Nil) {
  let needle_len = string.length(needle)
  let remaining_len = string.length(haystack) - pos

  case remaining_len < needle_len {
    True -> Error(Nil)
    False -> {
      let candidate = string.slice(haystack, pos, needle_len)
      case candidate == needle {
        True -> Ok(pos)
        False -> find_substring_impl(haystack, needle, pos + 1)
      }
    }
  }
}

/// Merge adjacent tokens with the same color, font style, and token type
fn merge_tokens_by_metadata(
  tokens: List(#(ThemedToken, Int)),
) -> List(ThemedToken) {
  case tokens {
    [] -> []
    [first, ..rest] -> merge_tokens_loop(rest, first, [])
  }
}

fn merge_tokens_loop(
  tokens: List(#(ThemedToken, Int)),
  current: #(ThemedToken, Int),
  acc: List(ThemedToken),
) -> List(ThemedToken) {
  let #(current_token, current_type) = current
  case tokens {
    [] -> list.reverse([current_token, ..acc])
    [next, ..rest] -> {
      let #(next_token, next_type) = next
      // Merge if same color, font style, AND token type
      case
        current_token.color == next_token.color
        && current_token.font_style == next_token.font_style
        && current_type == next_type
      {
        True -> {
          // Merge: combine content, keep current offset
          let merged =
            ThemedToken(
              content: current_token.content <> next_token.content,
              offset: current_token.offset,
              color: current_token.color,
              font_style: current_token.font_style,
              explanation: list.append(
                current_token.explanation,
                next_token.explanation,
              ),
            )
          merge_tokens_loop(rest, #(merged, current_type), acc)
        }
        False -> {
          // Don't merge: add current to result, continue with next
          merge_tokens_loop(rest, next, [current_token, ..acc])
        }
      }
    }
  }
}

/// Theme a single token
fn theme_single_token(
  token: textmate.TokenWithScopes,
  line: String,
  line_offset: Int,
  theme: Theme,
  color_replacements: Dict(String, String),
  include_explanation: ExplanationMode,
) -> ThemedToken {
  // Build scope stack
  let scope_stack = scope_matcher.from_scopes(token.scopes)

  // Match against theme
  let style = theme.match_scope(theme, scope_stack)

  // Get color from color map, applying any replacements
  let raw_color = color_map.get_color(theme.color_map, style.foreground_id)
  let color = case dict.get(color_replacements, string.lowercase(raw_color)) {
    Ok(replacement) -> replacement
    Error(_) -> raw_color
  }

  // Get token content with defensive clamping to avoid cross-line bleeding.
  // The tokenizer appends \n to lines for pattern matching, so indices may
  // extend beyond the actual line length.
  let line_len = string.length(line)
  let safe_start = int.min(token.start_index, line_len)
  let safe_end = int.min(token.end_index, line_len)
  let content = string_slice(line, safe_start, safe_end)

  // Build explanation if needed
  let explanation = case include_explanation {
    NoExplanation -> []
    ScopeNameOnly -> explain_scopes_name_only(token.scopes)
    FullExplanation ->
      explain_scopes_full(theme.settings, token.scopes, content)
  }

  ThemedToken(
    content: content,
    offset: line_offset + token.start_index,
    color: color,
    font_style: normalize_font_style(style.font_style),
    explanation: explanation,
  )
}

/// Normalize font style (convert -1 to 0)
fn normalize_font_style(font_style: Int) -> Int {
  case font_style < 0 {
    True -> 0
    False -> font_style
  }
}

/// Slice a string from start to end index using grapheme-correct indexing.
/// This is necessary because string.drop_start uses byte/codeunit counting
/// which doesn't match the grapheme indices from the tokenizer.
fn string_slice(str: String, start: Int, end: Int) -> String {
  str
  |> string.to_graphemes
  |> list.drop(start)
  |> list.take(end - start)
  |> string.join("")
}

/// Explain scopes with just names (no theme matches)
fn explain_scopes_name_only(
  scopes: List(String),
) -> List(ThemedTokenExplanation) {
  [
    ThemedTokenExplanation(
      content: "",
      scopes: list.map(scopes, fn(scope) {
        ThemedTokenScopeExplanation(scope_name: scope, theme_matches: [])
      }),
    ),
  ]
}

/// Explain scopes with full theme matching
fn explain_scopes_full(
  theme_settings: List(RawThemeSetting),
  scopes: List(String),
  content: String,
) -> List(ThemedTokenExplanation) {
  let scope_explanations =
    explain_scopes_full_loop(theme_settings, scopes, 0, [])

  [ThemedTokenExplanation(content: content, scopes: scope_explanations)]
}

fn explain_scopes_full_loop(
  theme_settings: List(RawThemeSetting),
  scopes: List(String),
  index: Int,
  acc: List(ThemedTokenScopeExplanation),
) -> List(ThemedTokenScopeExplanation) {
  case list.drop(scopes, index) {
    [] -> list.reverse(acc)
    [scope, ..] -> {
      let parent_scopes = list.take(scopes, index)
      let matches = find_theme_matches(theme_settings, scope, parent_scopes)
      let explanation =
        ThemedTokenScopeExplanation(scope_name: scope, theme_matches: matches)
      explain_scopes_full_loop(theme_settings, scopes, index + 1, [
        explanation,
        ..acc
      ])
    }
  }
}

/// Find theme settings that match a scope
fn find_theme_matches(
  settings: List(RawThemeSetting),
  scope: String,
  parent_scopes: List(String),
) -> List(RawThemeSetting) {
  list.filter(settings, fn(setting) {
    matches_setting(setting, scope, parent_scopes)
  })
}

/// Check if a theme setting matches a scope
fn matches_setting(
  setting: RawThemeSetting,
  scope: String,
  parent_scopes: List(String),
) -> Bool {
  // Parse setting scope into selectors
  let selectors = parse_setting_scope(setting.scope)

  list.any(selectors, fn(selector_pieces) {
    matches_selector(selector_pieces, scope, parent_scopes)
  })
}

/// Parse a setting scope string into selectors
fn parse_setting_scope(scope_str: String) -> List(List(String)) {
  scope_str
  |> string.split(",")
  |> list.map(fn(s) { string.trim(s) |> string.split(" ") })
}

/// Check if a selector matches a scope with parent scopes
fn matches_selector(
  selectors: List(String),
  scope: String,
  parent_scopes: List(String),
) -> Bool {
  case list.last(selectors) {
    Error(Nil) -> False
    Ok(last_selector) -> {
      case scope_matcher.matches_scope(scope, last_selector) {
        False -> False
        True -> {
          let selector_parents =
            selectors
            |> list.reverse
            |> list.drop(1)
            |> list.reverse
          matches_parents(selector_parents, parent_scopes)
        }
      }
    }
  }
}

/// Check if selector parents match in parent scopes
fn matches_parents(
  selector_parents: List(String),
  parent_scopes: List(String),
) -> Bool {
  matches_parents_loop(
    list.reverse(selector_parents),
    list.reverse(parent_scopes),
  )
}

fn matches_parents_loop(
  selector_parents: List(String),
  parent_scopes: List(String),
) -> Bool {
  case selector_parents {
    [] -> True
    [selector, ..rest_selectors] -> {
      case parent_scopes {
        [] -> False
        [parent, ..rest_parents] -> {
          case scope_matcher.matches_scope(parent, selector) {
            True -> matches_parents_loop(rest_selectors, rest_parents)
            False -> matches_parents_loop(selector_parents, rest_parents)
          }
        }
      }
    }
  }
}
