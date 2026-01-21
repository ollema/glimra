//// Pure Gleam Theme implementation
////
//// This module provides theme types and the ThemeTrie data structure for
//// efficient scope-to-style matching, following the vscode-textmate algorithm.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import glimra/textmate/color_map.{type ColorMap}
import glimra/textmate/scope_matcher.{
  type ScopeStack, ScopeStack, ScopeStackNull,
}
import glimra/types/token.{type RawThemeSetting}

/// Font style flags (can be combined with bitwise OR)
pub const font_style_not_set = -1

pub const font_style_none = 0

pub const font_style_italic = 1

pub const font_style_bold = 2

pub const font_style_underline = 4

pub const font_style_strikethrough = 8

/// Style attributes for a token
pub type StyleAttributes {
  StyleAttributes(
    /// Font style flags (italic, bold, underline, strikethrough)
    font_style: Int,
    /// Foreground color ID in the color map
    foreground_id: Int,
    /// Background color ID in the color map
    background_id: Int,
  )
}

/// A complete theme with color map and matching trie
pub type Theme {
  Theme(
    /// Theme name
    name: String,
    /// Default foreground color
    fg: String,
    /// Default background color
    bg: String,
    /// Color map for ID ↔ color string conversion
    color_map: ColorMap,
    /// Default style attributes
    defaults: StyleAttributes,
    /// Root of the ThemeTrie for scope matching
    root: ThemeTrieElement,
    /// Original settings for explanation feature
    settings: List(RawThemeSetting),
  )
}

/// A parsed theme rule ready for building the trie
pub type ParsedThemeRule {
  ParsedThemeRule(
    /// The scope to match (e.g., "keyword.operator")
    scope: String,
    /// Parent scope constraints (reversed - deepest first)
    parent_scopes: Option(List(String)),
    /// Original index in the settings array (for stability)
    index: Int,
    /// Font style flags
    font_style: Int,
    /// Foreground color (or empty string)
    foreground: String,
    /// Background color (or empty string)
    background: String,
  )
}

/// A rule stored in the trie element
pub type ThemeTrieElementRule {
  ThemeTrieElementRule(
    /// Depth of the scope (number of segments)
    scope_depth: Int,
    /// Parent scope constraints (reversed - deepest first)
    parent_scopes: List(String),
    /// Font style flags
    font_style: Int,
    /// Foreground color ID
    foreground: Int,
    /// Background color ID
    background: Int,
  )
}

/// A node in the theme trie
pub type ThemeTrieElement {
  ThemeTrieElement(
    /// Main rule (no parent scope constraints)
    main_rule: ThemeTrieElementRule,
    /// Rules with parent scope constraints
    rules_with_parent_scopes: List(ThemeTrieElementRule),
    /// Children keyed by scope segment
    children: Dict(String, ThemeTrieElement),
  )
}

/// Match a scope path against the theme, returning style attributes
///
/// This function tries to match the innermost scope first. If no rule matches,
/// it recursively tries parent scopes to inherit styling. This matches
/// vscode-textmate's behavior where styling cascades from parent to child scopes.
pub fn match_scope(theme: Theme, scope_path: ScopeStack) -> StyleAttributes {
  case scope_path {
    ScopeStackNull -> theme.defaults
    ScopeStack(parent, scope_name) -> {
      let matching_rules = trie_match(theme.root, scope_name)

      // Find the first rule that matches parent scope constraints
      case find_matching_rule(matching_rules, parent) {
        Some(rule) -> {
          // Get parent scope's style for inheritance of unset values
          // This is critical: fontStyle should inherit from parent scope, not defaults
          let parent_style = match_scope(theme, parent)

          // If a matched rule doesn't specify a value (0 or -1), inherit from parent scope
          let font_style = case rule.font_style == font_style_not_set {
            True -> parent_style.font_style
            False -> rule.font_style
          }
          let foreground_id = case rule.foreground == 0 {
            True -> parent_style.foreground_id
            False -> rule.foreground
          }
          let background_id = case rule.background == 0 {
            True -> parent_style.background_id
            False -> rule.background
          }
          StyleAttributes(
            font_style: font_style,
            foreground_id: foreground_id,
            background_id: background_id,
          )
        }
        None -> {
          // No rule matches the innermost scope - try parent scopes
          // This allows styling to cascade from parent scopes
          match_scope(theme, parent)
        }
      }
    }
  }
}

fn find_matching_rule(
  rules: List(ThemeTrieElementRule),
  parent_path: ScopeStack,
) -> Option(ThemeTrieElementRule) {
  case rules {
    [] -> None
    [rule, ..rest] -> {
      // Only consider rules that have at least one meaningful value
      // A rule with fontStyle=-1, foreground=0, background=0 is just a placeholder
      let has_meaningful_value =
        rule.font_style != font_style_not_set
        || rule.foreground != 0
        || rule.background != 0

      case
        has_meaningful_value
        && scope_matcher.matches_parent_scopes(parent_path, rule.parent_scopes)
      {
        True -> Some(rule)
        False -> find_matching_rule(rest, parent_path)
      }
    }
  }
}

/// Match a scope name against the trie
fn trie_match(
  element: ThemeTrieElement,
  scope: String,
) -> List(ThemeTrieElementRule) {
  case string.is_empty(scope) {
    True -> collect_rules(element)
    False -> {
      // Split scope by "." to get head and tail
      let #(head, tail) = split_scope(scope)

      case dict.get(element.children, head) {
        Ok(child) -> trie_match(child, tail)
        Error(Nil) -> collect_rules(element)
      }
    }
  }
}

fn split_scope(scope: String) -> #(String, String) {
  case string.split_once(scope, ".") {
    Ok(#(head, tail)) -> #(head, tail)
    Error(Nil) -> #(scope, "")
  }
}

/// Collect and sort rules from a trie element
fn collect_rules(element: ThemeTrieElement) -> List(ThemeTrieElementRule) {
  let rules = [element.main_rule, ..element.rules_with_parent_scopes]
  sort_rules_by_specificity(rules)
}

/// Sort rules by specificity (most specific first)
fn sort_rules_by_specificity(
  rules: List(ThemeTrieElementRule),
) -> List(ThemeTrieElementRule) {
  list.sort(rules, compare_rule_specificity)
}

fn compare_rule_specificity(
  a: ThemeTrieElementRule,
  b: ThemeTrieElementRule,
) -> order.Order {
  // First compare scope depths (deeper = more specific)
  case int.compare(b.scope_depth, a.scope_depth) {
    order.Eq -> {
      // Then compare parent scope depths (sum of segment counts)
      // "meta.function.parameters" (3 segments) should beat "variable.other" (2 segments)
      let a_parent_depth = parent_scopes_depth(a.parent_scopes)
      let b_parent_depth = parent_scopes_depth(b.parent_scopes)
      int.compare(b_parent_depth, a_parent_depth)
    }
    other -> other
  }
}

/// Calculate total depth of parent scope constraints
fn parent_scopes_depth(parent_scopes: List(String)) -> Int {
  list.fold(parent_scopes, 0, fn(acc, scope) {
    // Count segments: "meta.function.parameters" has 3 segments
    acc
    + {
      1 + string.length(scope) - string.length(string.replace(scope, ".", ""))
    }
  })
}

import gleam/order

// ============================================
// Theme Trie Building
// ============================================

/// Create a new empty trie element
pub fn new_trie_element() -> ThemeTrieElement {
  ThemeTrieElement(
    main_rule: ThemeTrieElementRule(
      scope_depth: 0,
      parent_scopes: [],
      font_style: font_style_not_set,
      foreground: 0,
      background: 0,
    ),
    rules_with_parent_scopes: [],
    children: dict.new(),
  )
}

/// Insert a rule into the trie
pub fn trie_insert(
  element: ThemeTrieElement,
  scope_depth: Int,
  scope: String,
  parent_scopes: Option(List(String)),
  font_style: Int,
  foreground: Int,
  background: Int,
) -> ThemeTrieElement {
  case string.is_empty(scope) {
    True ->
      do_insert_here(
        element,
        scope_depth,
        parent_scopes,
        font_style,
        foreground,
        background,
      )
    False -> {
      let #(head, tail) = split_scope(scope)

      let child = case dict.get(element.children, head) {
        Ok(existing) -> existing
        Error(Nil) ->
          // Clone main rule and rules for the new child
          ThemeTrieElement(
            main_rule: clone_rule(element.main_rule),
            rules_with_parent_scopes: list.map(
              element.rules_with_parent_scopes,
              clone_rule,
            ),
            children: dict.new(),
          )
      }

      let updated_child =
        trie_insert(
          child,
          scope_depth + 1,
          tail,
          parent_scopes,
          font_style,
          foreground,
          background,
        )

      ThemeTrieElement(
        ..element,
        children: dict.insert(element.children, head, updated_child),
      )
    }
  }
}

fn do_insert_here(
  element: ThemeTrieElement,
  scope_depth: Int,
  parent_scopes: Option(List(String)),
  font_style: Int,
  foreground: Int,
  background: Int,
) -> ThemeTrieElement {
  case parent_scopes {
    None -> {
      // Merge into main rule
      let main_rule =
        accept_overwrite(
          element.main_rule,
          scope_depth,
          font_style,
          foreground,
          background,
        )
      ThemeTrieElement(..element, main_rule: main_rule)
    }
    Some(ps) -> {
      // Try to find and merge with existing rule with same parent scopes
      case
        find_rule_with_parent_scopes(element.rules_with_parent_scopes, ps, [])
      {
        Ok(#(found, rest)) -> {
          let updated =
            accept_overwrite(
              found,
              scope_depth,
              font_style,
              foreground,
              background,
            )
          ThemeTrieElement(..element, rules_with_parent_scopes: [
            updated,
            ..rest
          ])
        }
        Error(Nil) -> {
          // Create new rule, inheriting from main rule
          let actual_font_style = case font_style == font_style_not_set {
            True -> element.main_rule.font_style
            False -> font_style
          }
          let actual_foreground = case foreground == 0 {
            True -> element.main_rule.foreground
            False -> foreground
          }
          let actual_background = case background == 0 {
            True -> element.main_rule.background
            False -> background
          }
          let new_rule =
            ThemeTrieElementRule(
              scope_depth: scope_depth,
              parent_scopes: ps,
              font_style: actual_font_style,
              foreground: actual_foreground,
              background: actual_background,
            )
          ThemeTrieElement(..element, rules_with_parent_scopes: [
            new_rule,
            ..element.rules_with_parent_scopes
          ])
        }
      }
    }
  }
}

fn find_rule_with_parent_scopes(
  rules: List(ThemeTrieElementRule),
  target: List(String),
  checked: List(ThemeTrieElementRule),
) -> Result(#(ThemeTrieElementRule, List(ThemeTrieElementRule)), Nil) {
  case rules {
    [] -> Error(Nil)
    [rule, ..rest] -> {
      case rule.parent_scopes == target {
        True -> Ok(#(rule, list.append(list.reverse(checked), rest)))
        False -> find_rule_with_parent_scopes(rest, target, [rule, ..checked])
      }
    }
  }
}

fn accept_overwrite(
  rule: ThemeTrieElementRule,
  scope_depth: Int,
  font_style: Int,
  foreground: Int,
  background: Int,
) -> ThemeTrieElementRule {
  let new_scope_depth = case rule.scope_depth > scope_depth {
    True -> rule.scope_depth
    False -> scope_depth
  }
  let new_font_style = case font_style == font_style_not_set {
    True -> rule.font_style
    False -> font_style
  }
  let new_foreground = case foreground == 0 {
    True -> rule.foreground
    False -> foreground
  }
  let new_background = case background == 0 {
    True -> rule.background
    False -> background
  }
  ThemeTrieElementRule(
    scope_depth: new_scope_depth,
    parent_scopes: rule.parent_scopes,
    font_style: new_font_style,
    foreground: new_foreground,
    background: new_background,
  )
}

fn clone_rule(rule: ThemeTrieElementRule) -> ThemeTrieElementRule {
  ThemeTrieElementRule(
    scope_depth: rule.scope_depth,
    parent_scopes: rule.parent_scopes,
    font_style: rule.font_style,
    foreground: rule.foreground,
    background: rule.background,
  )
}
