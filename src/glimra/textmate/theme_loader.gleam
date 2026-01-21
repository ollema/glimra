//// Theme Loader - Parse theme JSON into Theme structures
////
//// This module parses VS Code/TextMate theme JSON files and builds
//// the Theme structure with its ThemeTrie for efficient scope matching.

import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import glimra/textmate/color_map.{type ColorMap}
import glimra/textmate/theme.{
  type ParsedThemeRule, type StyleAttributes, type Theme, type ThemeTrieElement,
  ParsedThemeRule, StyleAttributes, Theme,
}
import glimra/types/token.{type RawThemeSetting, RawThemeSetting}

/// Raw theme structure from JSON parsing
pub type RawTheme {
  RawTheme(
    name: String,
    display_name: String,
    fg: String,
    bg: String,
    token_colors: List(RawTokenColor),
  )
}

/// Raw token color from JSON
pub type RawTokenColor {
  RawTokenColor(name: String, scope: TokenScope, settings: TokenSettings)
}

/// Scope can be a string or list of strings
pub type TokenScope {
  ScopeString(String)
  ScopeList(List(String))
  ScopeNone
}

/// Token settings
pub type TokenSettings {
  TokenSettings(
    font_style: Option(String),
    foreground: String,
    background: String,
  )
}

/// Parse a theme JSON string into a Theme
pub fn parse_theme_json(json_string: String) -> Result(Theme, String) {
  case json.parse(json_string, raw_theme_decoder()) {
    Ok(raw) -> Ok(build_theme(raw))
    Error(e) -> Error("Theme JSON parse error: " <> json_error_to_string(e))
  }
}

fn json_error_to_string(error: json.DecodeError) -> String {
  case error {
    json.UnableToDecode(errors) -> {
      case errors {
        [] -> "unable to decode"
        [first, ..] -> decode_error_to_string(first)
      }
    }
    json.UnexpectedByte(byte) -> "unexpected byte: " <> byte
    json.UnexpectedSequence(seq) -> "unexpected sequence: " <> seq
    json.UnexpectedEndOfInput -> "unexpected end of input"
  }
}

fn decode_error_to_string(error: decode.DecodeError) -> String {
  "decode error at path " <> path_to_string(error.path)
}

fn path_to_string(path: List(String)) -> String {
  case path {
    [] -> "root"
    _ -> string.join(path, ".")
  }
}

// ============================================
// JSON Decoders
// ============================================

fn raw_theme_decoder() -> Decoder(RawTheme) {
  use name <- decode.optional_field("name", "", decode.string)
  use display_name <- decode.optional_field("displayName", "", decode.string)
  use colors <- decode.optional_field("colors", dict.new(), colors_decoder())
  use token_colors <- decode.optional_field(
    "tokenColors",
    [],
    decode.list(raw_token_color_decoder()),
  )

  // Extract fg/bg from colors or first tokenColor settings
  let fg = extract_fg(colors, token_colors)
  let bg = extract_bg(colors, token_colors)

  decode.success(RawTheme(
    name: case string.is_empty(name) {
      True -> display_name
      False -> name
    },
    display_name: display_name,
    fg: fg,
    bg: bg,
    token_colors: token_colors,
  ))
}

fn colors_decoder() -> Decoder(Dict(String, String)) {
  decode.dict(decode.string, decode.optional(decode.string))
  |> decode.map(fn(d) {
    dict.fold(d, dict.new(), fn(acc, k, v) {
      case v {
        Some(color) -> dict.insert(acc, k, color)
        None -> acc
      }
    })
  })
}

fn raw_token_color_decoder() -> Decoder(RawTokenColor) {
  use name <- decode.optional_field("name", "", decode.string)
  use scope <- decode.optional_field("scope", ScopeNone, scope_decoder())
  use settings <- decode.optional_field(
    "settings",
    TokenSettings(None, "", ""),
    token_settings_decoder(),
  )
  decode.success(RawTokenColor(name: name, scope: scope, settings: settings))
}

fn scope_decoder() -> Decoder(TokenScope) {
  decode.one_of(
    // Try string first
    decode.string |> decode.map(ScopeString),
    [
      // Then try list of strings
      decode.list(decode.string) |> decode.map(ScopeList),
    ],
  )
}

fn token_settings_decoder() -> Decoder(TokenSettings) {
  // Use Option(String) to distinguish missing fontStyle (None) from empty string (Some(""))
  use font_style <- decode.optional_field(
    "fontStyle",
    None,
    decode.string |> decode.map(Some),
  )
  use foreground <- decode.optional_field("foreground", "", decode.string)
  use background <- decode.optional_field("background", "", decode.string)
  decode.success(TokenSettings(
    font_style: font_style,
    foreground: foreground,
    background: background,
  ))
}

fn extract_fg(
  colors: Dict(String, String),
  token_colors: List(RawTokenColor),
) -> String {
  // Try editor.foreground from colors first
  case dict.get(colors, "editor.foreground") {
    Ok(fg) -> fg
    Error(Nil) -> {
      // Try foreground from colors
      case dict.get(colors, "foreground") {
        Ok(fg) -> fg
        Error(Nil) -> {
          // Try first tokenColor with no scope (default settings)
          case find_default_foreground(token_colors) {
            Some(fg) -> fg
            None -> "#000000"
          }
        }
      }
    }
  }
}

fn extract_bg(
  colors: Dict(String, String),
  token_colors: List(RawTokenColor),
) -> String {
  case dict.get(colors, "editor.background") {
    Ok(bg) -> bg
    Error(Nil) -> {
      case dict.get(colors, "background") {
        Ok(bg) -> bg
        Error(Nil) -> {
          case find_default_background(token_colors) {
            Some(bg) -> bg
            None -> "#ffffff"
          }
        }
      }
    }
  }
}

fn find_default_foreground(token_colors: List(RawTokenColor)) -> Option(String) {
  case token_colors {
    [] -> None
    [tc, ..rest] -> {
      case tc.scope {
        ScopeNone -> {
          case string.is_empty(tc.settings.foreground) {
            True -> find_default_foreground(rest)
            False -> Some(tc.settings.foreground)
          }
        }
        _ -> find_default_foreground(rest)
      }
    }
  }
}

fn find_default_background(token_colors: List(RawTokenColor)) -> Option(String) {
  case token_colors {
    [] -> None
    [tc, ..rest] -> {
      case tc.scope {
        ScopeNone -> {
          case string.is_empty(tc.settings.background) {
            True -> find_default_background(rest)
            False -> Some(tc.settings.background)
          }
        }
        _ -> find_default_background(rest)
      }
    }
  }
}

// ============================================
// Theme Building
// ============================================

/// Build a Theme from a RawTheme
fn build_theme(raw: RawTheme) -> Theme {
  // Parse theme rules
  let parsed_rules = parse_theme_rules(raw.token_colors)

  // Sort rules lexicographically
  let sorted_rules = sort_parsed_rules(parsed_rules)

  // Extract defaults and build trie
  let #(defaults, remaining_rules, initial_color_map) =
    extract_defaults(sorted_rules, raw.fg, raw.bg)

  // Build trie from remaining rules (updates color_map with all rule colors)
  let #(root, final_color_map) = build_trie(remaining_rules, initial_color_map)

  // Build settings for explanation feature
  let settings = build_raw_settings(raw.token_colors)

  Theme(
    name: raw.name,
    fg: raw.fg,
    bg: raw.bg,
    color_map: final_color_map,
    defaults: defaults,
    root: root,
    settings: settings,
  )
}

fn build_raw_settings(
  token_colors: List(RawTokenColor),
) -> List(RawThemeSetting) {
  list.filter_map(token_colors, fn(tc) {
    let scope_str = scope_to_string(tc.scope)
    case string.is_empty(scope_str) {
      True -> Error(Nil)
      False ->
        Ok(RawThemeSetting(
          name: tc.name,
          scope: scope_str,
          foreground: tc.settings.foreground,
        ))
    }
  })
}

fn scope_to_string(scope: TokenScope) -> String {
  case scope {
    ScopeNone -> ""
    ScopeString(s) -> s
    ScopeList(scopes) -> string.join(scopes, ", ")
  }
}

/// Parse raw token colors into parsed rules
fn parse_theme_rules(token_colors: List(RawTokenColor)) -> List(ParsedThemeRule) {
  parse_theme_rules_loop(token_colors, 0, [])
}

fn parse_theme_rules_loop(
  token_colors: List(RawTokenColor),
  index: Int,
  acc: List(ParsedThemeRule),
) -> List(ParsedThemeRule) {
  case token_colors {
    [] -> list.reverse(acc)
    [tc, ..rest] -> {
      // Get scopes as list
      let scopes = case tc.scope {
        ScopeNone -> [""]
        ScopeString(s) -> parse_scope_string(s)
        ScopeList(ss) -> ss
      }

      // Parse font style
      let font_style = parse_font_style(tc.settings.font_style)

      // Create rules for each scope
      let rules =
        list.map(scopes, fn(scope_str) {
          let trimmed = string.trim(scope_str)
          let segments = string.split(trimmed, " ")

          let #(scope, parent_scopes) = case list.reverse(segments) {
            [] -> #("", None)
            [s] -> #(s, None)
            [s, ..parents] -> #(s, Some(parents))
          }

          ParsedThemeRule(
            scope: scope,
            parent_scopes: parent_scopes,
            index: index,
            font_style: font_style,
            foreground: tc.settings.foreground,
            background: tc.settings.background,
          )
        })

      parse_theme_rules_loop(
        rest,
        index + 1,
        list.append(list.reverse(rules), acc),
      )
    }
  }
}

fn parse_scope_string(scope: String) -> List(String) {
  // Remove leading/trailing commas and split by comma
  scope
  |> string.trim
  |> string.replace(",", " , ")
  |> string.split(",")
  |> list.map(string.trim)
  |> list.filter(fn(s) { !string.is_empty(s) })
}

fn parse_font_style(style: Option(String)) -> Int {
  case style {
    // Missing fontStyle key → inherit from parent (-1)
    None -> theme.font_style_not_set
    // Empty string fontStyle → explicitly no style (0)
    Some("") -> theme.font_style_none
    // Non-empty fontStyle → parse the style values
    Some(s) -> {
      let segments = string.split(s, " ")
      parse_font_style_segments(segments, theme.font_style_none)
    }
  }
}

fn parse_font_style_segments(segments: List(String), acc: Int) -> Int {
  case segments {
    [] -> acc
    [seg, ..rest] -> {
      let addition = case seg {
        "italic" -> theme.font_style_italic
        "bold" -> theme.font_style_bold
        "underline" -> theme.font_style_underline
        "strikethrough" -> theme.font_style_strikethrough
        _ -> 0
      }
      parse_font_style_segments(rest, int.bitwise_or(acc, addition))
    }
  }
}

/// Sort rules lexicographically by scope, then parent scopes, then index
fn sort_parsed_rules(rules: List(ParsedThemeRule)) -> List(ParsedThemeRule) {
  list.sort(rules, compare_parsed_rules)
}

fn compare_parsed_rules(a: ParsedThemeRule, b: ParsedThemeRule) -> order.Order {
  case string.compare(a.scope, b.scope) {
    order.Eq -> {
      case compare_parent_scopes(a.parent_scopes, b.parent_scopes) {
        order.Eq -> int.compare(a.index, b.index)
        other -> other
      }
    }
    other -> other
  }
}

import gleam/order

fn compare_parent_scopes(
  a: Option(List(String)),
  b: Option(List(String)),
) -> order.Order {
  case a, b {
    None, None -> order.Eq
    None, Some(_) -> order.Lt
    Some(_), None -> order.Gt
    Some(a_scopes), Some(b_scopes) -> compare_string_lists(a_scopes, b_scopes)
  }
}

fn compare_string_lists(a: List(String), b: List(String)) -> order.Order {
  case a, b {
    [], [] -> order.Eq
    [], _ -> order.Lt
    _, [] -> order.Gt
    [x, ..xs], [y, ..ys] -> {
      case string.compare(x, y) {
        order.Eq -> compare_string_lists(xs, ys)
        other -> other
      }
    }
  }
}

/// Extract defaults from sorted rules (rules with empty scope)
fn extract_defaults(
  rules: List(ParsedThemeRule),
  default_fg: String,
  default_bg: String,
) -> #(StyleAttributes, List(ParsedThemeRule), ColorMap) {
  let #(defaults, remaining, color_map) =
    extract_defaults_loop(
      rules,
      theme.font_style_none,
      default_fg,
      default_bg,
      color_map.new(),
      [],
    )

  #(defaults, list.reverse(remaining), color_map)
}

fn extract_defaults_loop(
  rules: List(ParsedThemeRule),
  font_style: Int,
  fg: String,
  bg: String,
  color_map: ColorMap,
  remaining: List(ParsedThemeRule),
) -> #(StyleAttributes, List(ParsedThemeRule), ColorMap) {
  case rules {
    [] -> {
      // Get color IDs for defaults
      let #(cm1, fg_id) = color_map.get_id(color_map, fg)
      let #(cm2, bg_id) = color_map.get_id(cm1, bg)
      let defaults =
        StyleAttributes(
          font_style: font_style,
          foreground_id: fg_id,
          background_id: bg_id,
        )
      #(defaults, remaining, cm2)
    }
    [rule, ..rest] -> {
      case string.is_empty(rule.scope) {
        True -> {
          // This is a default rule
          let new_font_style = case
            rule.font_style == theme.font_style_not_set
          {
            True -> font_style
            False -> rule.font_style
          }
          let new_fg = case string.is_empty(rule.foreground) {
            True -> fg
            False -> rule.foreground
          }
          let new_bg = case string.is_empty(rule.background) {
            True -> bg
            False -> rule.background
          }
          extract_defaults_loop(
            rest,
            new_font_style,
            new_fg,
            new_bg,
            color_map,
            remaining,
          )
        }
        False -> {
          // Non-default rule, add to remaining
          extract_defaults_loop(
            rules |> list.drop(1),
            font_style,
            fg,
            bg,
            color_map,
            [rule, ..remaining],
          )
        }
      }
    }
  }
}

/// Build the theme trie from parsed rules
fn build_trie(
  rules: List(ParsedThemeRule),
  initial_color_map: ColorMap,
) -> #(ThemeTrieElement, ColorMap) {
  build_trie_loop(rules, theme.new_trie_element(), initial_color_map)
}

fn build_trie_loop(
  rules: List(ParsedThemeRule),
  root: ThemeTrieElement,
  color_map: ColorMap,
) -> #(ThemeTrieElement, ColorMap) {
  case rules {
    [] -> #(root, color_map)
    [rule, ..rest] -> {
      let #(cm1, fg_id) = color_map.get_id(color_map, rule.foreground)
      let #(cm2, bg_id) = color_map.get_id(cm1, rule.background)

      let new_root =
        theme.trie_insert(
          root,
          0,
          rule.scope,
          rule.parent_scopes,
          rule.font_style,
          fg_id,
          bg_id,
        )

      build_trie_loop(rest, new_root, cm2)
    }
  }
}
