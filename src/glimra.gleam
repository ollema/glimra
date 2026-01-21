//// Glimra - Zero Runtime Syntax highlighter for Gleam
////
//// This module provides the public API for Glimra.
//// TBC

import gleam/dict.{type Dict}
import gleam/list
import gleam/set
import glimra/ffi/onig_scanner
import glimra/highlight/highlighter as internal_highlighter
import glimra/highlight/registry.{type Registry}
import glimra/languages.{type Language, all_languages, language_id}
import glimra/themes.{
  type BundledTheme, type Theme, Bundled, all_themes, theme_id, theme_name,
}
import glimra/types/token.{type ThemedToken}
import glimra/utils/strings.{type Line, split_lines}
import simplifile

// ============================================================================
// RESULT TYPES
// ============================================================================

/// Result of codeToTokens, containing 2D array of tokens and meta info
pub type TokensResult {
  TokensResult(
    /// 2D array of tokens, first dimension is lines, second dimension is tokens in a line
    tokens: List(List(ThemedToken)),
    /// Foreground color of the code
    fg: String,
    /// Background color of the code
    bg: String,
    /// Name of the theme used
    theme_name: String,
  )
}

// ============================================================================
// HIGHLIGHTER BUILDER (with phantom types for safety)
// ============================================================================

/// Phantom type indicating no languages have been added
pub type NoLangs

/// Phantom type indicating at least one language has been added
pub type HasLangs

/// Phantom type indicating no theme has been added
pub type NoTheme

/// Phantom type indicating at least one theme has been added
pub type HasTheme

/// Builder for creating a Highlighter.
/// Uses phantom types to ensure at compile time that at least one
/// language and one theme are added before building.
pub opaque type HighlighterBuilder(has_langs, has_theme) {
  HighlighterBuilder(
    languages: List(Language),
    themes: List(Theme),
    lang_aliases: Dict(String, String),
  )
}

/// A configured highlighter ready for tokenization.
pub opaque type Highlighter {
  Highlighter(
    registry: Registry,
    loaded_languages: List(Language),
    loaded_themes: List(Theme),
    lang_aliases: Dict(String, String),
  )
}

/// Create a new highlighter builder.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(highlighter) =
///   new_highlighter()
///   |> with_language(Javascript)
///   |> with_bundled_theme(Nord)
///   |> build()
/// ```
pub fn new_highlighter() -> HighlighterBuilder(NoLangs, NoTheme) {
  HighlighterBuilder(languages: [], themes: [], lang_aliases: dict.new())
}

/// Add a single language to the highlighter builder.
/// Transitions NoLangs -> HasLangs on first call.
pub fn with_language(
  builder: HighlighterBuilder(langs, theme),
  lang: Language,
) -> HighlighterBuilder(HasLangs, theme) {
  HighlighterBuilder(
    languages: [lang, ..builder.languages],
    themes: builder.themes,
    lang_aliases: builder.lang_aliases,
  )
}

/// Add multiple languages to the highlighter builder.
/// Transitions NoLangs -> HasLangs on first call.
pub fn with_languages(
  builder: HighlighterBuilder(langs, theme),
  langs: List(Language),
) -> HighlighterBuilder(HasLangs, theme) {
  HighlighterBuilder(
    languages: list.append(langs, builder.languages),
    themes: builder.themes,
    lang_aliases: builder.lang_aliases,
  )
}

/// Add all bundled languages to the highlighter builder.
pub fn with_all_languages(
  builder: HighlighterBuilder(langs, theme),
) -> HighlighterBuilder(HasLangs, theme) {
  HighlighterBuilder(
    languages: all_languages(),
    themes: builder.themes,
    lang_aliases: builder.lang_aliases,
  )
}

/// Add a single bundled theme to the highlighter builder.
/// Transitions NoTheme -> HasTheme on first call.
pub fn with_bundled_theme(
  builder: HighlighterBuilder(langs, theme),
  t: BundledTheme,
) -> HighlighterBuilder(langs, HasTheme) {
  HighlighterBuilder(
    languages: builder.languages,
    themes: [Bundled(t), ..builder.themes],
    lang_aliases: builder.lang_aliases,
  )
}

/// Add multiple bundled themes to the highlighter builder.
/// Transitions NoTheme -> HasTheme on first call.
pub fn with_bundled_themes(
  builder: HighlighterBuilder(langs, theme),
  themes: List(BundledTheme),
) -> HighlighterBuilder(langs, HasTheme) {
  let new_themes = list.map(themes, Bundled)
  HighlighterBuilder(
    languages: builder.languages,
    themes: list.append(new_themes, builder.themes),
    lang_aliases: builder.lang_aliases,
  )
}

/// Add all bundled themes to the highlighter builder.
pub fn with_all_bundled_themes(
  builder: HighlighterBuilder(langs, theme),
) -> HighlighterBuilder(langs, HasTheme) {
  let new_themes = list.map(all_themes(), Bundled)
  HighlighterBuilder(
    languages: builder.languages,
    themes: new_themes,
    lang_aliases: builder.lang_aliases,
  )
}

/// Add a custom theme with JSON content to the highlighter builder.
/// Transitions NoTheme -> HasTheme on first call.
pub fn with_custom_theme(
  builder: HighlighterBuilder(langs, theme),
  name: String,
  json: String,
) -> HighlighterBuilder(langs, HasTheme) {
  HighlighterBuilder(
    languages: builder.languages,
    themes: [themes.Custom(name, json), ..builder.themes],
    lang_aliases: builder.lang_aliases,
  )
}

/// Add a language alias mapping to the highlighter builder.
/// This allows using alternative names for languages.
///
/// ## Examples
///
/// ```gleam
/// new_highlighter()
///   |> with_language(Javascript)
///   |> with_lang_alias("js", Javascript)
///   |> with_bundled_theme(Nord)
///   |> build()
/// ```
pub fn with_lang_alias(
  builder: HighlighterBuilder(langs, theme),
  alias: String,
  target: Language,
) -> HighlighterBuilder(langs, theme) {
  HighlighterBuilder(
    languages: builder.languages,
    themes: builder.themes,
    lang_aliases: dict.insert(builder.lang_aliases, alias, language_id(target)),
  )
}

/// Build the highlighter.
/// Requires that at least one language and one theme have been added
/// (enforced at compile time through phantom types).
pub fn build(
  builder: HighlighterBuilder(HasLangs, HasTheme),
) -> Result(Highlighter, String) {
  // 1. Expand language list with dependencies
  let all_langs = expand_dependencies(builder.languages)

  // 2. Create registry
  let reg = registry.new(onig_scanner.create_scanner)

  // 3. Load all grammars
  case load_grammars(reg, all_langs) {
    Error(e) -> Error(e)
    Ok(reg) -> {
      // 4. Load all themes
      case load_themes(reg, builder.themes) {
        Error(e) -> Error(e)
        Ok(reg) ->
          Ok(Highlighter(
            registry: reg,
            loaded_languages: all_langs,
            loaded_themes: builder.themes,
            lang_aliases: builder.lang_aliases,
          ))
      }
    }
  }
}

/// Expand a list of languages to include all their dependencies
fn expand_dependencies(langs: List(Language)) -> List(Language) {
  let all_with_deps =
    list.flat_map(langs, fn(lang) {
      [lang, ..languages.language_dependencies(lang)]
    })

  // Remove duplicates while preserving order
  let seen = set.new()
  unique_languages(all_with_deps, seen, [])
}

fn unique_languages(
  langs: List(Language),
  seen: set.Set(String),
  acc: List(Language),
) -> List(Language) {
  case langs {
    [] -> list.reverse(acc)
    [lang, ..rest] -> {
      let name = language_id(lang)
      case set.contains(seen, name) {
        True -> unique_languages(rest, seen, acc)
        False -> unique_languages(rest, set.insert(seen, name), [lang, ..acc])
      }
    }
  }
}

const grammar_dir = "priv/grammars/"

const theme_dir = "priv/themes/"

/// Load grammars for all languages into the registry
fn load_grammars(
  reg: Registry,
  langs: List(Language),
) -> Result(Registry, String) {
  list.try_fold(langs, reg, fn(r, lang) {
    let path = grammar_dir <> languages.language_grammar_file(lang)
    case simplifile.read(path) {
      Error(_) -> Error("Failed to read grammar file: " <> path)
      Ok(json) -> {
        let name = language_id(lang)
        registry.add_grammar_json(r, name, json)
      }
    }
  })
}

/// Load themes into the registry
fn load_themes(
  reg: Registry,
  theme_list: List(Theme),
) -> Result(Registry, String) {
  list.try_fold(theme_list, reg, fn(r, theme) {
    case theme {
      Bundled(t) -> {
        let name = theme_id(t)
        let path = theme_dir <> name <> ".json"
        case simplifile.read(path) {
          Error(_) -> Error("Failed to read theme file: " <> path)
          Ok(json) -> registry.add_theme_json(r, name, json)
        }
      }
      themes.Custom(name, json) -> registry.add_theme_json(r, name, json)
    }
  })
}

// ============================================================================
// TOKENS OPTIONS
// ============================================================================

/// Explanation mode for tokenization
pub type ExplanationMode {
  /// No explanation included
  NoExplanation
  /// Include scope names only (more performant)
  ScopeNameOnly
  /// Include full explanation with theme matches
  FullExplanation
}

/// Options for code_to_tokens
pub type TokensOptions {
  TokensOptions(
    /// The language to use for tokenization
    lang: Language,
    /// The theme to use for tokenization
    theme: Theme,
    /// Include explanation of why a token is given a color
    include_explanation: ExplanationMode,
    /// Color replacements map (lowercase hex color -> replacement color)
    color_replacements: Dict(String, String),
    /// Lines above this length will not be tokenized for performance (0 = no limit)
    tokenize_max_line_length: Int,
    /// Time limit in milliseconds for tokenizing a single line
    tokenize_time_limit: Int,
  )
}

/// Create tokens options with default values.
///
/// ## Examples
///
/// ```gleam
/// let options = tokens_options(Javascript, Custom("my-theme", json))
/// ```
pub fn tokens_options(lang: Language, theme: Theme) -> TokensOptions {
  TokensOptions(
    lang: lang,
    theme: theme,
    include_explanation: NoExplanation,
    color_replacements: dict.new(),
    tokenize_max_line_length: 0,
    tokenize_time_limit: 500,
  )
}

/// Create tokens options with a bundled theme.
///
/// ## Examples
///
/// ```gleam
/// let options = tokens_options_bundled(Javascript, Nord)
/// ```
pub fn tokens_options_bundled(
  lang: Language,
  theme: BundledTheme,
) -> TokensOptions {
  tokens_options(lang, Bundled(theme))
}

/// Set the explanation mode for tokens options.
pub fn tokens_with_explanation(
  opts: TokensOptions,
  mode: ExplanationMode,
) -> TokensOptions {
  TokensOptions(..opts, include_explanation: mode)
}

/// Set color replacements for tokens options.
pub fn tokens_with_color_replacements(
  opts: TokensOptions,
  replacements: Dict(String, String),
) -> TokensOptions {
  TokensOptions(..opts, color_replacements: replacements)
}

/// Set max line length for tokens options.
pub fn tokens_with_max_line_length(
  opts: TokensOptions,
  length: Int,
) -> TokensOptions {
  TokensOptions(..opts, tokenize_max_line_length: length)
}

/// Set time limit for tokens options.
pub fn tokens_with_time_limit(opts: TokensOptions, ms: Int) -> TokensOptions {
  TokensOptions(..opts, tokenize_time_limit: ms)
}

// ============================================================================
// HTML OPTIONS
// ============================================================================

/// HTML output structure type
pub type HtmlStructure {
  /// Classic structure with pre/code tags
  StructureClassic
  /// Inline structure without wrapper tags
  StructureInline
}

/// Whitespace merging mode
pub type MergeWhitespaces {
  /// Merge whitespaces (default)
  MergeWhitespacesTrue
  /// Don't merge whitespaces
  MergeWhitespacesFalse
  /// Never merge whitespaces
  MergeWhitespacesNever
}

/// Tab index setting
pub type TabIndex {
  /// Set a specific tab index value
  TabIndexValue(Int)
  /// Disable tab index
  TabIndexDisabled
}

/// Options for code_to_html
pub type HtmlOptions {
  HtmlOptions(
    lang: Language,
    theme: Theme,
    include_explanation: ExplanationMode,
    color_replacements: Dict(String, String),
    tokenize_max_line_length: Int,
    tokenize_time_limit: Int,
    structure: HtmlStructure,
    merge_whitespaces: MergeWhitespaces,
    merge_same_style_tokens: Bool,
    tabindex: TabIndex,
    root_style: Result(String, Nil),
    data: Dict(String, String),
    meta: Dict(String, String),
  )
}

/// Create HTML options with default values.
pub fn html_options(lang: Language, theme: Theme) -> HtmlOptions {
  HtmlOptions(
    lang: lang,
    theme: theme,
    include_explanation: NoExplanation,
    color_replacements: dict.new(),
    tokenize_max_line_length: 0,
    tokenize_time_limit: 500,
    structure: StructureClassic,
    merge_whitespaces: MergeWhitespacesTrue,
    merge_same_style_tokens: False,
    tabindex: TabIndexValue(0),
    root_style: Error(Nil),
    data: dict.new(),
    meta: dict.new(),
  )
}

/// Create HTML options with a bundled theme.
pub fn html_options_bundled(lang: Language, theme: BundledTheme) -> HtmlOptions {
  html_options(lang, Bundled(theme))
}

/// Set HTML structure for HTML options.
pub fn html_with_structure(opts: HtmlOptions, s: HtmlStructure) -> HtmlOptions {
  HtmlOptions(..opts, structure: s)
}

/// Set whitespace merging for HTML options.
pub fn html_with_merge_whitespaces(
  opts: HtmlOptions,
  m: MergeWhitespaces,
) -> HtmlOptions {
  HtmlOptions(..opts, merge_whitespaces: m)
}

/// Set whether to merge same-style tokens for HTML options.
pub fn html_with_merge_same_style_tokens(
  opts: HtmlOptions,
  merge: Bool,
) -> HtmlOptions {
  HtmlOptions(..opts, merge_same_style_tokens: merge)
}

/// Set tab index for HTML options.
pub fn html_with_tabindex(opts: HtmlOptions, ti: TabIndex) -> HtmlOptions {
  HtmlOptions(..opts, tabindex: ti)
}

/// Set root style for HTML options.
pub fn html_with_root_style(opts: HtmlOptions, style: String) -> HtmlOptions {
  HtmlOptions(..opts, root_style: Ok(style))
}

/// Set data attributes for HTML options.
pub fn html_with_data(
  opts: HtmlOptions,
  data: Dict(String, String),
) -> HtmlOptions {
  HtmlOptions(..opts, data: data)
}

/// Set meta attributes for HTML options.
pub fn html_with_meta(
  opts: HtmlOptions,
  meta: Dict(String, String),
) -> HtmlOptions {
  HtmlOptions(..opts, meta: meta)
}

/// Set explanation mode for HTML options.
pub fn html_with_explanation(
  opts: HtmlOptions,
  mode: ExplanationMode,
) -> HtmlOptions {
  HtmlOptions(..opts, include_explanation: mode)
}

/// Set color replacements for HTML options.
pub fn html_with_color_replacements(
  opts: HtmlOptions,
  replacements: Dict(String, String),
) -> HtmlOptions {
  HtmlOptions(..opts, color_replacements: replacements)
}

/// Set max line length for HTML options.
pub fn html_with_max_line_length(opts: HtmlOptions, length: Int) -> HtmlOptions {
  HtmlOptions(..opts, tokenize_max_line_length: length)
}

/// Set time limit for HTML options.
pub fn html_with_time_limit(opts: HtmlOptions, ms: Int) -> HtmlOptions {
  HtmlOptions(..opts, tokenize_time_limit: ms)
}

// ============================================================================
// MAIN API
// ============================================================================

/// Convert code to themed tokens.
///
/// This is the main entry point for tokenization.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(highlighter) =
///   new_highlighter()
///   |> with_language(Javascript)
///   |> with_bundled_theme(Nord)
///   |> build()
///
/// let options = tokens_options_bundled(Javascript, Nord)
/// let assert Ok(#(highlighter, result)) =
///   code_to_tokens(highlighter, "const x = 42;", options)
/// // result.tokens contains the 2D array of tokens
/// // result.fg contains the theme foreground color
/// // result.bg contains the theme background color
/// ```
pub fn code_to_tokens(
  highlighter: Highlighter,
  code: String,
  options: TokensOptions,
) -> Result(#(Highlighter, TokensResult), String) {
  let lang_str = language_id(options.lang)
  let resolved_lang = case dict.get(highlighter.lang_aliases, lang_str) {
    Ok(alias) -> alias
    Error(_) -> lang_str
  }
  let theme_str = theme_name(options.theme)

  // Handle plaintext or "none" theme - return unstyled tokens
  let is_plain = resolved_lang == "" || resolved_lang == "plaintext"
  case is_plain || theme_str == "none" {
    True -> {
      let tokens =
        split_lines(code)
        |> list.map(fn(line: Line) {
          let #(content, offset) = line
          [token.simple_token(content, offset)]
        })
      Ok(#(
        highlighter,
        TokensResult(tokens: tokens, fg: "", bg: "", theme_name: ""),
      ))
    }
    False -> {
      let highlight_options = to_highlighter_options(options, resolved_lang)
      case
        internal_highlighter.highlight(
          highlighter.registry,
          code,
          highlight_options,
        )
      {
        Ok(#(new_reg, result)) -> {
          let new_highlighter = Highlighter(..highlighter, registry: new_reg)
          Ok(#(
            new_highlighter,
            TokensResult(
              tokens: result.tokens,
              fg: result.fg,
              bg: result.bg,
              theme_name: result.theme_name,
            ),
          ))
        }
        Error(e) -> Error(e)
      }
    }
  }
}

/// Convert code to HTML.
///
/// STUB: This function is not yet implemented.
pub fn code_to_html(
  _highlighter: Highlighter,
  _code: String,
  _options: HtmlOptions,
) -> Result(#(Highlighter, String), String) {
  Error("code_to_html is not yet implemented")
}

// ============================================================================
// HIGHLIGHTER ACCESSORS
// ============================================================================

/// Check if a language is loaded in the highlighter
pub fn has_language(highlighter: Highlighter, lang: Language) -> Bool {
  list.contains(highlighter.loaded_languages, lang)
}

/// Check if a theme is loaded in the highlighter
pub fn has_theme(highlighter: Highlighter, theme: Theme) -> Bool {
  let name = theme_name(theme)
  list.any(highlighter.loaded_themes, fn(t) { theme_name(t) == name })
}

/// Get list of loaded language names
pub fn get_loaded_language_names(highlighter: Highlighter) -> List(String) {
  list.map(highlighter.loaded_languages, language_id)
}

/// Get list of loaded theme names
pub fn get_loaded_theme_names(highlighter: Highlighter) -> List(String) {
  list.map(highlighter.loaded_themes, theme_name)
}

/// Get the number of loaded languages
pub fn loaded_language_count(highlighter: Highlighter) -> Int {
  list.length(highlighter.loaded_languages)
}

/// Get the number of loaded themes
pub fn loaded_theme_count(highlighter: Highlighter) -> Int {
  list.length(highlighter.loaded_themes)
}

// ============================================================================
// INTERNAL HELPERS
// ============================================================================

/// Convert TokensOptions to internal highlighter options
fn to_highlighter_options(
  options: TokensOptions,
  resolved_lang: String,
) -> internal_highlighter.HighlightOptions {
  let include_explanation = case options.include_explanation {
    NoExplanation -> internal_highlighter.NoExplanation
    ScopeNameOnly -> internal_highlighter.ScopeNameOnly
    FullExplanation -> internal_highlighter.FullExplanation
  }

  internal_highlighter.HighlightOptions(
    lang: resolved_lang,
    theme: theme_name(options.theme),
    color_replacements: options.color_replacements,
    include_explanation: include_explanation,
    tokenize_max_line_length: options.tokenize_max_line_length,
  )
}
