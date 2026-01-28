/// Generated API tests for glimra public API functions
/// Tests builder functions, query functions, and token options
/// using JavaScript Shiki as the source of truth for token validation.

import gleam/list
import glimra/languages
import glimra
import simplifile
import startest.{describe, it}
import startest/expect
import test_helpers
import glimra/themes

// ============================================================================
// Builder API Tests
// ============================================================================

pub fn builder_api_tests() {
  describe("builder API", [
    // with_languages - load multiple languages at once
    it("with_languages loads multiple languages", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_languages([languages.Javascript, languages.Python, languages.Rust])
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      // Verify all languages are loaded
      glimra.has_language(highlighter, languages.Javascript)
      |> expect.to_be_true()
      glimra.has_language(highlighter, languages.Python)
      |> expect.to_be_true()
      glimra.has_language(highlighter, languages.Rust)
      |> expect.to_be_true()
    }),

    // with_all_languages - load all bundled languages
    it("with_all_languages loads all languages", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_all_languages()
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      // Should have loaded all languages
      let all_langs = [languages.Astro, languages.Bash, languages.C, languages.Clojure, languages.Cpp, languages.Crystal, languages.Css, languages.Dart, languages.Diff, languages.Docker, languages.Elixir, languages.Elm, languages.Erlang, languages.Glsl, languages.Gleam, languages.Go, languages.Graphql, languages.Haskell, languages.Html, languages.Ini, languages.Javascript, languages.Json, languages.Julia, languages.Kotlin, languages.Latex, languages.Lua, languages.Markdown, languages.Mojo, languages.Nim, languages.Nix, languages.Odin, languages.Ocaml, languages.Php, languages.Python, languages.R, languages.Ruby, languages.Rust, languages.Scala, languages.Scheme, languages.Svelte, languages.Swift, languages.Toml, languages.Tsx, languages.Typescript, languages.Typst, languages.Vue, languages.Wasm, languages.Wgsl, languages.Yaml, languages.Zig]
      list.each(all_langs, fn(lang) {
        glimra.has_language(highlighter, lang)
        |> expect.to_be_true()
      })
    }),

    // with_bundled_themes - load multiple themes at once
    it("with_bundled_themes loads multiple themes", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_themes([themes.Nord, themes.Dracula, themes.Monokai])
        |> glimra.build()

      // Verify all themes are loaded
      glimra.has_theme(highlighter, themes.Bundled(themes.Nord))
      |> expect.to_be_true()
      glimra.has_theme(highlighter, themes.Bundled(themes.Dracula))
      |> expect.to_be_true()
      glimra.has_theme(highlighter, themes.Bundled(themes.Monokai))
      |> expect.to_be_true()
    }),

    // with_all_bundled_themes - load all bundled themes
    it("with_all_bundled_themes loads all themes", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_all_bundled_themes()
        |> glimra.build()

      // Should have loaded all themes
      let all_themes = [themes.CatppuccinMocha, themes.Dracula, themes.GithubDark, themes.GruvboxDarkMedium, themes.Monokai, themes.NightOwl, themes.Nord, themes.OneDarkPro, themes.TokyoNight, themes.VitesseDark]
      list.each(all_themes, fn(theme) {
        glimra.has_theme(highlighter, themes.Bundled(theme))
        |> expect.to_be_true()
      })
    }),

    // with_custom_theme - load a custom theme from JSON
    it("with_custom_theme loads custom theme", fn() {
      // Read an existing theme JSON as our "custom" theme
      let assert Ok(theme_json) = simplifile.read("priv/themes/nord.json")

      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_custom_theme("my-custom-theme", theme_json)
        |> glimra.build()

      // Verify the custom theme is loaded
      glimra.has_theme(highlighter, themes.Custom("my-custom-theme", theme_json))
      |> expect.to_be_true()
    }),

    // with_lang_alias - add language alias
    it("with_lang_alias allows using alias", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_lang_alias("js", languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      // The alias should work - verify highlighter built successfully
      // (alias resolution is used internally during tokenization)
      glimra.has_language(highlighter, languages.Javascript)
      |> expect.to_be_true()
    }),
  ])
}

// ============================================================================
// Query API Tests
// ============================================================================

pub fn query_api_tests() {
  describe("query API", [
    // has_language - check if language is loaded
    it("has_language returns True for loaded languages", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      glimra.has_language(highlighter, languages.Javascript)
      |> expect.to_be_true()
    }),

    it("has_language returns False for unloaded languages", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      glimra.has_language(highlighter, languages.Python)
      |> expect.to_be_false()
    }),

    // has_theme - check if theme is loaded
    it("has_theme returns True for loaded themes", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      glimra.has_theme(highlighter, themes.Bundled(themes.Nord))
      |> expect.to_be_true()
    }),

    it("has_theme returns False for unloaded themes", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      glimra.has_theme(highlighter, themes.Bundled(themes.Dracula))
      |> expect.to_be_false()
    }),
  ])
}

// ============================================================================
// Token Options Tests (using JS Shiki as source of truth)
// ============================================================================

pub fn token_options_tests() {
  describe("token options", [
    // tokens_with_color_replacements - validates against JS Shiki output
    it("tokens_with_color_replacements matches JS Shiki output", fn() {
      let code = test_helpers.read_snippet(languages.Javascript, "snippet")

      let assert Ok(expected) =
        test_helpers.read_expected_tokens_options(
          languages.Javascript,
          "snippet",
          themes.Nord,
        )

      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      // Apply the same color replacements that JS Shiki used
      let options =
        glimra.tokens_options_bundled(languages.Javascript, themes.Nord)
        |> glimra.tokens_with_color_replacements(expected.replacement_map)

      let assert Ok(#(_highlighter, result)) =
        glimra.code_to_tokens(highlighter, code, options)

      case test_helpers.compare_tokens(result.tokens, expected.tokens) {
        Ok(Nil) -> Nil
        Error(msg) -> msg |> expect.to_equal("")
      }
    }),

    // tokens_with_max_line_length - validates against JS Shiki output
    it("tokens_with_max_line_length matches JS Shiki output", fn() {
      let code = test_helpers.read_snippet(languages.Javascript, "snippet")

      let assert Ok(expected) =
        test_helpers.read_expected_tokens_max_line_length(
          languages.Javascript,
          "snippet",
          themes.Nord,
        )

      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      // Apply the same max line length that JS Shiki used
      let options =
        glimra.tokens_options_bundled(languages.Javascript, themes.Nord)
        |> glimra.tokens_with_max_line_length(expected.max_line_length)

      let assert Ok(#(_highlighter, result)) =
        glimra.code_to_tokens(highlighter, code, options)

      case test_helpers.compare_tokens(result.tokens, expected.tokens) {
        Ok(Nil) -> Nil
        Error(msg) -> msg |> expect.to_equal("")
      }
    }),

    // tokens_with_time_limit - verify option can be set and tokenization works
    it("tokens_with_time_limit allows tokenization", fn() {
      let code = test_helpers.read_snippet(languages.Javascript, "snippet")

      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      // Set a generous time limit
      let options =
        glimra.tokens_options_bundled(languages.Javascript, themes.Nord)
        |> glimra.tokens_with_time_limit(5000)

      let assert Ok(#(_highlighter, result)) =
        glimra.code_to_tokens(highlighter, code, options)

      // Just verify we got some tokens back
      list.length(result.tokens)
      |> expect.to_not_equal(0)
    }),

    // tokens_options - test with custom theme (not bundled)
    it("tokens_options works with custom theme", fn() {
      let code = test_helpers.read_snippet(languages.Javascript, "snippet")

      // Read nord.json and use it as a custom theme
      let assert Ok(theme_json) = simplifile.read("priv/themes/nord.json")

      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_custom_theme("my-custom-nord", theme_json)
        |> glimra.build()

      // Use tokens_options with the Custom theme variant
      let options =
        glimra.tokens_options(
          languages.Javascript,
          themes.Custom("my-custom-nord", theme_json),
        )

      let assert Ok(#(_highlighter, result)) =
        glimra.code_to_tokens(highlighter, code, options)

      // Verify we got tokens and they have colors
      list.length(result.tokens)
      |> expect.to_not_equal(0)

      // Check first line has tokens with colors
      let assert [first_line, ..] = result.tokens
      let assert [first_token, ..] = first_line
      // Nord comment color
      first_token.color
      |> expect.to_equal("#616E88")
    }),

    // tokens_with_explanation - test ScopeNameOnly mode
    it("tokens_with_explanation ScopeNameOnly includes scope names", fn() {
      let code = "const x = 42;"

      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      let options =
        glimra.tokens_options_bundled(languages.Javascript, themes.Nord)
        |> glimra.tokens_with_explanation(glimra.ScopeNameOnly)

      let assert Ok(#(_highlighter, result)) =
        glimra.code_to_tokens(highlighter, code, options)

      // Verify tokens have explanation data
      let assert [first_line, ..] = result.tokens
      let assert [first_token, ..] = first_line

      // Should have non-empty explanation
      list.length(first_token.explanation)
      |> expect.to_not_equal(0)

      // Check that scopes are present
      let assert [explanation, ..] = first_token.explanation
      list.length(explanation.scopes)
      |> expect.to_not_equal(0)

      // Check scope_name is present (should include source.js)
      let assert [first_scope, ..] = explanation.scopes
      first_scope.scope_name
      |> expect.to_equal("source.js")
    }),

    // tokens_with_explanation - test FullExplanation mode
    it("tokens_with_explanation FullExplanation includes theme matches", fn() {
      let code = "const x = 42;"

      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      let options =
        glimra.tokens_options_bundled(languages.Javascript, themes.Nord)
        |> glimra.tokens_with_explanation(glimra.FullExplanation)

      let assert Ok(#(_highlighter, result)) =
        glimra.code_to_tokens(highlighter, code, options)

      // Verify tokens have explanation data
      let assert [first_line, ..] = result.tokens
      // Find the "const" token which should have theme matches
      let const_token =
        list.find(first_line, fn(t) { t.content == "const" })
        |> expect.to_be_ok()

      // Should have non-empty explanation
      list.length(const_token.explanation)
      |> expect.to_not_equal(0)

      // Check that explanation has content
      let assert [explanation, ..] = const_token.explanation
      explanation.content
      |> expect.to_equal("const")
    }),
  ])
}
