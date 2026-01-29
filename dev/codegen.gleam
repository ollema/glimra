//// Codegen script - Unified code generation for glimra
////
//// This script handles:
//// 1. Vendoring grammar/theme JSON files from textmate-grammars-themes
//// 2. Generating expected token files (via JS Shiki)
//// 3. Generating test files for each language
////
//// ## Usage
////
////   gleam run -m codegen <textmate-grammars-themes-path>
////
//// ## Prerequisites
////
//// Clone the textmate-grammars-themes repository:
////   git clone https://github.com/shikijs/textmate-grammars-themes
////
//// For expected token generation, you need Node.js and dependencies installed:
////   pnpm install

import argv
import gleam/io
import gleam/json
import gleam/list
import gleam/string
import glimra/languages.{type Language}
import glimra/themes.{type BundledTheme}
import simplifile

@external(javascript, "./codegen_ffi.mjs", "run_generate_expected_tokens")
fn run_generate_expected_tokens_ffi(
  config_json: String,
) -> Result(String, String)

@external(javascript, "./codegen_ffi.mjs", "run_extract_patterns")
fn run_extract_patterns_ffi(config_json: String) -> Result(String, String)

@external(javascript, "./codegen_ffi.mjs", "run_generate_expected_ast")
fn run_generate_expected_ast_ffi(config_json: String) -> Result(String, String)

@external(javascript, "./codegen_ffi.mjs", "run_generate_expected_regex_plus_ast")
fn run_generate_expected_regex_plus_ast_ffi(
  config_json: String,
) -> Result(String, String)

@external(javascript, "./codegen_ffi.mjs", "run_generate_expected_generated")
fn run_generate_expected_generated_ffi(
  config_json: String,
) -> Result(String, String)

@external(javascript, "./codegen_ffi.mjs", "run_spy_toregexp_calls")
pub fn run_spy_toregexp_calls_ffi(config_json: String) -> Result(String, String)

@external(javascript, "./codegen_ffi.mjs", "run_generate_expected_recursion")
fn run_generate_expected_recursion_ffi(
  config_json: String,
) -> Result(String, String)

pub fn main() {
  case argv.load().arguments {
    [source_path] -> run_all(source_path)
    _ -> {
      io.println("Usage: gleam run -m codegen <textmate-grammars-themes-path>")
      io.println("")
      io.println(
        "Runs all codegen tasks: vendor, generate-expected-tokens, generate-tests",
      )
      io.println("")
      io.println("Prerequisites:")
      io.println(
        "  - Clone textmate-grammars-themes: git clone https://github.com/shikijs/textmate-grammars-themes",
      )
      io.println("  - Install npm dependencies: npm install")
    }
  }
}

// ============================================================================
// Vendor command
// ============================================================================

fn run_vendor(source_path: String) {
  io.println("Vendoring grammars and themes...")
  io.println("")

  let grammar_src = source_path <> "/packages/tm-grammars/grammars/"
  let theme_src = source_path <> "/packages/tm-themes/themes/"

  // Create directories
  let _ = simplifile.create_directory_all("priv/grammars")
  let _ = simplifile.create_directory_all("priv/themes")

  // Copy grammars - use the single source of truth
  list.each(languages.all_languages(), fn(lang) {
    let filename = languages.language_grammar_file(lang)
    let src = grammar_src <> filename
    let dest = "priv/grammars/" <> filename
    case simplifile.copy_file(src, dest) {
      Ok(_) -> io.println("Copied " <> filename)
      Error(e) ->
        io.println("Error copying " <> filename <> ": " <> string.inspect(e))
    }
  })

  // Copy themes - use the single source of truth
  list.each(themes.all_themes(), fn(theme) {
    let filename = themes.theme_info(theme).theme_file
    let src = theme_src <> filename
    let dest = "priv/themes/" <> filename
    case simplifile.copy_file(src, dest) {
      Ok(_) -> io.println("Copied " <> filename)
      Error(e) ->
        io.println("Error copying " <> filename <> ": " <> string.inspect(e))
    }
  })

  io.println("")
  io.println("Done! Files vendored to priv/")
}

// ============================================================================
// Generate expected tokens command
// ============================================================================

pub fn build_config_json() -> String {
  let languages_json =
    languages.all_languages()
    |> list.map(fn(lang) {
      json.object([
        #("name", json.string(languages.language_id(lang))),
        #("file", json.string(languages.language_grammar_file(lang))),
      ])
    })
    |> json.preprocessed_array

  let themes_json =
    themes.all_themes()
    |> list.map(fn(theme) { json.string(themes.theme_id(theme)) })
    |> json.preprocessed_array

  json.object([#("languages", languages_json), #("themes", themes_json)])
  |> json.to_string
}

fn run_generate_expected_tokens() {
  io.println("Generating expected token files...")
  io.println("")

  let config_json = build_config_json()

  case run_generate_expected_tokens_ffi(config_json) {
    Ok(output) -> {
      io.println(output)
    }
    Error(error_msg) -> {
      io.println("Error running generate_expected_tokens.mjs:")
      io.println(error_msg)
      io.println("")
      io.println("Make sure you have Node.js installed and dependencies:")
      io.println("  pnpm install")
    }
  }
}

// ============================================================================
// Generate tests command
// ============================================================================

fn run_generate_tests() {
  io.println("Generating test files...")
  io.println("")

  list.each(languages.all_languages(), fn(lang) { generate_test_file(lang) })

  // Generate API tests
  generate_api_test_file()

  io.println("")
  io.println("Done! Test files generated in test/snippets/")
}

fn generate_test_file(lang: Language) {
  let lang_id = languages.language_id(lang)
  let dir = "test/snippets/" <> lang_id
  let path = dir <> "/" <> lang_id <> "_test.gleam"

  // Ensure directory exists
  let _ = simplifile.create_directory_all(dir)

  let content = generate_test_content(lang)

  case simplifile.write(path, content) {
    Ok(_) -> io.println("Generated " <> path)
    Error(e) ->
      io.println("Error writing " <> path <> ": " <> string.inspect(e))
  }
}

fn generate_test_content(lang: Language) -> String {
  let lang_id = languages.language_id(lang)
  let lang_ctor = languages.language_info(lang).constructor

  let token_test_cases =
    themes.all_themes()
    |> list.map(fn(theme) { generate_test_case(lang_ctor, theme) })
    |> string.join(",\n")

  "import glimra/languages
import startest.{describe, it}
import test_helpers
import glimra/themes

pub fn " <> lang_id <> "_tests() {
  describe(\"" <> lang_id <> " expected tokens snippet validation\", [
" <> token_test_cases <> ",
  ])
}

pub fn " <> lang_id <> "_ast_tests() {
  describe(\"" <> lang_id <> " expected ast validation\", [
    it(\"parses all patterns correctly\", fn() {
      test_helpers.validate_expected_ast(languages." <> lang_ctor <> ")
    }),
  ])
}

pub fn " <> lang_id <> "_regex_plus_ast_tests() {
  describe(\"" <> lang_id <> " expected regex_plus_ast validation\", [
    it(\"transforms all patterns correctly\", fn() {
      test_helpers.validate_expected_regex_plus_ast(languages." <> lang_ctor <> ")
    }),
  ])
}

pub fn " <> lang_id <> "_generated_tests() {
  describe(\"" <> lang_id <> " expected generated validation\", [
    it(\"generates all patterns correctly\", fn() {
      test_helpers.validate_expected_generated(languages." <> lang_ctor <> ")
    }),
  ])
}
"
}

fn generate_test_case(lang_ctor: String, theme: BundledTheme) -> String {
  let theme_id = themes.theme_id(theme)
  let theme_ctor = themes.theme_info(theme).constructor

  "    it(\"validates with " <> theme_id <> " theme\", fn() {
      test_helpers.validate_snippet(languages." <> lang_ctor <> ", \"snippet\", themes." <> theme_ctor <> ")
    })"
}

// ============================================================================
// Generate API test file
// ============================================================================

fn generate_api_test_file() {
  let path = "test/api_test.gleam"

  // Generate a list of all language constructors for with_all_languages test
  let all_lang_ctors =
    languages.all_languages()
    |> list.map(fn(lang) {
      "languages." <> languages.language_info(lang).constructor
    })
    |> string.join(", ")

  // Generate a list of all theme constructors for with_all_bundled_themes test
  let all_theme_ctors =
    themes.all_themes()
    |> list.map(fn(theme) { "themes." <> themes.theme_info(theme).constructor })
    |> string.join(", ")

  let content = "/// Generated API tests for glimra public API functions
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
  describe(\"builder API\", [
    // with_languages - load multiple languages at once
    it(\"with_languages loads multiple languages\", fn() {
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
    it(\"with_all_languages loads all languages\", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_all_languages()
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      // Should have loaded all languages
      let all_langs = [" <> all_lang_ctors <> "]
      list.each(all_langs, fn(lang) {
        glimra.has_language(highlighter, lang)
        |> expect.to_be_true()
      })
    }),

    // with_bundled_themes - load multiple themes at once
    it(\"with_bundled_themes loads multiple themes\", fn() {
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
    it(\"with_all_bundled_themes loads all themes\", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_all_bundled_themes()
        |> glimra.build()

      // Should have loaded all themes
      let all_themes = [" <> all_theme_ctors <> "]
      list.each(all_themes, fn(theme) {
        glimra.has_theme(highlighter, themes.Bundled(theme))
        |> expect.to_be_true()
      })
    }),

    // with_custom_theme - load a custom theme from JSON
    it(\"with_custom_theme loads custom theme\", fn() {
      // Read an existing theme JSON as our \"custom\" theme
      let assert Ok(theme_json) = simplifile.read(\"priv/themes/nord.json\")

      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_custom_theme(\"my-custom-theme\", theme_json)
        |> glimra.build()

      // Verify the custom theme is loaded
      glimra.has_theme(highlighter, themes.Custom(\"my-custom-theme\", theme_json))
      |> expect.to_be_true()
    }),

    // with_lang_alias - add language alias
    it(\"with_lang_alias allows using alias\", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_lang_alias(\"js\", languages.Javascript)
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
  describe(\"query API\", [
    // has_language - check if language is loaded
    it(\"has_language returns True for loaded languages\", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      glimra.has_language(highlighter, languages.Javascript)
      |> expect.to_be_true()
    }),

    it(\"has_language returns False for unloaded languages\", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      glimra.has_language(highlighter, languages.Python)
      |> expect.to_be_false()
    }),

    // has_theme - check if theme is loaded
    it(\"has_theme returns True for loaded themes\", fn() {
      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_bundled_theme(themes.Nord)
        |> glimra.build()

      glimra.has_theme(highlighter, themes.Bundled(themes.Nord))
      |> expect.to_be_true()
    }),

    it(\"has_theme returns False for unloaded themes\", fn() {
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
  describe(\"token options\", [
    // tokens_with_color_replacements - validates against JS Shiki output
    it(\"tokens_with_color_replacements matches JS Shiki output\", fn() {
      let code = test_helpers.read_snippet(languages.Javascript, \"snippet\")

      let assert Ok(expected) =
        test_helpers.read_expected_tokens_options(
          languages.Javascript,
          \"snippet\",
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
        Error(msg) -> msg |> expect.to_equal(\"\")
      }
    }),

    // tokens_with_max_line_length - validates against JS Shiki output
    it(\"tokens_with_max_line_length matches JS Shiki output\", fn() {
      let code = test_helpers.read_snippet(languages.Javascript, \"snippet\")

      let assert Ok(expected) =
        test_helpers.read_expected_tokens_max_line_length(
          languages.Javascript,
          \"snippet\",
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
        Error(msg) -> msg |> expect.to_equal(\"\")
      }
    }),

    // tokens_with_time_limit - verify option can be set and tokenization works
    it(\"tokens_with_time_limit allows tokenization\", fn() {
      let code = test_helpers.read_snippet(languages.Javascript, \"snippet\")

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
    it(\"tokens_options works with custom theme\", fn() {
      let code = test_helpers.read_snippet(languages.Javascript, \"snippet\")

      // Read nord.json and use it as a custom theme
      let assert Ok(theme_json) = simplifile.read(\"priv/themes/nord.json\")

      let assert Ok(highlighter) =
        glimra.new_highlighter()
        |> glimra.with_language(languages.Javascript)
        |> glimra.with_custom_theme(\"my-custom-nord\", theme_json)
        |> glimra.build()

      // Use tokens_options with the Custom theme variant
      let options =
        glimra.tokens_options(
          languages.Javascript,
          themes.Custom(\"my-custom-nord\", theme_json),
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
      |> expect.to_equal(\"#616E88\")
    }),

    // tokens_with_explanation - test ScopeNameOnly mode
    it(\"tokens_with_explanation ScopeNameOnly includes scope names\", fn() {
      let code = \"const x = 42;\"

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
      |> expect.to_equal(\"source.js\")
    }),

    // tokens_with_explanation - test FullExplanation mode
    it(\"tokens_with_explanation FullExplanation includes theme matches\", fn() {
      let code = \"const x = 42;\"

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
      // Find the \"const\" token which should have theme matches
      let const_token =
        list.find(first_line, fn(t) { t.content == \"const\" })
        |> expect.to_be_ok()

      // Should have non-empty explanation
      list.length(const_token.explanation)
      |> expect.to_not_equal(0)

      // Check that explanation has content
      let assert [explanation, ..] = const_token.explanation
      explanation.content
      |> expect.to_equal(\"const\")
    }),
  ])
}
"

  case simplifile.write(path, content) {
    Ok(_) -> io.println("Generated " <> path)
    Error(e) ->
      io.println("Error writing " <> path <> ": " <> string.inspect(e))
  }
}

// ============================================================================
// Extract patterns command (for parser testing)
// ============================================================================

fn run_extract_patterns() {
  io.println("Extracting patterns from grammars...")
  io.println("")

  let config_json = build_config_json()

  case run_extract_patterns_ffi(config_json) {
    Ok(output) -> {
      io.println(output)
    }
    Error(error_msg) -> {
      io.println("Error running extract_patterns.mjs:")
      io.println(error_msg)
    }
  }
}

// ============================================================================
// Generate expected AST command (for parser testing)
// ============================================================================

fn run_generate_expected_ast() {
  io.println("Generating expected AST for patterns...")
  io.println("")

  let config_json = build_config_json()

  case run_generate_expected_ast_ffi(config_json) {
    Ok(output) -> {
      io.println(output)
    }
    Error(error_msg) -> {
      io.println("Error running generate_expected_ast.mjs:")
      io.println(error_msg)
      io.println("")
      io.println("Make sure the oniguruma-parser JS reference is built:")
      io.println("  cd js_reference/oniguruma-parser && npm run build")
    }
  }
}

// ============================================================================
// Generate expected RegexPlusAst command (for transform testing)
// ============================================================================

fn run_generate_expected_regex_plus_ast() {
  io.println("Generating expected RegexPlusAst for patterns...")
  io.println("")

  let config_json = build_config_json()

  case run_generate_expected_regex_plus_ast_ffi(config_json) {
    Ok(output) -> {
      io.println(output)
    }
    Error(error_msg) -> {
      io.println("Error running generate_expected_regex_plus_ast.mjs:")
      io.println(error_msg)
      io.println("")
      io.println("Make sure the oniguruma-to-es JS reference is available:")
      io.println("  js_reference/oniguruma-to-es/src/transform.js")
    }
  }
}

// ============================================================================
// Generate expected Generated command (for generate testing)
// ============================================================================

fn run_generate_expected_generated() {
  io.println("Generating expected Generated output for patterns...")
  io.println("")

  let config_json = build_config_json()

  case run_generate_expected_generated_ffi(config_json) {
    Ok(output) -> {
      io.println(output)
    }
    Error(error_msg) -> {
      io.println("Error running generate_expected_generated.mjs:")
      io.println(error_msg)
      io.println("")
      io.println("Make sure the oniguruma-to-es JS reference is available:")
      io.println("  js_reference/oniguruma-to-es/src/generate.js")
    }
  }
}

// ============================================================================
// Generate expected Recursion command (for recursion testing)
// ============================================================================

fn run_generate_expected_recursion() {
  io.println("Generating expected recursion output for patterns...")
  io.println("")

  let config_json = build_config_json()

  case run_generate_expected_recursion_ffi(config_json) {
    Ok(output) -> {
      io.println(output)
    }
    Error(error_msg) -> {
      io.println("Error running generate_expected_recursion.mjs:")
      io.println(error_msg)
      io.println("")
      io.println("Make sure the regex-recursion package is installed:")
      io.println("  pnpm install")
    }
  }
}

// ============================================================================
// All command
// ============================================================================

fn run_all(source_path: String) {
  io.println("Running all codegen tasks...")
  io.println("========================================")
  io.println("")

  run_vendor(source_path)

  io.println("")
  io.println("========================================")
  io.println("")

  run_generate_expected_tokens()

  io.println("")
  io.println("========================================")
  io.println("")

  run_extract_patterns()

  io.println("")
  io.println("========================================")
  io.println("")

  run_generate_expected_ast()

  io.println("")
  io.println("========================================")
  io.println("")

  run_generate_expected_regex_plus_ast()

  io.println("")
  io.println("========================================")
  io.println("")

  run_generate_expected_generated()

  io.println("")
  io.println("========================================")
  io.println("")

  run_generate_expected_recursion()

  io.println("")
  io.println("========================================")
  io.println("")

  run_generate_tests()

  io.println("")
  io.println("========================================")
  io.println("All codegen tasks complete!")
}
