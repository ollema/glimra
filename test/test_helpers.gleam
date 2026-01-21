/// Shared test utilities for all test files
///
/// This module provides common functionality used across tests:
/// - Token formatting for snapshots
/// - Token comparison for validation
/// - File reading utilities
/// - Expected token loading from JSON reference files
/// - AST validation for parser testing
import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/string
import glimra
import glimra/languages.{type Language, language_id}
import glimra/oniguruma_parser/json as ast_json
import glimra/oniguruma_parser/parser
import glimra/themes.{type BundledTheme, theme_id}
import glimra/types/token.{type ThemedToken, ThemedToken}
import simplifile
import startest/expect

// ============================================
// File Utilities
// ============================================

/// Read a snippet file from test/snippets/<lang>/
pub fn read_snippet(lang: Language, name: String) -> String {
  let lang_id = language_id(lang)
  let path = "test/snippets/" <> lang_id <> "/" <> name <> ".txt"
  let assert Ok(content) = simplifile.read(path)
  content
}

/// Result type for expected token files
pub type ExpectedTokens {
  ExpectedTokens(fg: String, bg: String, tokens: List(List(ThemedToken)))
}

/// Result type for expected tokens with color replacements
pub type ExpectedTokensOptions {
  ExpectedTokensOptions(
    fg: String,
    bg: String,
    replacement_map: dict.Dict(String, String),
    tokens: List(List(ThemedToken)),
  )
}

/// Result type for expected tokens with max line length
pub type ExpectedTokensMaxLineLength {
  ExpectedTokensMaxLineLength(
    fg: String,
    bg: String,
    max_line_length: Int,
    tokens: List(List(ThemedToken)),
  )
}

/// Read expected tokens from the consolidated JSON reference file
pub fn read_expected_tokens(
  lang: Language,
  _name: String,
  theme: BundledTheme,
) -> Result(ExpectedTokens, String) {
  let lang_id = language_id(lang)
  let theme_id = theme_id(theme)
  let path = "test/snippets/" <> lang_id <> "/expected_tokens.json"

  case simplifile.read(path) {
    Error(_) -> Error("Failed to read expected tokens file: " <> path)
    Ok(content) -> decode_expected_tokens(content, theme_id)
  }
}

fn decode_expected_tokens(
  content: String,
  theme_id: String,
) -> Result(ExpectedTokens, String) {
  let token_decoder =
    decode.field("content", decode.string, fn(c) {
      decode.field("offset", decode.int, fn(o) {
        decode.field("color", decode.string, fn(col) {
          decode.field("fontStyle", decode.int, fn(fs) {
            decode.success(
              ThemedToken(
                content: c,
                offset: o,
                color: col,
                font_style: fs,
                explanation: [],
              ),
            )
          })
        })
      })
    })

  let theme_data_decoder =
    decode.field("tokens", decode.list(decode.list(token_decoder)), fn(tokens) {
      decode.field("fg", decode.string, fn(fg) {
        decode.field("bg", decode.string, fn(bg) {
          decode.success(ExpectedTokens(fg: fg, bg: bg, tokens: tokens))
        })
      })
    })

  let all_themes_decoder = decode.dict(decode.string, theme_data_decoder)

  json.parse(content, all_themes_decoder)
  |> result.map_error(fn(_) { "Failed to decode expected tokens JSON" })
  |> result.try(fn(themes_dict) {
    case dict.get(themes_dict, theme_id) {
      Ok(tokens) -> Ok(tokens)
      Error(_) -> Error("Theme not found in expected tokens: " <> theme_id)
    }
  })
}

/// Read expected tokens with options (e.g., color replacements) from JSON reference file
pub fn read_expected_tokens_options(
  lang: Language,
  _name: String,
  theme: BundledTheme,
) -> Result(ExpectedTokensOptions, String) {
  let lang_id = language_id(lang)
  let theme_id = theme_id(theme)
  let path = "test/snippets/" <> lang_id <> "/expected_tokens_options.json"

  case simplifile.read(path) {
    Error(_) -> Error("Failed to read expected tokens options file: " <> path)
    Ok(content) -> decode_expected_tokens_options(content, theme_id)
  }
}

fn decode_expected_tokens_options(
  content: String,
  theme_id: String,
) -> Result(ExpectedTokensOptions, String) {
  let token_decoder =
    decode.field("content", decode.string, fn(c) {
      decode.field("offset", decode.int, fn(o) {
        decode.field("color", decode.string, fn(col) {
          decode.field("fontStyle", decode.int, fn(fs) {
            decode.success(
              ThemedToken(
                content: c,
                offset: o,
                color: col,
                font_style: fs,
                explanation: [],
              ),
            )
          })
        })
      })
    })

  // Decoder for the color_replacements variant
  let color_replacements_decoder =
    decode.field("tokens", decode.list(decode.list(token_decoder)), fn(tokens) {
      decode.field("fg", decode.string, fn(fg) {
        decode.field("bg", decode.string, fn(bg) {
          decode.field(
            "replacement_map",
            decode.dict(decode.string, decode.string),
            fn(replacement_map) {
              decode.success(ExpectedTokensOptions(
                fg: fg,
                bg: bg,
                replacement_map: replacement_map,
                tokens: tokens,
              ))
            },
          )
        })
      })
    })

  // The structure is: { theme_id: { "color_replacements": {...} } }
  let theme_variants_decoder =
    decode.field("color_replacements", color_replacements_decoder, fn(cr) {
      decode.success(cr)
    })

  let all_themes_decoder = decode.dict(decode.string, theme_variants_decoder)

  json.parse(content, all_themes_decoder)
  |> result.map_error(fn(_) { "Failed to decode expected tokens options JSON" })
  |> result.try(fn(themes_dict) {
    case dict.get(themes_dict, theme_id) {
      Ok(options) -> Ok(options)
      Error(_) ->
        Error("Theme not found in expected tokens options: " <> theme_id)
    }
  })
}

/// Read expected tokens with max line length from JSON reference file
pub fn read_expected_tokens_max_line_length(
  lang: Language,
  _name: String,
  theme: BundledTheme,
) -> Result(ExpectedTokensMaxLineLength, String) {
  let lang_id = language_id(lang)
  let theme_id = theme_id(theme)
  let path = "test/snippets/" <> lang_id <> "/expected_tokens_options.json"

  case simplifile.read(path) {
    Error(_) -> Error("Failed to read expected tokens options file: " <> path)
    Ok(content) -> decode_expected_tokens_max_line_length(content, theme_id)
  }
}

fn decode_expected_tokens_max_line_length(
  content: String,
  theme_id: String,
) -> Result(ExpectedTokensMaxLineLength, String) {
  let token_decoder =
    decode.field("content", decode.string, fn(c) {
      decode.field("offset", decode.int, fn(o) {
        decode.field("color", decode.string, fn(col) {
          decode.field("fontStyle", decode.int, fn(fs) {
            decode.success(
              ThemedToken(
                content: c,
                offset: o,
                color: col,
                font_style: fs,
                explanation: [],
              ),
            )
          })
        })
      })
    })

  // Decoder for the max_line_length variant
  let max_line_length_decoder =
    decode.field("tokens", decode.list(decode.list(token_decoder)), fn(tokens) {
      decode.field("fg", decode.string, fn(fg) {
        decode.field("bg", decode.string, fn(bg) {
          decode.field("max_line_length", decode.int, fn(max_len) {
            decode.success(ExpectedTokensMaxLineLength(
              fg: fg,
              bg: bg,
              max_line_length: max_len,
              tokens: tokens,
            ))
          })
        })
      })
    })

  // The structure is: { theme_id: { "max_line_length": {...} } }
  let theme_variants_decoder =
    decode.field("max_line_length", max_line_length_decoder, fn(mll) {
      decode.success(mll)
    })

  let all_themes_decoder = decode.dict(decode.string, theme_variants_decoder)

  json.parse(content, all_themes_decoder)
  |> result.map_error(fn(_) {
    "Failed to decode expected tokens max line length JSON"
  })
  |> result.try(fn(themes_dict) {
    case dict.get(themes_dict, theme_id) {
      Ok(options) -> Ok(options)
      Error(_) ->
        Error(
          "Theme not found in expected tokens max line length: " <> theme_id,
        )
    }
  })
}

// ============================================
// Token Comparison (for validation)
// ============================================

/// Compare two token lists, return Ok(Nil) if equal, Error(message) if different
pub fn compare_tokens(
  gleam: List(List(ThemedToken)),
  reference: List(List(ThemedToken)),
) -> Result(Nil, String) {
  let gleam_len = list.length(gleam)
  let ref_len = list.length(reference)

  case gleam_len == ref_len {
    False ->
      Error(
        "Line count mismatch: Gleam="
        <> int.to_string(gleam_len)
        <> ", Reference="
        <> int.to_string(ref_len),
      )
    True -> compare_lines(gleam, reference, 0)
  }
}

fn compare_lines(
  gleam: List(List(ThemedToken)),
  reference: List(List(ThemedToken)),
  line_idx: Int,
) -> Result(Nil, String) {
  case gleam, reference {
    [], [] -> Ok(Nil)
    [g_line, ..g_rest], [r_line, ..r_rest] -> {
      case compare_line_tokens(g_line, r_line, line_idx, 0) {
        Ok(Nil) -> compare_lines(g_rest, r_rest, line_idx + 1)
        Error(e) -> Error(e)
      }
    }
    _, _ -> Error("Unexpected line mismatch")
  }
}

fn compare_line_tokens(
  gleam: List(ThemedToken),
  reference: List(ThemedToken),
  line_idx: Int,
  token_idx: Int,
) -> Result(Nil, String) {
  let gleam_len = list.length(gleam)
  let ref_len = list.length(reference)

  case gleam_len == ref_len {
    False ->
      Error(
        "Line "
        <> int.to_string(line_idx)
        <> ": Token count mismatch: Gleam="
        <> int.to_string(gleam_len)
        <> ", Reference="
        <> int.to_string(ref_len),
      )
    True -> compare_tokens_recursive(gleam, reference, line_idx, token_idx)
  }
}

fn compare_tokens_recursive(
  gleam: List(ThemedToken),
  reference: List(ThemedToken),
  line_idx: Int,
  token_idx: Int,
) -> Result(Nil, String) {
  case gleam, reference {
    [], [] -> Ok(Nil)
    [g, ..g_rest], [r, ..r_rest] -> {
      case compare_single_token(g, r, line_idx, token_idx) {
        Ok(Nil) ->
          compare_tokens_recursive(g_rest, r_rest, line_idx, token_idx + 1)
        Error(e) -> Error(e)
      }
    }
    _, _ -> Error("Unexpected token mismatch")
  }
}

fn compare_single_token(
  g: ThemedToken,
  r: ThemedToken,
  line_idx: Int,
  token_idx: Int,
) -> Result(Nil, String) {
  let prefix =
    "Line " <> int.to_string(line_idx) <> ", Token " <> int.to_string(token_idx)

  case g.content == r.content {
    False ->
      Error(
        prefix
        <> ": Content mismatch: \""
        <> g.content
        <> "\" vs \""
        <> r.content
        <> "\"",
      )
    True ->
      case g.offset == r.offset {
        False ->
          Error(
            prefix
            <> ": Offset mismatch: "
            <> int.to_string(g.offset)
            <> " vs "
            <> int.to_string(r.offset),
          )
        True ->
          case g.color == r.color {
            False ->
              Error(
                prefix
                <> ": Color mismatch: \""
                <> g.color
                <> "\" vs \""
                <> r.color
                <> "\"",
              )
            True ->
              case g.font_style == r.font_style {
                False ->
                  Error(
                    prefix
                    <> ": FontStyle mismatch: "
                    <> int.to_string(g.font_style)
                    <> " vs "
                    <> int.to_string(r.font_style),
                  )
                True -> Ok(Nil)
              }
          }
      }
  }
}

// ============================================
// Snippet Validation
// ============================================

/// Validate a snippet against expected tokens from JSON reference.
/// Uses the public glimra API with Language and BundledTheme types.
pub fn validate_snippet(
  lang: Language,
  name: String,
  theme: BundledTheme,
) -> Nil {
  let code = read_snippet(lang, name)
  let expected =
    read_expected_tokens(lang, name, theme)
    |> expect.to_be_ok()

  // Use the public API: build highlighter with specific language and theme
  let assert Ok(highlighter) =
    glimra.new_highlighter()
    |> glimra.with_language(lang)
    |> glimra.with_bundled_theme(theme)
    |> glimra.build()

  let options = glimra.tokens_options_bundled(lang, theme)

  let assert Ok(#(_highlighter, result)) =
    glimra.code_to_tokens(highlighter, code, options)

  case compare_tokens(result.tokens, expected.tokens) {
    Ok(Nil) -> Nil
    Error(msg) -> msg |> expect.to_equal("")
  }
}

// ============================================
// AST Validation (for parser testing)
// ============================================

/// Type for expected AST pattern entry
pub type ExpectedAstPattern {
  ExpectedAstPattern(pattern: String, success: Bool, ast_json: String)
}

/// Type for expected AST file
pub type ExpectedAstFile {
  ExpectedAstFile(
    language: String,
    total: Int,
    successful: Int,
    failed: Int,
    patterns: List(ExpectedAstPattern),
  )
}

/// Read expected AST from JSON file
pub fn read_expected_ast(lang: Language) -> Result(ExpectedAstFile, String) {
  let lang_id = language_id(lang)
  let path = "test/snippets/" <> lang_id <> "/expected_ast.json"

  case simplifile.read(path) {
    Error(_) -> Error("Failed to read expected AST file: " <> path)
    Ok(content) -> decode_expected_ast(content)
  }
}

fn decode_expected_ast(content: String) -> Result(ExpectedAstFile, String) {
  // Decode just the metadata - we'll handle patterns separately
  let metadata_decoder =
    decode.field("language", decode.string, fn(language) {
      decode.field("total", decode.int, fn(total) {
        decode.field("successful", decode.int, fn(successful) {
          decode.field("failed", decode.int, fn(failed) {
            decode.success(#(language, total, successful, failed))
          })
        })
      })
    })

  // First decode metadata
  case json.parse(content, metadata_decoder) {
    Error(_) -> Error("Failed to decode expected AST JSON metadata")
    Ok(#(language, total, successful, failed)) -> {
      // Now extract patterns using a different approach
      // We'll decode the patterns array with a dynamic decoder
      let patterns_decoder =
        decode.field("patterns", decode.list(decode.dynamic), fn(patterns_dyn) {
          decode.success(patterns_dyn)
        })

      case json.parse(content, patterns_decoder) {
        Error(_) -> Error("Failed to decode patterns array")
        Ok(patterns_dyn) -> {
          let patterns =
            patterns_dyn
            |> list.filter_map(fn(p) { decode_pattern_entry(p) })
          Ok(ExpectedAstFile(
            language: language,
            total: total,
            successful: successful,
            failed: failed,
            patterns: patterns,
          ))
        }
      }
    }
  }
}

fn decode_pattern_entry(dyn: decode.Dynamic) -> Result(ExpectedAstPattern, Nil) {
  let pattern_decoder =
    decode.field("pattern", decode.string, fn(pattern) {
      decode.field("success", decode.bool, fn(success) {
        decode.success(#(pattern, success))
      })
    })

  case decode.run(dyn, pattern_decoder) {
    Error(_) -> Error(Nil)
    Ok(#(pattern, success)) -> {
      // Now get the ast field as raw JSON
      let ast_decoder =
        decode.field("ast", decode.dynamic, fn(ast_dyn) {
          decode.success(ast_dyn)
        })

      let ast_json = case decode.run(dyn, ast_decoder) {
        Ok(ast_dyn) -> json.to_string(dynamic_to_json(ast_dyn))
        Error(_) -> ""
      }

      Ok(ExpectedAstPattern(
        pattern: pattern,
        success: success,
        ast_json: ast_json,
      ))
    }
  }
}

/// Convert a dynamic value back to json.Json for serialization
fn dynamic_to_json(dyn: decode.Dynamic) -> json.Json {
  // Try to decode as various types and convert to JSON
  case decode.run(dyn, decode.string) {
    Ok(s) -> json.string(s)
    Error(_) ->
      case decode.run(dyn, decode.int) {
        Ok(i) -> json.int(i)
        Error(_) ->
          case decode.run(dyn, decode.float) {
            Ok(f) -> json.float(f)
            Error(_) ->
              case decode.run(dyn, decode.bool) {
                Ok(b) -> json.bool(b)
                Error(_) ->
                  case decode.run(dyn, decode.list(decode.dynamic)) {
                    Ok(lst) -> json.array(lst, dynamic_to_json)
                    Error(_) ->
                      case
                        decode.run(
                          dyn,
                          decode.dict(decode.string, decode.dynamic),
                        )
                      {
                        Ok(d) ->
                          d
                          |> dict.to_list
                          |> list.map(fn(pair) {
                            #(pair.0, dynamic_to_json(pair.1))
                          })
                          |> json.object
                        Error(_) ->
                          // null or unknown - treat as null
                          json.null()
                      }
                  }
              }
          }
      }
  }
}

/// Validate expected AST for a language by parsing all patterns and comparing
pub fn validate_expected_ast(lang: Language) -> Nil {
  let expected =
    read_expected_ast(lang)
    |> expect.to_be_ok()

  let errors =
    expected.patterns
    |> list.filter_map(fn(entry) {
      case entry.success {
        False ->
          // Skip patterns that failed in JS parser
          Error(Nil)
        True -> {
          // Parse with Gleam parser
          let parse_opts = parser.default_options()
          case parser.parse(entry.pattern, parse_opts) {
            Error(err) ->
              Ok("Pattern \"" <> entry.pattern <> "\" failed to parse: " <> err)
            Ok(ast) -> {
              // Convert to JSON and compare semantically
              let gleam_json = ast_json.ast_to_string(ast)
              case compare_json_strings(gleam_json, entry.ast_json) {
                True -> Error(Nil)
                False ->
                  Ok(
                    "Pattern \""
                    <> entry.pattern
                    <> "\" AST mismatch:\nExpected: "
                    <> string.slice(entry.ast_json, 0, 200)
                    <> "\nGot: "
                    <> string.slice(gleam_json, 0, 200),
                  )
              }
            }
          }
        }
      }
    })

  case errors {
    [] -> Nil
    first_errors -> {
      // Show first few errors
      let error_msg =
        first_errors
        |> list.take(5)
        |> string.join("\n\n")
      error_msg |> expect.to_equal("")
    }
  }
}

/// Compare two JSON strings semantically (ignoring key order)
fn compare_json_strings(a: String, b: String) -> Bool {
  // Parse both as dynamic and compare
  let a_decoder = decode.dynamic
  let b_decoder = decode.dynamic

  case json.parse(a, a_decoder), json.parse(b, b_decoder) {
    Ok(a_dyn), Ok(b_dyn) -> compare_dynamic(a_dyn, b_dyn)
    _, _ -> False
  }
}

/// Compare two dynamic values recursively
fn compare_dynamic(a: decode.Dynamic, b: decode.Dynamic) -> Bool {
  // Try string
  case decode.run(a, decode.string), decode.run(b, decode.string) {
    Ok(a_str), Ok(b_str) -> a_str == b_str
    Error(_), Error(_) -> compare_dynamic_non_string(a, b)
    _, _ -> False
  }
}

fn compare_dynamic_non_string(a: decode.Dynamic, b: decode.Dynamic) -> Bool {
  // Try int
  case decode.run(a, decode.int), decode.run(b, decode.int) {
    Ok(a_int), Ok(b_int) -> a_int == b_int
    Error(_), Error(_) -> compare_dynamic_non_int(a, b)
    _, _ -> False
  }
}

fn compare_dynamic_non_int(a: decode.Dynamic, b: decode.Dynamic) -> Bool {
  // Try float
  case decode.run(a, decode.float), decode.run(b, decode.float) {
    Ok(a_float), Ok(b_float) -> a_float == b_float
    Error(_), Error(_) -> compare_dynamic_non_float(a, b)
    _, _ -> False
  }
}

fn compare_dynamic_non_float(a: decode.Dynamic, b: decode.Dynamic) -> Bool {
  // Try bool
  case decode.run(a, decode.bool), decode.run(b, decode.bool) {
    Ok(a_bool), Ok(b_bool) -> a_bool == b_bool
    Error(_), Error(_) -> compare_dynamic_non_bool(a, b)
    _, _ -> False
  }
}

fn compare_dynamic_non_bool(a: decode.Dynamic, b: decode.Dynamic) -> Bool {
  // Try list
  case
    decode.run(a, decode.list(decode.dynamic)),
    decode.run(b, decode.list(decode.dynamic))
  {
    Ok(a_list), Ok(b_list) -> compare_dynamic_lists(a_list, b_list)
    Error(_), Error(_) -> compare_dynamic_non_list(a, b)
    _, _ -> False
  }
}

fn compare_dynamic_lists(
  a: List(decode.Dynamic),
  b: List(decode.Dynamic),
) -> Bool {
  case list.length(a) == list.length(b) {
    False -> False
    True ->
      list.zip(a, b)
      |> list.all(fn(pair) { compare_dynamic(pair.0, pair.1) })
  }
}

fn compare_dynamic_non_list(a: decode.Dynamic, b: decode.Dynamic) -> Bool {
  // Try object (dict)
  case
    decode.run(a, decode.dict(decode.string, decode.dynamic)),
    decode.run(b, decode.dict(decode.string, decode.dynamic))
  {
    Ok(a_dict), Ok(b_dict) -> compare_dynamic_dicts(a_dict, b_dict)
    Error(_), Error(_) -> {
      // Both might be null - check via optional decoder
      case
        decode.run(a, decode.optional(decode.string)),
        decode.run(b, decode.optional(decode.string))
      {
        Ok(None), Ok(None) -> True
        _, _ -> False
      }
    }
    _, _ -> False
  }
}

fn compare_dynamic_dicts(
  a: dict.Dict(String, decode.Dynamic),
  b: dict.Dict(String, decode.Dynamic),
) -> Bool {
  let a_keys = dict.keys(a) |> list.sort(string.compare)
  let b_keys = dict.keys(b) |> list.sort(string.compare)
  case a_keys == b_keys {
    False -> False
    True ->
      a_keys
      |> list.all(fn(key) {
        case dict.get(a, key), dict.get(b, key) {
          Ok(a_val), Ok(b_val) -> compare_dynamic(a_val, b_val)
          _, _ -> False
        }
      })
  }
}
