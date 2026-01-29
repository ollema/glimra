//// Shared test utilities for all test files
////
//// This module provides common functionality used across tests:
//// - Token formatting for snapshots
//// - Token comparison for validation
//// - File reading utilities
//// - Expected token loading from JSON reference files
//// - AST validation for parser testing

import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import glimra
import glimra/languages.{type Language, language_id}
import glimra/oniguruma_parser/parser
import glimra/oniguruma_parser/parser/ast_types.{
  type AbsenceFunctionKind, type AbsenceFunctionNode, type AlternativeElement,
  type AlternativeNode, type AssertionKind, type AssertionNode,
  type BackreferenceNode, type BackreferenceRef, type CalloutArg,
  type CapturingGroupNode, type CharacterClassElement, type CharacterClassKind,
  type CharacterClassNode, type CharacterClassRangeNode, type CharacterNode,
  type CharacterSetKind, type CharacterSetNode, type DirectiveKind,
  type DirectiveNode, type FlagGroupModifiers, type FlagGroupSwitches,
  type FlagsNode, type GroupNode, type LookaroundAssertionKind,
  type LookaroundAssertionNode, type NamedCalloutKind, type NamedCalloutNode,
  type QuantifierKind, type QuantifierNode, type RegexNode, type SubroutineNode,
  type SubroutineRef, type TextSegmentMode, AbsenceFunctionE, Any, AssertionE,
  BackreferenceE, CapturingGroupE, CharacterCCE, CharacterClassCCE,
  CharacterClassE, CharacterClassRangeCCE, CharacterE, CharacterSetCCE,
  CharacterSetE, Cmp, Count, Custom, Digit, DirectiveE, Dot, ErrorCallout, Fail,
  Flags, Grapheme, Greedy, GroupE, Hex, Intersection, Keep, Lazy, LineEnd,
  LineStart, Lookahead, LookaroundAssertionE, Lookbehind, Max, Mismatch,
  NamedCalloutE, NamedRef, NamedSubroutineRef, Newline, NumberArg, NumberedRef,
  NumberedSubroutineRef, Posix, Possessive, Property, QuantifierE, Repeater,
  SearchStart, Skip, Space, StringArg, StringEnd, StringEndNewline, StringStart,
  SubroutineE, TextSegment, TextSegmentBoundary, Union, Word, WordBoundary,
  WordMode,
}
import glimra/oniguruma_parser/unicode
import glimra/oniguruma_to_es/transform
import glimra/oniguruma_to_es/transform/types.{
  type RegexPlusAst, type RegexPlusFlags, type Strategy, type TransformOptions,
  ClipSearch, TransformConfig,
}
import glimra/themes.{type BundledTheme, theme_id}
import glimra/types/token.{type ThemedToken, ThemedToken}
import simplifile
import startest/expect

// ============================================
// FFI for JSON comparison
// ============================================

/// Compare two JSON strings semantically (ignoring key order)
@external(javascript, "./json_compare_ffi.mjs", "compare_json")
fn compare_json(a: String, b: String) -> Bool

/// Convert a dynamic value to a JSON string
@external(javascript, "./json_compare_ffi.mjs", "stringify_dynamic")
fn stringify_dynamic(value: decode.Dynamic) -> String

/// Find first difference between two JSON strings (for debugging)
@external(javascript, "./json_compare_ffi.mjs", "json_diff")
fn json_diff(a: String, b: String) -> String

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
        Ok(ast_dyn) -> stringify_dynamic(ast_dyn)
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
          // Parse with Gleam parser using same options as JS generator
          // (singleline=true, capture_group=true, skip_backref_validation=true,
          //  normalize_unknown_property_names=true, unicodePropertyMap=JsUnicodePropertyMap)
          let parse_opts =
            parser.ParseOptions(
              ..parser.default_options(),
              singleline: True,
              capture_group: True,
              skip_backref_validation: True,
              normalize_unknown_property_names: True,
              unicode_property_map: option.Some(
                unicode.js_unicode_property_map(),
              ),
            )
          case parser.parse(entry.pattern, parse_opts) {
            Error(err) ->
              Ok("Pattern \"" <> entry.pattern <> "\" failed to parse: " <> err)
            Ok(ast) -> {
              // Convert to JSON and compare semantically
              let gleam_json = ast_to_string(ast)
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
/// Uses FFI to JavaScript to avoid stack overflow on large ASTs
fn compare_json_strings(a: String, b: String) -> Bool {
  compare_json(a, b)
}

// ============================================
// RegexPlusAst Validation (for transform testing)
// ============================================

/// Type for expected RegexPlusAst pattern entry
pub type ExpectedRegexPlusAstPattern {
  ExpectedRegexPlusAstPattern(
    pattern: String,
    success: Bool,
    regex_plus_ast_json: String,
  )
}

/// Type for expected RegexPlusAst file
pub type ExpectedRegexPlusAstFile {
  ExpectedRegexPlusAstFile(
    language: String,
    total: Int,
    successful: Int,
    failed: Int,
    patterns: List(ExpectedRegexPlusAstPattern),
  )
}

/// Read expected RegexPlusAst from JSON file
pub fn read_expected_regex_plus_ast(
  lang: Language,
) -> Result(ExpectedRegexPlusAstFile, String) {
  let lang_id = language_id(lang)
  let path = "test/snippets/" <> lang_id <> "/expected_regex_plus_ast.json"

  case simplifile.read(path) {
    Error(_) -> Error("Failed to read expected RegexPlusAst file: " <> path)
    Ok(content) -> decode_expected_regex_plus_ast(content)
  }
}

fn decode_expected_regex_plus_ast(
  content: String,
) -> Result(ExpectedRegexPlusAstFile, String) {
  // Decode just the metadata
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
    Error(_) -> Error("Failed to decode expected RegexPlusAst JSON metadata")
    Ok(#(language, total, successful, failed)) -> {
      // Now extract patterns using a different approach
      let patterns_decoder =
        decode.field("patterns", decode.list(decode.dynamic), fn(patterns_dyn) {
          decode.success(patterns_dyn)
        })

      case json.parse(content, patterns_decoder) {
        Error(_) -> Error("Failed to decode patterns array")
        Ok(patterns_dyn) -> {
          let patterns =
            patterns_dyn
            |> list.filter_map(fn(p) { decode_regex_plus_ast_pattern_entry(p) })
          Ok(ExpectedRegexPlusAstFile(
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

fn decode_regex_plus_ast_pattern_entry(
  dyn: decode.Dynamic,
) -> Result(ExpectedRegexPlusAstPattern, Nil) {
  let pattern_decoder =
    decode.field("pattern", decode.string, fn(pattern) {
      decode.field("success", decode.bool, fn(success) {
        decode.success(#(pattern, success))
      })
    })

  case decode.run(dyn, pattern_decoder) {
    Error(_) -> Error(Nil)
    Ok(#(pattern, success)) -> {
      // Now get the regexPlusAst field as raw JSON
      let ast_decoder =
        decode.field("regexPlusAst", decode.dynamic, fn(ast_dyn) {
          decode.success(ast_dyn)
        })

      let regex_plus_ast_json = case decode.run(dyn, ast_decoder) {
        Ok(ast_dyn) -> stringify_dynamic(ast_dyn)
        Error(_) -> ""
      }

      Ok(ExpectedRegexPlusAstPattern(
        pattern: pattern,
        success: success,
        regex_plus_ast_json: regex_plus_ast_json,
      ))
    }
  }
}

/// Validate expected RegexPlusAst for a language by parsing, transforming, and comparing
pub fn validate_expected_regex_plus_ast(lang: Language) -> Nil {
  let expected =
    read_expected_regex_plus_ast(lang)
    |> expect.to_be_ok()

  let errors =
    expected.patterns
    |> list.filter_map(fn(entry) {
      case entry.success {
        False ->
          // Skip patterns that failed in JS
          Error(Nil)
        True -> {
          // Parse with Gleam parser using same options as JS generator
          // (singleline=true, capture_group=true, skip_backref_validation=true,
          //  normalize_unknown_property_names=true, unicodePropertyMap=JsUnicodePropertyMap)
          let parse_opts =
            parser.ParseOptions(
              ..parser.default_options(),
              singleline: True,
              capture_group: True,
              skip_backref_validation: True,
              normalize_unknown_property_names: True,
              unicode_property_map: option.Some(
                unicode.js_unicode_property_map(),
              ),
            )
          case parser.parse(entry.pattern, parse_opts) {
            Error(err) ->
              Ok("Pattern \"" <> entry.pattern <> "\" failed to parse: " <> err)
            Ok(ast) -> {
              // Transform the AST using same options as JS generator
              // (asciiWordBoundaries=true, accuracy=default, etc.)
              let config =
                TransformConfig(
                  ..transform.default_config(),
                  ascii_word_boundaries: True,
                )
              case transform.transform(ast, config) {
                Error(err) ->
                  Ok(
                    "Pattern \""
                    <> entry.pattern
                    <> "\" failed to transform: "
                    <> err,
                  )
                Ok(regex_plus_ast) -> {
                  // Convert to JSON and compare semantically
                  let gleam_json = regex_plus_ast_to_string(regex_plus_ast)
                  case
                    compare_json_strings(gleam_json, entry.regex_plus_ast_json)
                  {
                    True -> Error(Nil)
                    False -> {
                      let diff =
                        json_diff(entry.regex_plus_ast_json, gleam_json)
                      Ok(
                        "Pattern \""
                        <> string.slice(entry.pattern, 0, 80)
                        <> "...\" mismatch:\nDiff: "
                        <> diff,
                      )
                    }
                  }
                }
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

// ============================================
// AST JSON Serialization (for testing)
// ============================================

/// Serialize an AST to a JSON string
pub fn ast_to_string(ast: RegexNode) -> String {
  regex_to_json(ast)
  |> json.to_string
}

/// Serialize a RegexPlusAst to a JSON string
pub fn regex_plus_ast_to_string(rpa: RegexPlusAst) -> String {
  regex_plus_ast_to_json(rpa)
  |> json.to_string
}

fn regex_to_json(node: RegexNode) -> json.Json {
  json.object([
    #("type", json.string("Regex")),
    #("body", json.array(node.body, alternative_to_json)),
    #("flags", flags_to_json(node.flags)),
  ])
}

fn alternative_to_json(node: AlternativeNode) -> json.Json {
  json.object([
    #("type", json.string("Alternative")),
    #("body", json.array(node.body, element_to_json)),
  ])
}

fn element_to_json(element: AlternativeElement) -> json.Json {
  case element {
    AbsenceFunctionE(n) -> absence_function_to_json(n)
    AssertionE(n) -> assertion_to_json(n)
    BackreferenceE(n) -> backreference_to_json(n)
    CapturingGroupE(n) -> capturing_group_to_json(n)
    CharacterE(n) -> character_to_json(n)
    CharacterClassE(n) -> character_class_to_json(n)
    CharacterSetE(n) -> character_set_to_json(n)
    DirectiveE(n) -> directive_to_json(n)
    GroupE(n) -> group_to_json(n)
    LookaroundAssertionE(n) -> lookaround_assertion_to_json(n)
    NamedCalloutE(n) -> named_callout_to_json(n)
    QuantifierE(n) -> quantifier_to_json(n)
    SubroutineE(n) -> subroutine_to_json(n)
  }
}

fn absence_function_to_json(node: AbsenceFunctionNode) -> json.Json {
  json.object([
    #("type", json.string("AbsenceFunction")),
    #("kind", absence_function_kind_to_json(node.kind)),
    #("body", json.array(node.body, alternative_to_json)),
  ])
}

fn absence_function_kind_to_json(kind: AbsenceFunctionKind) -> json.Json {
  case kind {
    Repeater -> json.string("repeater")
  }
}

fn assertion_to_json(node: AssertionNode) -> json.Json {
  let base = [
    #("type", json.string("Assertion")),
    #("kind", assertion_kind_to_json(node.kind)),
  ]
  let with_negate = case node.negate {
    option.Some(n) -> list.append(base, [#("negate", json.bool(n))])
    option.None -> base
  }
  json.object(with_negate)
}

fn assertion_kind_to_json(kind: AssertionKind) -> json.Json {
  case kind {
    LineEnd -> json.string("line_end")
    LineStart -> json.string("line_start")
    SearchStart -> json.string("search_start")
    StringEnd -> json.string("string_end")
    StringEndNewline -> json.string("string_end_newline")
    StringStart -> json.string("string_start")
    TextSegmentBoundary -> json.string("text_segment_boundary")
    WordBoundary -> json.string("word_boundary")
  }
}

fn backreference_to_json(node: BackreferenceNode) -> json.Json {
  let base = [
    #("type", json.string("Backreference")),
    #("ref", backref_ref_to_json(node.ref)),
  ]
  let with_orphan = case node.orphan {
    option.Some(o) -> list.append(base, [#("orphan", json.bool(o))])
    option.None -> base
  }
  json.object(with_orphan)
}

fn backref_ref_to_json(ref: BackreferenceRef) -> json.Json {
  case ref {
    NumberedRef(n) -> json.int(n)
    NamedRef(name) -> json.string(name)
  }
}

fn capturing_group_to_json(node: CapturingGroupNode) -> json.Json {
  // -1 is a sentinel for null (used for dummy captures)
  let number_json = case node.number {
    -1 -> json.null()
    n -> json.int(n)
  }
  let base = [
    #("type", json.string("CapturingGroup")),
    #("number", number_json),
  ]
  // Name comes before body in the expected output
  let with_name = case node.name {
    option.Some(n) -> list.append(base, [#("name", json.string(n))])
    option.None -> base
  }
  // Then body
  let with_body =
    list.append(with_name, [
      #("body", json.array(node.body, alternative_to_json)),
    ])
  // Then isSubroutined - only output if true (JS doesn't output false)
  let with_subroutined = case node.is_subroutined {
    option.Some(True) ->
      list.append(with_body, [#("isSubroutined", json.bool(True))])
    option.Some(False) | option.None -> with_body
  }
  json.object(with_subroutined)
}

fn character_to_json(node: CharacterNode) -> json.Json {
  json.object([
    #("type", json.string("Character")),
    #("value", json.int(node.value)),
  ])
}

fn character_class_to_json(node: CharacterClassNode) -> json.Json {
  json.object([
    #("type", json.string("CharacterClass")),
    #("kind", character_class_kind_to_json(node.kind)),
    #("negate", json.bool(node.negate)),
    #("body", json.array(node.body, cc_element_to_json)),
  ])
}

fn character_class_kind_to_json(kind: CharacterClassKind) -> json.Json {
  case kind {
    Union -> json.string("union")
    Intersection -> json.string("intersection")
  }
}

fn cc_element_to_json(element: CharacterClassElement) -> json.Json {
  case element {
    CharacterCCE(n) -> character_to_json(n)
    CharacterClassCCE(n) -> character_class_to_json(n)
    CharacterClassRangeCCE(n) -> character_class_range_to_json(n)
    CharacterSetCCE(n) -> character_set_to_json(n)
  }
}

fn character_class_range_to_json(node: CharacterClassRangeNode) -> json.Json {
  json.object([
    #("type", json.string("CharacterClassRange")),
    #("min", character_to_json(node.min)),
    #("max", character_to_json(node.max)),
  ])
}

fn character_set_to_json(node: CharacterSetNode) -> json.Json {
  let base = [
    #("type", json.string("CharacterSet")),
    #("kind", character_set_kind_to_json(node.kind)),
  ]
  let with_value = case node.value {
    option.Some(v) -> list.append(base, [#("value", json.string(v))])
    option.None -> base
  }
  let with_negate = case node.negate {
    option.Some(n) -> list.append(with_value, [#("negate", json.bool(n))])
    option.None -> with_value
  }
  let with_var_len = case node.variable_length {
    option.Some(vl) ->
      list.append(with_negate, [#("variableLength", json.bool(vl))])
    option.None -> with_negate
  }
  json.object(with_var_len)
}

fn character_set_kind_to_json(kind: CharacterSetKind) -> json.Json {
  case kind {
    Any -> json.string("any")
    Digit -> json.string("digit")
    Dot -> json.string("dot")
    Hex -> json.string("hex")
    Newline -> json.string("newline")
    Posix -> json.string("posix")
    Property -> json.string("property")
    Space -> json.string("space")
    TextSegment -> json.string("text_segment")
    Word -> json.string("word")
  }
}

fn directive_to_json(node: DirectiveNode) -> json.Json {
  let base = [
    #("type", json.string("Directive")),
    #("kind", directive_kind_to_json(node.kind)),
  ]
  let with_flags = case node.flags {
    option.Some(f) ->
      list.append(base, [#("flags", flag_group_modifiers_to_json(f))])
    option.None -> base
  }
  json.object(with_flags)
}

fn directive_kind_to_json(kind: DirectiveKind) -> json.Json {
  case kind {
    Keep -> json.string("keep")
    Flags -> json.string("flags")
  }
}

fn flags_to_json(node: FlagsNode) -> json.Json {
  let text_segment_json = case node.text_segment_mode {
    option.Some(tsm) -> text_segment_mode_to_json(tsm)
    option.None -> json.null()
  }
  json.object([
    #("type", json.string("Flags")),
    #("ignoreCase", json.bool(node.ignore_case)),
    #("dotAll", json.bool(node.dot_all)),
    #("extended", json.bool(node.extended)),
    #("digitIsAscii", json.bool(node.digit_is_ascii)),
    #("posixIsAscii", json.bool(node.posix_is_ascii)),
    #("spaceIsAscii", json.bool(node.space_is_ascii)),
    #("wordIsAscii", json.bool(node.word_is_ascii)),
    #("textSegmentMode", text_segment_json),
  ])
}

fn text_segment_mode_to_json(mode: TextSegmentMode) -> json.Json {
  case mode {
    Grapheme -> json.string("grapheme")
    WordMode -> json.string("word")
  }
}

fn flag_group_modifiers_to_json(modifiers: FlagGroupModifiers) -> json.Json {
  let base = []
  let with_enable = case modifiers.enable {
    option.Some(e) ->
      list.append(base, [#("enable", flag_group_switches_to_json(e))])
    option.None -> base
  }
  let with_disable = case modifiers.disable {
    option.Some(d) ->
      list.append(with_enable, [#("disable", flag_group_switches_to_json(d))])
    option.None -> with_enable
  }
  json.object(with_disable)
}

fn flag_group_switches_to_json(switches: FlagGroupSwitches) -> json.Json {
  let base = []
  let with_ic = case switches.ignore_case {
    option.Some(ic) -> list.append(base, [#("ignoreCase", json.bool(ic))])
    option.None -> base
  }
  let with_da = case switches.dot_all {
    option.Some(da) -> list.append(with_ic, [#("dotAll", json.bool(da))])
    option.None -> with_ic
  }
  let with_ext = case switches.extended {
    option.Some(ext) -> list.append(with_da, [#("extended", json.bool(ext))])
    option.None -> with_da
  }
  json.object(with_ext)
}

fn group_to_json(node: GroupNode) -> json.Json {
  // Build fields in order: type, atomic?, flags?, body
  let base = [#("type", json.string("Group"))]
  let with_atomic = case node.atomic {
    option.Some(a) -> list.append(base, [#("atomic", json.bool(a))])
    option.None -> base
  }
  let with_flags = case node.flags {
    option.Some(f) ->
      list.append(with_atomic, [#("flags", flag_group_modifiers_to_json(f))])
    option.None -> with_atomic
  }
  let with_body =
    list.append(with_flags, [
      #("body", json.array(node.body, alternative_to_json)),
    ])
  json.object(with_body)
}

fn lookaround_assertion_to_json(node: LookaroundAssertionNode) -> json.Json {
  json.object([
    #("type", json.string("LookaroundAssertion")),
    #("kind", lookaround_kind_to_json(node.kind)),
    #("negate", json.bool(node.negate)),
    #("body", json.array(node.body, alternative_to_json)),
  ])
}

fn lookaround_kind_to_json(kind: LookaroundAssertionKind) -> json.Json {
  case kind {
    Lookahead -> json.string("lookahead")
    Lookbehind -> json.string("lookbehind")
  }
}

fn named_callout_to_json(node: NamedCalloutNode) -> json.Json {
  let base = [
    #("type", json.string("NamedCallout")),
    #("kind", named_callout_kind_to_json(node.kind)),
  ]
  let with_tag = case node.tag {
    option.Some(t) -> list.append(base, [#("tag", json.string(t))])
    option.None -> base
  }
  let with_args = case node.arguments {
    option.Some(args) ->
      list.append(with_tag, [
        #("arguments", json.array(args, callout_arg_to_json)),
      ])
    option.None -> with_tag
  }
  json.object(with_args)
}

fn named_callout_kind_to_json(kind: NamedCalloutKind) -> json.Json {
  case kind {
    Count -> json.string("count")
    Cmp -> json.string("cmp")
    ErrorCallout -> json.string("error")
    Fail -> json.string("fail")
    Max -> json.string("max")
    Mismatch -> json.string("mismatch")
    Skip -> json.string("skip")
    ast_types.TotalCount -> json.string("total_count")
    Custom -> json.string("custom")
  }
}

fn callout_arg_to_json(arg: CalloutArg) -> json.Json {
  case arg {
    StringArg(s) -> json.string(s)
    NumberArg(n) -> json.int(n)
  }
}

fn quantifier_to_json(node: QuantifierNode) -> json.Json {
  // If max is the infinity constant, serialize as null (matching JS behavior)
  let max_json = case node.max == ast_types.quantifier_max_infinity {
    True -> json.null()
    False -> json.int(node.max)
  }
  json.object([
    #("type", json.string("Quantifier")),
    #("kind", quantifier_kind_to_json(node.kind)),
    #("min", json.int(node.min)),
    #("max", max_json),
    #("body", quantifiable_to_json(node.body)),
  ])
}

fn quantifier_kind_to_json(kind: QuantifierKind) -> json.Json {
  case kind {
    Greedy -> json.string("greedy")
    Lazy -> json.string("lazy")
    Possessive -> json.string("possessive")
  }
}

fn quantifiable_to_json(node: ast_types.QuantifiableNode) -> json.Json {
  case node {
    ast_types.AbsenceFunctionQ(n) -> absence_function_to_json(n)
    ast_types.BackreferenceQ(n) -> backreference_to_json(n)
    ast_types.CapturingGroupQ(n) -> capturing_group_to_json(n)
    ast_types.CharacterQ(n) -> character_to_json(n)
    ast_types.CharacterClassQ(n) -> character_class_to_json(n)
    ast_types.CharacterSetQ(n) -> character_set_to_json(n)
    ast_types.GroupQ(n) -> group_to_json(n)
    ast_types.QuantifierQ(n) -> quantifier_to_json(n)
    ast_types.SubroutineQ(n) -> subroutine_to_json(n)
  }
}

fn subroutine_to_json(node: SubroutineNode) -> json.Json {
  let base = [
    #("type", json.string("Subroutine")),
    #("ref", subroutine_ref_to_json(node.ref)),
  ]
  let with_recursive = case node.is_recursive {
    option.Some(True) -> list.append(base, [#("isRecursive", json.bool(True))])
    _ -> base
  }
  json.object(with_recursive)
}

fn subroutine_ref_to_json(ref: SubroutineRef) -> json.Json {
  case ref {
    NumberedSubroutineRef(n) -> json.int(n)
    NamedSubroutineRef(name) -> json.string(name)
  }
}

fn regex_plus_ast_to_json(rpa: RegexPlusAst) -> json.Json {
  json.object([
    #("type", json.string("Regex")),
    #("body", json.array(rpa.ast.body, alternative_to_json)),
    #("flags", regex_plus_flags_to_json(rpa.flags)),
    #("options", transform_options_to_json(rpa.options)),
    #("_originMap", origin_map_to_json(rpa.origin_map)),
    #("_strategy", strategy_to_json(rpa.strategy)),
  ])
}

fn regex_plus_flags_to_json(flags: RegexPlusFlags) -> json.Json {
  json.object([
    #("type", json.string("Flags")),
    #("ignoreCase", json.bool(flags.ignore_case)),
    #("dotAll", json.bool(flags.dot_all)),
    #("global", json.bool(flags.global)),
    #("hasIndices", json.bool(flags.has_indices)),
    #("multiline", json.bool(flags.multiline)),
    #("sticky", json.bool(flags.sticky)),
  ])
}

fn transform_options_to_json(options: TransformOptions) -> json.Json {
  json.object([
    #(
      "disable",
      json.object([
        #("x", json.bool(options.disable.x)),
        #("n", json.bool(options.disable.n)),
      ]),
    ),
    #("force", json.object([#("v", json.bool(options.force.v))])),
  ])
}

fn origin_map_to_json(
  origin_map: List(#(CapturingGroupNode, CapturingGroupNode)),
) -> json.Json {
  let pairs =
    origin_map
    |> list.map(fn(pair) {
      let #(copy, origin) = pair
      json.array(
        [capturing_group_to_json(copy), capturing_group_to_json(origin)],
        fn(x) { x },
      )
    })
  json.preprocessed_array(pairs)
}

fn strategy_to_json(strategy: option.Option(Strategy)) -> json.Json {
  case strategy {
    option.None -> json.null()
    option.Some(ClipSearch) -> json.string("clip_search")
  }
}
