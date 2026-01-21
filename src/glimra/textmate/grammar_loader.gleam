//// Pure Gleam grammar loader for parsing TextMate grammar JSON files
////
//// This module provides functions to parse TextMate grammar JSON into
//// RawGrammar structures that can be compiled into Grammar objects.

import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/json
import gleam/option.{type Option, None, Some}
import glimra/textmate/raw_grammar.{
  type RawGrammar, type RawRule, RawGrammar, RawRule,
}

/// Parse a grammar JSON string into a RawGrammar
pub fn parse_grammar_json(json_string: String) -> Result(RawGrammar, String) {
  case json.parse(json_string, raw_grammar_decoder()) {
    Ok(grammar) -> Ok(grammar)
    Error(e) -> Error("JSON parse error: " <> json_error_to_string(e))
  }
}

/// Convert a json.DecodeError to a string for error messages
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
    _ -> join_strings(path, ".")
  }
}

fn join_strings(strings: List(String), sep: String) -> String {
  case strings {
    [] -> ""
    [s] -> s
    [s, ..rest] -> s <> sep <> join_strings(rest, sep)
  }
}

/// Decoder for RawGrammar
fn raw_grammar_decoder() -> Decoder(RawGrammar) {
  use scope_name <- decode.field("scopeName", decode.string)
  use name <- decode.optional_field("name", None, option_string_decoder())
  use patterns <- decode.optional_field(
    "patterns",
    [],
    decode.list(raw_rule_decoder()),
  )
  use repository <- decode.optional_field(
    "repository",
    dict.new(),
    raw_rule_dict_decoder(),
  )
  use injections <- decode.optional_field(
    "injections",
    dict.new(),
    raw_rule_dict_decoder(),
  )
  use injection_selector <- decode.optional_field(
    "injectionSelector",
    None,
    option_string_decoder(),
  )
  use file_types <- decode.optional_field(
    "fileTypes",
    [],
    decode.list(decode.string),
  )
  use first_line_match <- decode.optional_field(
    "firstLineMatch",
    None,
    option_string_decoder(),
  )
  decode.success(RawGrammar(
    scope_name: scope_name,
    name: name,
    patterns: patterns,
    repository: repository,
    injections: injections,
    injection_selector: injection_selector,
    file_types: file_types,
    first_line_match: first_line_match,
  ))
}

/// Decoder for Option(String) - wraps a string in Some
fn option_string_decoder() -> Decoder(Option(String)) {
  use s <- decode.then(decode.string)
  decode.success(Some(s))
}

/// Decoder for Bool that also accepts 0/1 integers
fn bool_or_int_decoder() -> Decoder(Bool) {
  decode.one_of(decode.bool, [
    decode.int
    |> decode.map(fn(n) { n != 0 }),
  ])
}

/// Decoder for Dict(String, RawRule) - uses recursive decoder
fn raw_rule_dict_decoder() -> Decoder(Dict(String, RawRule)) {
  decode.dict(decode.string, raw_rule_decoder())
}

/// Decoder for RawRule - uses decode.recursive to handle self-referential structure
/// This handles all the optional fields that can appear in a TextMate rule
fn raw_rule_decoder() -> Decoder(RawRule) {
  use <- decode.recursive
  use include <- decode.optional_field("include", None, option_string_decoder())
  use name <- decode.optional_field("name", None, option_string_decoder())
  use content_name <- decode.optional_field(
    "contentName",
    None,
    option_string_decoder(),
  )
  use match_pattern <- decode.optional_field(
    "match",
    None,
    option_string_decoder(),
  )
  use captures <- decode.optional_field(
    "captures",
    dict.new(),
    decode.dict(decode.string, raw_rule_decoder()),
  )
  use begin <- decode.optional_field("begin", None, option_string_decoder())
  use begin_captures <- decode.optional_field(
    "beginCaptures",
    dict.new(),
    decode.dict(decode.string, raw_rule_decoder()),
  )
  use end <- decode.optional_field("end", None, option_string_decoder())
  use end_captures <- decode.optional_field(
    "endCaptures",
    dict.new(),
    decode.dict(decode.string, raw_rule_decoder()),
  )
  use while_pattern <- decode.optional_field(
    "while",
    None,
    option_string_decoder(),
  )
  use while_captures <- decode.optional_field(
    "whileCaptures",
    dict.new(),
    decode.dict(decode.string, raw_rule_decoder()),
  )
  use patterns <- decode.optional_field(
    "patterns",
    [],
    decode.list(raw_rule_decoder()),
  )
  use repository <- decode.optional_field(
    "repository",
    dict.new(),
    decode.dict(decode.string, raw_rule_decoder()),
  )
  use apply_end_pattern_last <- decode.optional_field(
    "applyEndPatternLast",
    False,
    bool_or_int_decoder(),
  )
  decode.success(RawRule(
    id: None,
    include: include,
    name: name,
    content_name: content_name,
    match_pattern: match_pattern,
    captures: captures,
    begin: begin,
    begin_captures: begin_captures,
    end: end,
    end_captures: end_captures,
    while_pattern: while_pattern,
    while_captures: while_captures,
    patterns: patterns,
    repository: repository,
    apply_end_pattern_last: apply_end_pattern_last,
  ))
}
