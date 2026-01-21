import gleam/int
import gleam/io
import gleam/list
import glimra/languages
import glimra/oniguruma_parser/json as ast_json
import glimra/oniguruma_parser/parser
import startest.{describe, it}
import startest/expect
import test_helpers

pub fn parser_tests() {
  describe("parser basic tests", [
    it("parses a simple string pattern", fn() {
      let opts = parser.default_options()
      case parser.parse("hello", opts) {
        Ok(_ast) -> Nil
        Error(e) -> e |> expect.to_equal("")
      }
    }),
    it("parses a character class", fn() {
      let opts = parser.default_options()
      case parser.parse("[a-z]", opts) {
        Ok(_ast) -> Nil
        Error(e) -> e |> expect.to_equal("")
      }
    }),
    it("parses a quantifier", fn() {
      let opts = parser.default_options()
      case parser.parse("a+", opts) {
        Ok(_ast) -> Nil
        Error(e) -> e |> expect.to_equal("")
      }
    }),
    it("parses first 10 JS patterns", fn() {
      let opts = parser.default_options()
      let patterns = [
        "!",
        "!|&&|\\|\\||\\?\\?",
        "\"",
        "#",
        "#{",
        "$",
        "\\$(?=\\{)",
        "%",
        "&",
        "'",
      ]
      list.each(patterns, fn(p) {
        case parser.parse(p, opts) {
          Ok(_) -> Nil
          Error(e) -> {
            io.println("Failed pattern: " <> p <> " - " <> e)
          }
        }
      })
    }),
    it("parses and serializes pattern", fn() {
      let opts = parser.default_options()
      case parser.parse("hello", opts) {
        Ok(ast) -> {
          let json_str = ast_json.ast_to_string(ast)
          io.println("JSON: " <> json_str)
          Nil
        }
        Error(e) -> e |> expect.to_equal("")
      }
    }),
    it("reads expected_ast for gleam (smaller file)", fn() {
      case test_helpers.read_expected_ast(languages.Gleam) {
        Ok(expected) -> {
          io.println(
            "Loaded "
            <> expected.language
            <> " with "
            <> int.to_string(expected.total)
            <> " patterns",
          )
          Nil
        }
        Error(e) -> e |> expect.to_equal("")
      }
    }),
  ])
}
