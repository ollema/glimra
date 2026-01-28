import gleam/io
import gleam/option.{None}
import glimra/oniguruma_parser/parser
import glimra/oniguruma_to_es/transform
import glimra/oniguruma_parser/json
import startest.{describe, it}
import startest/expect

pub fn debug_transform_tests() {
  describe("debug transform", [
    it("shows transform output for simple pattern", fn() {
      let pattern = "\\b_\\p{word}+{0,1}\\b"
      io.println("\n\nPattern: " <> pattern)

      // Use same options as test_helpers (TextMate grammar style)
      let options =
        parser.ParseOptions(
          flags: "",
          normalize_unknown_property_names: False,
          skip_backref_validation: True,
          skip_lookbehind_validation: False,
          skip_property_name_validation: False,
          unicode_property_map: None,
          capture_group: True,
          singleline: True,
        )
      case parser.parse(pattern, options) {
        Ok(ast) -> {
          io.println("\nParsed AST:")
          io.println(json.ast_to_string(ast))

          let config = transform.default_config()
          case transform.transform(ast, config) {
            Ok(rpa) -> {
              io.println("\nTransformed RegexPlusAst:")
              io.println(json.regex_plus_ast_to_string(rpa))
            }
            Error(e) -> io.println("Transform error: " <> e)
          }
        }
        Error(e) -> io.println("Parse error: " <> e)
      }

      expect.to_equal(1, 1)
    }),
  ])
}
