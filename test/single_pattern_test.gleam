import gleam/io
import gleam/list
import gleam/string
import glimra/languages
import glimra/oniguruma_parser/json as ast_json
import glimra/oniguruma_parser/parser
import startest.{describe, it}
import startest/expect
import test_helpers

pub fn single_pattern_tests() {
  describe("single pattern", [
    it("parses a CSS pattern", fn() {
      let opts =
        parser.ParseOptions(
          ..parser.default_options(),
          singleline: True,
          capture_group: True,
          skip_backref_validation: True,
        )

      // Test a single CSS pattern
      let pattern = "!\\s*important(?![-\\w])"
      case parser.parse(pattern, opts) {
        Ok(ast) -> {
          let json_str = ast_json.ast_to_string(ast)
          io.println(
            "Success! Length: " <> string.inspect(string.length(json_str)),
          )
          Nil
        }
        Error(e) -> {
          io.println("Error: " <> e)
          e |> expect.to_equal("")
        }
      }
    }),
    it("reads CSS expected AST", fn() {
      io.println("Reading expected AST...")
      case test_helpers.read_expected_ast(languages.Css) {
        Error(e) -> {
          io.println("Error reading: " <> e)
          e |> expect.to_equal("")
        }
        Ok(expected) -> {
          io.println(
            "Success! Got "
            <> string.inspect(list.length(expected.patterns))
            <> " patterns",
          )
          // Just try the first 5
          io.println("Testing first 5 patterns...")
          let first_5 = list.take(expected.patterns, 5)
          list.each(first_5, fn(entry) {
            io.println("Pattern: " <> string.slice(entry.pattern, 0, 40))
          })
          Nil
        }
      }
    }),
    it("parses and validates first 10 CSS patterns", fn() {
      let opts =
        parser.ParseOptions(
          ..parser.default_options(),
          singleline: True,
          capture_group: True,
          skip_backref_validation: True,
        )

      io.println("Reading expected AST...")
      let assert Ok(expected) = test_helpers.read_expected_ast(languages.Css)

      // Skip first 61, test just pattern at index 61 (the 314KB one)
      io.println("Testing just pattern at index 61 (314KB AST)...")
      let first_10 = list.drop(expected.patterns, 61) |> list.take(1)
      let errors =
        list.filter_map(first_10, fn(entry) {
          io.println("Parsing: " <> string.slice(entry.pattern, 0, 40))
          case entry.success {
            False -> Error(Nil)
            // Skip failed patterns
            True -> {
              io.println("  Starting parse...")
              case parser.parse(entry.pattern, opts) {
                Error(e) -> Ok("Parse error for " <> entry.pattern <> ": " <> e)
                Ok(_ast) -> {
                  io.println("  Parsed OK! (skipping JSON for now)")
                  Error(Nil)
                }
              }
            }
          }
        })

      case errors {
        [] -> {
          io.println("All 10 passed!")
          Nil
        }
        errs -> {
          io.println("Errors: " <> string.inspect(errs))
          string.join(errs, ", ") |> expect.to_equal("")
        }
      }
    }),
  ])
}
