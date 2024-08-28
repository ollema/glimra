// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/list
import gleam/result
import gleam/string
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

// FFI -------------------------------------------------------------------------

@external(erlang, "libglans", "get_supported_languages")
fn get_supported_languages() -> List(String)

@external(erlang, "libglans", "get_highlight_events")
fn get_highlight_events(
  source_code: String,
  lang_atom: String,
) -> Result(List(HighlightEvent), String)

@external(erlang, "libglans", "get_highlight_name")
fn get_highlight_name(index: Int) -> String

// MAIN ------------------------------------------------------------------------

pub fn syntax_highlighter(language language: String) -> Config {
  Config(language: language, line_class: "line", block_class: "")
}

pub fn syntax_highlight(
  config config: Config,
  source source: String,
) -> Result(Element(Nil), SyntaxHighlightingError) {
  let language = string.lowercase(config.language)
  let language_supported = list.contains(get_supported_languages(), language)

  use <- bool.guard(
    when: !language_supported,
    return: Error(UnsupportedLanguage(language)),
  )

  use events <- result.try(
    get_highlight_events(source, language)
    // TODO: figure out if there is a way to get the error message from the NIF
    // and either map it to a SyntaxHighlightingError or have it directly be mapped
    // to a SyntaxHighlightingError from the FFI function definition
    |> result.replace_error(TreeSitterError),
  )

  use reversed_lines <- result.try(do_syntax_highlight(
    source:,
    events:,
    config:,
    code_block: [],
    code_row: [],
    highlights: [],
    snippet: "",
  ))

  Ok(html.code(
    [attribute.class(config.block_class)],
    list.reverse(reversed_lines),
  ))
}

// TYPES -----------------------------------------------------------------------

pub opaque type Config {
  Config(language: String, line_class: String, block_class: String)
}

pub type SyntaxHighlightingError {
  UnsupportedLanguage(language: String)
  TreeSitterError
  UnmatchedHighlightEvents
}

type HighlightEvent {
  HighlightStart(highlight_type: Int)
  Source(start: Int, end: Int)
  HighlightEnd
}

// BUILDERS --------------------------------------------------------------------

pub fn line_class(config config: Config, class class: String) -> Config {
  Config(..config, line_class: class)
}

pub fn block_class(config config: Config, class class: String) -> Config {
  Config(..config, block_class: class)
}

// IMPLEMENTATION --------------------------------------------------------------

fn do_syntax_highlight(
  source source: String,
  events events: List(HighlightEvent),
  config config: Config,
  // accumulators below
  code_block code_block: List(Element(Nil)),
  code_row code_row: List(Element(Nil)),
  highlights highlights: List(Int),
  snippet snippet: String,
) -> Result(List(Element(Nil)), SyntaxHighlightingError) {
  let #(highlight_name, rest_of_highlights) = parse_highlights(highlights)

  case events {
    [event, ..rest] -> {
      case event {
        HighlightStart(highlight_type) -> {
          case snippet {
            "" -> {
              // if the current snippet is empty, we just add the highlight type
              do_syntax_highlight(
                source:,
                events: rest,
                config:,
                code_block:,
                code_row:,
                highlights: [highlight_type, ..highlights],
                snippet:,
              )
            }

            _ -> {
              // add the current snippet to the current row before starting a new highlight
              do_syntax_highlight(
                source:,
                events: rest,
                config:,
                code_block:,
                code_row: prepend_with_snippet(
                  code_row:,
                  highlight_name: highlight_name,
                  snippet: snippet,
                ),
                highlights: [highlight_type, ..highlights],
                snippet: "",
              )
            }
          }
        }

        HighlightEnd -> {
          case
            string.is_empty(highlight_name) && list.is_empty(rest_of_highlights)
          {
            True -> {
              // if there is no current highlight, we have an unmatched highlight end
              Error(UnmatchedHighlightEvents)
            }
            False -> {
              // add the current snippet to the current row before discarding the current highlight
              do_syntax_highlight(
                source:,
                events: rest,
                config:,
                code_block:,
                code_row: prepend_with_snippet(
                  code_row:,
                  highlight_name: highlight_name,
                  snippet: snippet,
                ),
                highlights: rest_of_highlights,
                snippet: "",
              )
            }
          }
        }

        Source(start, end) -> {
          let new_snippet = string.slice(source, start, end - start)

          // if the new snippet contains a newline...
          case string.contains(does: new_snippet, contain: "\n") {
            True -> {
              // then we want to add what is before the linebreak
              // to the current <span> and then add a new row
              // and add what is after the linebreak to the next row
              // while keeping the current highlight type, if any
              let assert Ok(#(before_linebreak, after_linebreak)) =
                string.split_once(new_snippet, on: "\n")

              do_syntax_highlight(
                source:,
                events: rest,
                config:,
                code_block: prepend_with_linebreak(
                  config:,
                  current_code_block: code_block,
                  children: prepend_with_snippet(
                    code_row:,
                    highlight_name: highlight_name,
                    snippet: snippet <> before_linebreak,
                  ),
                ),
                code_row: prepend_with_snippet(
                  code_row: [],
                  highlight_name: highlight_name,
                  snippet: after_linebreak,
                ),
                highlights:,
                snippet: "",
              )
            }

            False -> {
              // otherwise we just add the snippet to the current snippet
              do_syntax_highlight(
                source:,
                events: rest,
                config:,
                code_block:,
                code_row:,
                highlights:,
                snippet: snippet <> new_snippet,
              )
            }
          }
        }
      }
    }

    [] -> {
      // add remaining snippet to the current
      Ok(prepend_with_linebreak(
        config:,
        current_code_block: code_block,
        children: prepend_with_snippet(
          code_row:,
          highlight_name: highlight_name,
          snippet: snippet,
        ),
      ))
    }
  }
}

// HELPERS ---------------------------------------------------------------------

fn parse_highlights(highlights: List(Int)) -> #(String, List(Int)) {
  case highlights {
    [highlight, ..rest_of_highlights] -> {
      #(get_highlight_name(highlight), rest_of_highlights)
    }
    [] -> {
      #("", [])
    }
  }
}

// ELEMENT BUILDERS ------------------------------------------------------------

fn prepend_with_snippet(
  code_row siblings: List(Element(Nil)),
  highlight_name highlight_name: String,
  snippet snippet: String,
) -> List(Element(Nil)) {
  case string.is_empty(snippet) {
    True -> siblings
    False -> [
      html.span([attribute.class(highlight_name)], [html.text(snippet)]),
      ..siblings
    ]
  }
}

fn prepend_with_linebreak(
  config config: Config,
  current_code_block siblings: List(Element(Nil)),
  children children: List(Element(Nil)),
) -> List(Element(Nil)) {
  [
    html.span([attribute.class(config.line_class)], list.reverse(children)),
    ..siblings
  ]
}
