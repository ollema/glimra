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

// TODO: use builders!!!
pub fn syntax_highlight(
  source source: String,
  language language: String,
) -> Result(Element(Nil), SyntaxHighlightingError) {
  let language = string.lowercase(language)
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
    source: source,
    events: events,
    code_block: [],
    current_code_row: [],
    current_highlights: [],
    current_snippet: "",
  ))

  Ok(html.code([], list.reverse(reversed_lines)))
}

// TYPES -----------------------------------------------------------------------

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

// IMPLEMENTATION -------------------------------------------------------------

fn do_syntax_highlight(
  source source: String,
  events events: List(HighlightEvent),
  code_block code_block: List(Element(Nil)),
  current_code_row current_code_row: List(Element(Nil)),
  current_highlights current_highlights: List(Int),
  current_snippet current_snippet: String,
) -> Result(List(Element(Nil)), SyntaxHighlightingError) {
  let #(current_highlight_name, rest_of_current_highlights) = case
    current_highlights
  {
    [current_highlight, ..rest_of_current_highlights] -> {
      #(get_highlight_name(current_highlight), rest_of_current_highlights)
    }

    [] -> {
      #("", [])
    }
  }

  case events {
    [event, ..rest] -> {
      case event {
        HighlightStart(highlight_type) -> {
          case current_snippet {
            "" -> {
              do_syntax_highlight(
                source:,
                events: rest,
                code_block:,
                current_code_row:,
                current_highlights: [highlight_type, ..current_highlights],
                current_snippet:,
              )
            }

            _ -> {
              do_syntax_highlight(
                source:,
                events: rest,
                code_block:,
                current_code_row: prepend_with_snippet(
                  current_code_row:,
                  highlight_name: current_highlight_name,
                  snippet: current_snippet,
                ),
                current_highlights: [highlight_type, ..current_highlights],
                current_snippet: "",
              )
            }
          }
        }

        HighlightEnd -> {
          do_syntax_highlight(
            source:,
            events: rest,
            code_block:,
            current_code_row: prepend_with_snippet(
              current_code_row:,
              highlight_name: current_highlight_name,
              snippet: current_snippet,
            ),
            current_highlights: rest_of_current_highlights,
            current_snippet: "",
          )
        }

        Source(start, end) -> {
          let snippet = string.slice(source, start, end - start)

          // if the snippet contains a newline...
          case string.contains(does: snippet, contain: "\n") {
            True -> {
              // then we want to add what is before the linebreak
              // to the current <span> and then add a new row
              // and add what is after the linebreak to the next row
              // while keeping the current highlight type, if any
              let assert Ok(#(before_linebreak, after_linebreak)) =
                string.split_once(snippet, on: "\n")

              do_syntax_highlight(
                source:,
                events: rest,
                code_block: prepend_with_linebreak(
                  current_code_block: code_block,
                  children: prepend_with_snippet(
                    current_code_row:,
                    highlight_name: current_highlight_name,
                    snippet: current_snippet <> before_linebreak,
                  ),
                ),
                current_code_row: prepend_with_snippet(
                  current_code_row: [],
                  highlight_name: current_highlight_name,
                  snippet: after_linebreak,
                ),
                current_highlights:,
                current_snippet: "",
              )
            }

            False -> {
              do_syntax_highlight(
                source:,
                events: rest,
                code_block:,
                current_code_row:,
                current_highlights:,
                current_snippet: current_snippet <> snippet,
              )
            }
          }
        }
      }
    }

    [] -> {
      Ok(prepend_with_linebreak(
        current_code_block: code_block,
        children: prepend_with_snippet(
          current_code_row:,
          highlight_name: current_highlight_name,
          snippet: current_snippet,
        ),
      ))
    }
  }
}

// ELEMENT BUILDERS ------------------------------------------------------------

fn prepend_with_snippet(
  current_code_row siblings: List(Element(Nil)),
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
  current_code_block siblings: List(Element(Nil)),
  children children: List(Element(Nil)),
) -> List(Element(Nil)) {
  [html.span([attribute.class("line")], list.reverse(children)), ..siblings]
}
