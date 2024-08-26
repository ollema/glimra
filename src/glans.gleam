import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/string
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub type HighlightEvent {
  HighlightStart(highlight_type: Int)
  Source(start: Int, end: Int)
  HighlightEnd
}

pub type Language {
  Rust
  // TODO: add more here
}

fn language_to_string(language: Language) {
  case language {
    Rust -> "rust"
    // TODO: add more here
  }
}

@external(erlang, "libglans", "get_highlight_types")
fn get_highlight_types() -> List(String)

@external(erlang, "libglans", "get_highlight_events")
fn get_highlight_events(
  source_code: String,
  lang_atom: String,
) -> Result(List(HighlightEvent), String)

pub fn syntax_highlight(
  source source: String,
  language language: Language,
) -> List(Element(Nil)) {
  let index_to_type_map =
    dict.from_list(list.index_map(get_highlight_types(), fn(x, i) { #(i, x) }))

  let events = case get_highlight_events(source, language_to_string(language)) {
    Ok(events) -> events
    _ -> panic as "could not perform syntax highlighting!"
  }

  list.reverse(do_syntax_highlighting(
    source: source,
    index_to_type_map: index_to_type_map,
    events: events,
    code_block: [],
    current_code_row: [],
    current_highlights: [],
    current_snippet: "",
  ))
}

pub fn main() {
  let source =
    "fn main() { 
  println!(\"Hello, world!\"); 
}"

  let language = Rust

  html.code([], syntax_highlight(source:, language:))
  |> element.to_string
  |> io.print
}

fn do_syntax_highlighting(
  source source: String,
  index_to_type_map index_to_type_map: Dict(Int, String),
  events events: List(HighlightEvent),
  code_block code_block: List(Element(Nil)),
  current_code_row current_code_row: List(Element(Nil)),
  current_highlights current_highlights: List(Int),
  current_snippet current_snippet: String,
) -> List(Element(Nil)) {
  case events {
    [event, ..rest] -> {
      case event {
        HighlightStart(highlight_type) -> {
          do_syntax_highlighting(
            source:,
            index_to_type_map:,
            events: rest,
            code_block:,
            current_code_row:,
            current_highlights: [highlight_type, ..current_highlights],
            current_snippet:,
          )
        }

        HighlightEnd -> {
          case current_highlights {
            [current_highlight, ..rest_of_current_highlights] -> {
              let highlight_type =
                get_highlight_type(index_to_type_map, current_highlight)

              do_syntax_highlighting(
                source:,
                index_to_type_map:,
                events: rest,
                code_block:,
                current_code_row: prepend_with_snippet(
                  current_code_row:,
                  highlight_type:,
                  snippet: current_snippet,
                ),
                current_highlights: rest_of_current_highlights,
                current_snippet: "",
              )
            }

            [] -> {
              panic as "unmatched start events and end events!"
            }
          }
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

              case current_highlights {
                [current_highlight, ..] -> {
                  let highlight_type =
                    get_highlight_type(index_to_type_map, current_highlight)

                  do_syntax_highlighting(
                    source:,
                    index_to_type_map:,
                    events: rest,
                    code_block: prepend_with_linebreak(
                      current_code_block: code_block,
                      children: prepend_with_snippet(
                        current_code_row:,
                        highlight_type:,
                        snippet: current_snippet <> before_linebreak,
                      ),
                    ),
                    current_code_row: prepend_with_snippet(
                      current_code_row: [],
                      highlight_type:,
                      snippet: after_linebreak,
                    ),
                    current_highlights:,
                    current_snippet: "",
                  )
                }

                [] -> {
                  do_syntax_highlighting(
                    source:,
                    index_to_type_map:,
                    events: rest,
                    code_block: prepend_with_linebreak(
                      current_code_block: code_block,
                      children: prepend_with_snippet(
                        current_code_row:,
                        highlight_type: "",
                        snippet: current_snippet <> before_linebreak,
                      ),
                    ),
                    current_code_row: prepend_with_snippet(
                      current_code_row: [],
                      highlight_type: "",
                      snippet: after_linebreak,
                    ),
                    current_highlights:,
                    current_snippet: "",
                  )
                }
              }
            }

            False -> {
              do_syntax_highlighting(
                source:,
                index_to_type_map:,
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
      case current_highlights {
        [current_highlight, ..] -> {
          let highlight_type =
            get_highlight_type(index_to_type_map, current_highlight)

          prepend_with_linebreak(
            current_code_block: code_block,
            children: prepend_with_snippet(
              current_code_row:,
              highlight_type:,
              snippet: current_snippet,
            ),
          )
        }

        [] -> {
          prepend_with_linebreak(
            current_code_block: code_block,
            children: prepend_with_snippet(
              current_code_row:,
              highlight_type: "",
              snippet: current_snippet,
            ),
          )
        }
      }
    }
  }
}

fn get_highlight_type(
  index_to_type_map index_to_type_map: Dict(Int, String),
  current_highlight current_highlight: Int,
) {
  let assert Ok(highlight_type) = dict.get(index_to_type_map, current_highlight)
  highlight_type
}

fn prepend_with_snippet(
  current_code_row siblings: List(Element(Nil)),
  highlight_type highlight_type: String,
  snippet snippet: String,
) -> List(Element(Nil)) {
  case string.is_empty(snippet) {
    True -> siblings
    False -> [
      html.span([attribute.class(highlight_type)], [html.text(snippet)]),
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
