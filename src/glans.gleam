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
}

fn language_to_string(language: Language) {
  case language {
    Rust -> "rust"
  }
}

@external(erlang, "libglans", "get_highlight_types")
fn get_highlight_types() -> List(String)

@external(erlang, "libglans", "get_highlight_events")
fn get_highlight_events(
  source_code: String,
  lang_atom: String,
) -> Result(List(HighlightEvent), String)

const demo_snippet = "
fn main() { 
  println!(\"Hello, world!\"); 
}"

const demo_lang = Rust

pub fn main() {
  html.code([], syntax_highlight(demo_snippet, demo_lang))
  |> element.to_string
  |> io.print
}

pub fn build_index_to_type_map() -> Dict(Int, String) {
  dict.from_list(list.index_map(get_highlight_types(), fn(x, i) { #(i, x) }))
}

fn syntax_highlight(source: String, language: Language) -> List(Element(Nil)) {
  let events = case get_highlight_events(source, language_to_string(language)) {
    Ok(events) -> events
    _ -> panic as "could not syntax highlight code snippet"
  }
  let index_to_type_map = build_index_to_type_map()
  list.reverse(do_process_raw_highlights(
    source,
    events,
    [],
    [],
    "",
    index_to_type_map,
  ))
}

fn do_process_raw_highlights(
  source: String,
  events: List(HighlightEvent),
  elements: List(Element(Nil)),
  current_highlights: List(Int),
  current_snippet: String,
  index_to_type_map: Dict(Int, String),
) -> List(Element(Nil)) {
  case events {
    [event, ..rest] -> {
      case event {
        HighlightStart(highlight_type) -> {
          do_process_raw_highlights(
            source,
            rest,
            elements,
            [highlight_type, ..current_highlights],
            current_snippet,
            index_to_type_map,
          )
        }
        HighlightEnd -> {
          case current_highlights {
            [current_highlight, ..rest_of_current_highlights] -> {
              let highlight_type = case
                dict.get(index_to_type_map, current_highlight)
              {
                Ok(highlight_type) -> highlight_type
                _ -> panic as "unknown highlight type!"
              }
              do_process_raw_highlights(
                source,
                rest,
                [syntax_block(highlight_type, current_snippet), ..elements],
                rest_of_current_highlights,
                "",
                index_to_type_map,
              )
            }
            [] -> {
              panic as "this should not happen. unmatched start events and end events"
            }
          }
        }
        Source(start, end) -> {
          let snippet =
            string.slice(source, start, end - start)
            |> string.replace("\n", "")
          do_process_raw_highlights(
            source,
            rest,
            elements,
            current_highlights,
            current_snippet <> snippet,
            index_to_type_map,
          )
        }
      }
    }
    [] -> elements
  }
}

pub fn syntax_block(highlight_type: String, snippet: String) -> Element(Nil) {
  html.span([attribute.class(highlight_type)], [html.text(snippet)])
}
