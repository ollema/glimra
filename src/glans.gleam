import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/string

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

pub fn build_index_to_type_map() -> Dict(Int, String) {
  dict.from_list(list.index_map(get_highlight_types(), fn(x, i) { #(i, x) }))
}

@external(erlang, "libglans", "get_highlight_events")
fn get_highlight_events(
  source_code: String,
  lang_atom: String,
) -> Result(List(HighlightEvent), String)

pub fn main() {
  let source =
    "fn main() { 
  println!(\"Hello, world!\"); 
}"
  let language = Rust

  let highlights = syntax_highlight(source, language)

  use highlight <- list.each(highlights)
  let #(highlight_type, text) = highlight
  io.println(highlight_type <> ":" <> "\"" <> text <> "\"")
}

fn syntax_highlight(source: String, lang: Language) -> List(#(String, String)) {
  let assert Ok(events) = get_highlight_events(source, language_to_string(lang))
  let index_to_type_map = build_index_to_type_map()
  do_process_raw_highlights(source, events, [], [], "", index_to_type_map)
}

fn do_process_raw_highlights(
  source: String,
  events: List(HighlightEvent),
  nif_events: List(#(String, String)),
  current_highlights: List(Int),
  current_snippet: String,
  index_to_type_map: Dict(Int, String),
) -> List(#(String, String)) {
  case events {
    [event, ..rest] -> {
      case event {
        HighlightStart(highlight_type) -> {
          do_process_raw_highlights(
            source,
            rest,
            nif_events,
            [highlight_type, ..current_highlights],
            current_snippet,
            index_to_type_map,
          )
        }
        HighlightEnd -> {
          case current_highlights {
            [current_highlight, ..rest_of_current_highlights] -> {
              io.println(
                int.to_string(current_highlight)
                <> ": "
                <> "\""
                <> current_snippet
                <> "\"",
              )
              do_process_raw_highlights(
                source,
                rest,
                nif_events,
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
          let snippet = string.slice(source, start, end - start)
          do_process_raw_highlights(
            source,
            rest,
            nif_events,
            current_highlights,
            current_snippet <> snippet,
            index_to_type_map,
          )
        }
      }
    }
    [] -> nif_events
  }
}
