import gleam/io
import gleam/string

@external(erlang, "libglans", "get_language_from_filename")
pub fn get_language_from_filename(filename: String) -> String

@external(erlang, "libglans", "get_supported_languages")
pub fn get_supported_languages() -> List(String)

@external(erlang, "libglans", "get_highlight_events")
pub fn get_highlight_events(
  source_code: String,
  lang_atom: String,
) -> Result(#(String, String), String)

pub fn main() {
  let filename = "example.rs"
  let lang = get_language_from_filename(filename)
  let supported_languages = get_supported_languages()
  io.debug(supported_languages)

  let source_code = "fn main() { println!(\"Hello, world!\"); }"
  let result = get_highlight_events(source_code, lang)

  io.debug(string.inspect(result))
}
