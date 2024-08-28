import birdie
import glans
import gleam/result
import gleeunit
import lustre/element
import simplifile

pub fn main() {
  gleeunit.main()
}

fn highlight_snippet(file: String, language: String) -> String {
  let assert Ok(source) = simplifile.read(file)

  let assert Ok(highlighted_string) =
    glans.syntax_highlighter(language)
    |> glans.syntax_highlight(source)
    |> result.map(element.to_string)

  highlighted_string
}

pub fn bash_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.bash", "bash")
  |> birdie.snap(title: "test bash syntax highlighting")
}

pub fn c_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.c", "c")
  |> birdie.snap(title: "test c syntax highlighting")
}

pub fn css_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.css", "css")
  |> birdie.snap(title: "test css syntax highlighting")
}

pub fn djot_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.dj", "djot")
  |> birdie.snap(title: "test djot syntax highlighting")
}

pub fn elixir_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.ex", "elixir")
  |> birdie.snap(title: "test elixir syntax highlighting")
}

pub fn erlang_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.erlang_file", "erlang")
  |> birdie.snap(title: "test erlang syntax highlighting")
}

pub fn gleam_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.gleam_file", "gleam")
  |> birdie.snap(title: "test gleam syntax highlighting")
}

pub fn go_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.go", "go")
  |> birdie.snap(title: "test go syntax highlighting")
}

pub fn html_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.html", "html")
  |> birdie.snap(title: "test html syntax highlighting")
}

pub fn javascript_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.js", "javascript")
  |> birdie.snap(title: "test javascript syntax highlighting")
}

pub fn json_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.json", "json")
  |> birdie.snap(title: "test json syntax highlighting")
}

pub fn python_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.py", "python")
  |> birdie.snap(title: "test python syntax highlighting")
}

pub fn rust_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.rs", "rust")
  |> birdie.snap(title: "test rust syntax highlighting")
}

pub fn custom_line_class_test() {
  let source = "let greeting = \"Hello, Joe!\""

  let assert Ok(highlighted_string) =
    glans.syntax_highlighter("gleam")
    |> glans.line_class("linje")
    |> glans.syntax_highlight(source)
    |> result.map(element.to_string)

  highlighted_string
  |> birdie.snap(title: "test custom line class")
}

pub fn custom_block_class_test() {
  let source = "let greeting = \"Hello, Joe!\""

  let assert Ok(highlighted_string) =
    glans.syntax_highlighter("gleam")
    |> glans.block_class("kod")
    |> glans.syntax_highlight(source)
    |> result.map(element.to_string)

  highlighted_string
  |> birdie.snap(title: "test custom block class")
}
