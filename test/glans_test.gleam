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
