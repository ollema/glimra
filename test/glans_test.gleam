import birdie
import glans
import gleeunit
import lustre/element
import simplifile

pub fn main() {
  gleeunit.main()
}

fn highlight_snippet(file: String, language: String) -> String {
  let assert Ok(source) = simplifile.read(file)
  let assert Ok(highlighted_snippet) = glans.syntax_highlight(source, language)
  element.to_string(highlighted_snippet)
}

pub fn bash_syntax_highlighting_test() {
  highlight_snippet("test/snippets/snippet.bash", "bash")
  |> birdie.snap(title: "test bash syntax highlighting")
}
