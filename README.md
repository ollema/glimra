# glimra

[![Package Version](https://img.shields.io/hexpm/v/glimra)](https://hex.pm/packages/glimra)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glimra/)
![Erlang-compatible](https://img.shields.io/badge/target-erlang-b83998)

Zero runtime syntax highlighter for [`lustre/ssg`](https://github.com/lustre-labs/ssg).

> In Swedish, `glimra` describes the brilliant gleam or lustre that comes from a polished, reflective surface, capturing the essence of light shining off something smooth and glossy.

## Platform support

`glimra` uses [NIFs](https://www.erlang.org/doc/system/nif) to extract syntax highlighting events provided by the [`tree-sitter`](https://crates.io/crates/tree-sitter) and [`tree-sitter-highlight`](https://crates.io/crates/tree-sitter-highlight) crates. This allows `glimra` to provide syntax highlighting for a wide range of languages with minimal effort.

Unfortunately, this also means that `glimra` only works with the Erlang target. You can use the provided precompiled binaries for the supported platforms or compile the NIFs yourself with the Rust toolchain.

Currently, the following operating systems and architectures are supported:

| OS      | Architecture(s) |
| ------- | --------------- |
| Linux   | x86_64          |
| macOS   | aarch64, x86_64 |
| Windows | x86_64          |

## Installation

```sh
gleam add glimra
```

## Usage

`glimra` can be used to highlight source code snippets in a variety of languages. While the primary use case is to provide syntax highlighting for code snippets in the `lustre/ssg` static site generator, it can also be used in a more standalone fashion:

```gleam
import glimra
import glimra/theme

pub fn main() {
  let syntax_highlighter =
    glimra.new_syntax_highlighter() |> glimra.set_theme(theme.default_theme())

  let source = "let greeting = \"Hello, Joe!\""
  let language = "gleam"

  let highlighted_snippet =
    syntax_highlighter
    |> glimra.syntax_highlight(source:, language:)

  let css = syntax_highlighter |> glimra.to_css()
}
```

### Usage with `lustre/ssg`

`glimra` provides a set of utilities to make it easier to work with `glimra` and `lustre/ssg`.

Given a typical `lustre/ssg` `build.gleam` file with the following `main()` function, you can generate and add the static stylesheet using the `add_static_stylesheet` builder:

```gleam
import glimra
import glimra/theme

pub fn main() {
  let syntax_highlighter =
    glimra.new_syntax_highlighter()
    |> glimra.set_theme(theme.default_theme())

  let build = ssg.new("./priv")
    |> ssg.add_static_route("/", index.view())
    |> ssg.add_static_route("/blog", blog.view(posts.all()))
    // the `add_static_stylesheet` builder works with your existing `lustre/ssg` config
    |> glimra.add_static_stylesheet(syntax_highlighter: syntax_highlighter)
    |> ssg.build
}
```

You can also link to the generated static stylesheet in your header:

```gleam
import glimra

fn head(title: String, description: String) {
  html.head([], [
    html.title([], title),
    html.meta([attribute.attribute("charset", "utf-8")]),
    html.meta([
      attribute.attribute("name", "viewport"),
      attribute.attribute("content", "width=device-width, initial-scale=1"),
    ]),
    html.link([attribute.href("/style.css"), attribute.rel("stylesheet")]),
    // `link_static_stylesheet` will use the same path as `add_static_stylesheet`
    glimra.link_static_stylesheet(),
  ])
}
```

Finally, you can use the included `codeblock_renderer` with your `lustre/ssg/djot` render configuration:

```gleam
pub fn parse(
  from filepath: String,
  // pass in the same `syntax_highlighter` you used to generate the stylesheet
  syntax_highlighter syntax_highlighter: Config(HasTheme),
) -> String {
  let renderer =
    djot.Renderer(
      ..djot.default_renderer(),
      // this will use `codeblock_renderer` to highlight code snippets
      codeblock: glimra.codeblock_renderer(syntax_highlighter),
    )

  let content = {
    use file <- result.try(
      simplifile.read(filepath) |> result.replace_error(Nil),
    )
    djot.render(file, renderer)
  }

  case content {
    Ok(content) -> content
    Error(_) -> {
      let error_message = "could not parse content from file: " <> filepath
      panic as error_message
    }
  }
}
```

## Configuration

`glimra` can be configured to include line numbers, to keep or trim whitespace, and to use a custom theme. The following configuration options are available:

```gleam
pub type Config(HasTheme) {
  line_numbers: Bool,
  trim_whitespace: Bool,
  theme: theme.Theme,
}
```

These are set with the builder pattern:

```gleam
let syntax_highlighter =
  glimra.new_syntax_highlighter()
  |> glimra.set_line_numbers(true)
  |> glimra.set_trim_whitespace(true)
  |> glimra.set_theme(theme.default_theme())
```

Note that the `theme` is required for any CSS generation to work!

---

Further documentation can be found at <https://hexdocs.pm/glimra>.

## Development

```sh
gleam test  # run the tests
```
