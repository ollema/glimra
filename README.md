# glimra

[![Package Version](https://img.shields.io/hexpm/v/glimra)](https://hex.pm/packages/glimra)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glimra/)
![Erlang-compatible](https://img.shields.io/badge/target-erlang-b83998)

Zero runtime syntax highlighter for [`lustre/ssg`](https://github.com/lustre-labs/ssg).

> In Swedish, `glimra` describes the brilliant gleam or lustre that comes from a polished, reflective surface, capturing the essence of light shining off something smooth and glossy.

## Platform support

`glimra` uses [NIFs](https://www.erlang.org/doc/system/nif) to extract syntax highlighting events provided by the [`tree-sitter`](https://crates.io/crates/tree-sitter) and [`tree-sitter-highlight`](https://crates.io/crates/tree-sitter-highlight) crates. This allows `glimra` to provide syntax highlighting for a wide range of languages with minimal effort.

Unfortunately, this also means that `glimra` only works with the Erlang target. Furthermore, you either need to have access to the Rust toolchain or you can use the provided precompiled binaries.

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

```gleam
import glimra

pub fn main() {
  let source = "let greeting = \"Hello, Joe!\""

  let highlighted_snippet =
    glimra.syntax_highlighter(language: "gleam")
    |> glimra.syntax_highlight(source: source)
}
```

Further documentation can be found at <https://hexdocs.pm/glimra>.

## Development

```sh
gleam test  # run the tests
```
