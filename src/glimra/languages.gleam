//// Languages - Single source of truth for all language definitions
////
//// This module defines the Language enum and all associated metadata.
//// All other modules should import from here rather than defining their own mappings.

/// Supported languages for syntax highlighting.
/// Each variant represents a language with bundled grammar support.
pub type Language {
  Astro
  Bash
  C
  Clojure
  Cpp
  Crystal
  Css
  Dart
  Diff
  Docker
  Elixir
  Elm
  Erlang
  Gleam
  Glsl
  Go
  Graphql
  Haskell
  Html
  Ini
  Javascript
  Json
  Julia
  Kotlin
  Latex
  Lua
  Markdown
  Mojo
  Nim
  Nix
  Odin
  Ocaml
  Php
  Python
  R
  Ruby
  Rust
  Scala
  Scheme
  Svelte
  Swift
  Toml
  Tsx
  Typescript
  Typst
  Vue
  Wasm
  Wgsl
  Yaml
  Zig
}

/// Metadata about a language
pub type LanguageInfo {
  LanguageInfo(
    /// Constructor name (e.g., "Javascript")
    constructor: String,
    /// Language identifier used in APIs (e.g., "javascript")
    id: String,
    /// Grammar filename (e.g., "javascript.json")
    grammar_file: String,
    /// Languages that should be loaded alongside this one
    dependencies: List(Language),
  )
}

/// Get all metadata for a language.
/// This is THE source of truth for language information.
pub fn language_info(lang: Language) -> LanguageInfo {
  case lang {
    Astro ->
      LanguageInfo("Astro", "astro", "astro.json", [
        Html, Css, Javascript, Typescript, Tsx,
      ])
    Bash -> LanguageInfo("Bash", "bash", "shellscript.json", [])
    C -> LanguageInfo("C", "c", "c.json", [])
    Clojure -> LanguageInfo("Clojure", "clojure", "clojure.json", [])
    Cpp -> LanguageInfo("Cpp", "cpp", "cpp.json", [])
    Crystal -> LanguageInfo("Crystal", "crystal", "crystal.json", [])
    Css -> LanguageInfo("Css", "css", "css.json", [])
    Dart -> LanguageInfo("Dart", "dart", "dart.json", [])
    Diff -> LanguageInfo("Diff", "diff", "diff.json", [])
    Docker -> LanguageInfo("Docker", "docker", "docker.json", [])
    Elixir -> LanguageInfo("Elixir", "elixir", "elixir.json", [])
    Elm -> LanguageInfo("Elm", "elm", "elm.json", [])
    Erlang -> LanguageInfo("Erlang", "erlang", "erlang.json", [])
    Gleam -> LanguageInfo("Gleam", "gleam", "gleam.json", [])
    Glsl -> LanguageInfo("Glsl", "glsl", "glsl.json", [C])
    Go -> LanguageInfo("Go", "go", "go.json", [])
    Graphql -> LanguageInfo("Graphql", "graphql", "graphql.json", [])
    Haskell -> LanguageInfo("Haskell", "haskell", "haskell.json", [])
    Html -> LanguageInfo("Html", "html", "html.json", [Css, Javascript])
    Ini -> LanguageInfo("Ini", "ini", "ini.json", [])
    Javascript ->
      LanguageInfo("Javascript", "javascript", "javascript.json", [])
    Json -> LanguageInfo("Json", "json", "json.json", [])
    Julia -> LanguageInfo("Julia", "julia", "julia.json", [])
    Kotlin -> LanguageInfo("Kotlin", "kotlin", "kotlin.json", [])
    Latex -> LanguageInfo("Latex", "latex", "latex.json", [])
    Lua -> LanguageInfo("Lua", "lua", "lua.json", [])
    Markdown ->
      LanguageInfo("Markdown", "markdown", "markdown.json", [
        Html, Css, Javascript, Typescript, Json, Yaml, Bash, Python, Ruby, Rust,
      ])
    Mojo -> LanguageInfo("Mojo", "mojo", "mojo.json", [])
    Nim -> LanguageInfo("Nim", "nim", "nim.json", [])
    Nix -> LanguageInfo("Nix", "nix", "nix.json", [])
    Odin -> LanguageInfo("Odin", "odin", "odin.json", [])
    Ocaml -> LanguageInfo("Ocaml", "ocaml", "ocaml.json", [])
    Php -> LanguageInfo("Php", "php", "php.json", [Html, Css, Javascript])
    Python -> LanguageInfo("Python", "python", "python.json", [])
    R -> LanguageInfo("R", "r", "r.json", [])
    Ruby -> LanguageInfo("Ruby", "ruby", "ruby.json", [])
    Rust -> LanguageInfo("Rust", "rust", "rust.json", [])
    Scala -> LanguageInfo("Scala", "scala", "scala.json", [])
    Scheme -> LanguageInfo("Scheme", "scheme", "scheme.json", [])
    Svelte ->
      LanguageInfo("Svelte", "svelte", "svelte.json", [
        Html, Css, Javascript, Typescript,
      ])
    Swift -> LanguageInfo("Swift", "swift", "swift.json", [])
    Toml -> LanguageInfo("Toml", "toml", "toml.json", [])
    Tsx -> LanguageInfo("Tsx", "tsx", "tsx.json", [Typescript, Javascript])
    Typescript ->
      LanguageInfo("Typescript", "typescript", "typescript.json", [])
    Typst -> LanguageInfo("Typst", "typst", "typst.json", [])
    Vue ->
      LanguageInfo("Vue", "vue", "vue.json", [Html, Css, Javascript, Typescript])
    Wasm -> LanguageInfo("Wasm", "wasm", "wasm.json", [])
    Wgsl -> LanguageInfo("Wgsl", "wgsl", "wgsl.json", [])
    Yaml -> LanguageInfo("Yaml", "yaml", "yaml.json", [])
    Zig -> LanguageInfo("Zig", "zig", "zig.json", [])
  }
}

/// All supported languages
pub fn all_languages() -> List(Language) {
  [
    Astro, Bash, C, Clojure, Cpp, Crystal, Css, Dart, Diff, Docker, Elixir, Elm,
    Erlang, Glsl, Gleam, Go, Graphql, Haskell, Html, Ini, Javascript, Json,
    Julia, Kotlin, Latex, Lua, Markdown, Mojo, Nim, Nix, Odin, Ocaml, Php,
    Python, R, Ruby, Rust, Scala, Scheme, Svelte, Swift, Toml, Tsx, Typescript,
    Typst, Vue, Wasm, Wgsl, Yaml, Zig,
  ]
}

/// Get the language identifier (e.g., "javascript")
pub fn language_id(lang: Language) -> String {
  language_info(lang).id
}

/// Get the grammar filename (e.g., "javascript.json")
pub fn language_grammar_file(lang: Language) -> String {
  language_info(lang).grammar_file
}

/// Get language dependencies
pub fn language_dependencies(lang: Language) -> List(Language) {
  language_info(lang).dependencies
}
