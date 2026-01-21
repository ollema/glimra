# glimra

A Gleam syntax highlighter powered by TextMate grammars, inspired by [Shiki](https://shiki.style/).

## Features

- 50 bundled languages
- 10 bundled themes (Nord, Dracula, GitHub Dark, Tokyo Night, etc.)
- Type-safe builder API with compile-time guarantees
- Uses VS Code's TextMate grammars via Oniguruma FFI

## Usage

```gleam
import glimra.{new_highlighter, with_language, with_bundled_theme, build, code_to_tokens, tokens_options_bundled}
import glimra/languages.{Javascript}
import glimra/themes.{Nord}

pub fn main() {
  let assert Ok(highlighter) =
    new_highlighter()
    |> with_language(Javascript)
    |> with_bundled_theme(Nord)
    |> build()

  let options = tokens_options_bundled(Javascript, Nord)
  let assert Ok(#(_, result)) = code_to_tokens(highlighter, "const x = 42;", options)
  // result.tokens contains themed tokens for rendering
}
```

## Development

```sh
gleam test                          # Run all tests
gleam test -- snippets/javascript   # Test specific language
```

## Adding Languages

### Prerequisites

- TextMate grammar file (JSON format)
- Representative code snippet covering major language features

### Steps

1. **Register the language** in `src/glimra/languages.gleam`:
   - Add variant to `Language` type (alphabetically)
   - Add case to `language_info()` function
   - Add to `all_languages()` list

2. **Add grammar file** to `priv/grammars/<language>.json`
   - If sourcing from textmate-grammars-themes, codegen handles this

3. **Create test snippet** at `test/snippets/<language>/snippet.txt`
   - Cover wide range of language features (see existing snippets for examples)

4. **Run codegen**:

   ```bash
   gleam run -m codegen /path/to/textmate-grammars-themes
   ```

5. **Run tests**:

   ```bash
   gleam test -- snippets/<language>
   ```
