# glimra Development Guide

Always format, check and test changes.

When debugging, use filters to limit the scope of tests run.

## Commands

```bash
# Format code
gleam format

# Type check
gleam check

# Run all tests (takes several minutes)
gleam test

# Run tests for a specific language (recommended when applicable)
gleam test -- snippets/javascript

# Run tests matching a name pattern, for example a theme name like "nord"
gleam test -- --test-name-filter="nord"

# Combine file and name filters
gleam test -- snippets/javascript --test-name-filter="nord"

# Run all codegen tasks (vendor + generate-references + generate-tests)
gleam run -m codegen /path/to/textmate-grammars-themes
```

# JavaScript reference:

When porting pacakges to Gleam, you can refer to the original JS code in the `js_reference` folder for guidance and implementation details.

`oniguruma-to-es`: `js_reference/oniguruma-to-es/src/`
`oniguruma-parser`: `js_reference/oniguruma-parser/src/`
`regex`: `js_reference/regex/src/`
`regex-recursion`: `js_reference/regex-recursion/src/`
`regex-utilities`: `js_reference/regex-utilities/src/`

