use core::str;
use once_cell::sync::Lazy;
use rustler::{Atom, Encoder, Env, Error as NifError, NifResult, Term as NifTerm};
use tree_sitter_highlight::{
    Error as HighlightError, HighlightConfiguration, HighlightEvent, Highlighter,
};

mod atoms {
    rustler::atoms! {
        source,
        highlight_start,
        highlight_end,
        highlight_cancelled,
        highlight_invalid_language,
        highlight_unknown,
        ok,
        unsupported_language,
    }
}

// taken from:
// https://github.com/nvim-treesitter/nvim-treesitter/blob/master/CONTRIBUTING.md#highlights
const HIGHLIGHT_NAMES: [&str; 56] = [
    // -------------------------------------------------------------------------
    // identifiers
    // -------------------------------------------------------------------------
    // variables
    "variable",                   // various variable names
    "variable.builtin",           // built-in variable names (e.g. `this`)
    "variable.member",            // object and struct fields
    "variable.parameter.builtin", // special parameters (e.g. `_`, `it`)
    "variable.parameter",         // parameters of a function
    // constants
    "constant",         // constant identifiers
    "constant.builtin", // built-in constant values
    "constant.macro",   // constants defined by the preprocessor
    // modules
    "module", // modules or namespaces
    // labels
    "label", // GOTO and other labels
    // -------------------------------------------------------------------------
    // literals
    // -------------------------------------------------------------------------
    // strings
    "string",               // string literals
    "string.documentation", // string documenting code
    "string.escape",        // escape sequences
    "string.regexp",        // regular expressions
    "string.special",       // other special strings (e.g. dates)
    // characters
    "character",         // character literals
    "character.special", // special characters (e.g. wildcards)
    // booleans
    "boolean", // boolean literals
    // numbers
    "number", // numeric literals
    // -------------------------------------------------------------------------
    // types
    // -------------------------------------------------------------------------
    // types
    "type",            // type or class definitions and annotations
    "type.builtin",    // built-in types
    "type.definition", // identifiers in type definitions (e.g. `typedef <type> <identifier>` in C)
    // attributes
    "attribute",         // attribute annotations (e.g. Python decorators, Rust lifetimes)
    "attribute.builtin", // builtin annotations (e.g. `@property` in Python)
    // properties
    "property", // the key in key/value pairs
    // -------------------------------------------------------------------------
    // functions
    // -------------------------------------------------------------------------
    // functions
    "function",             // function definitions
    "function.builtin",     // built-in functions
    "function.call",        // function calls
    "function.macro",       // preprocessor macros
    "function.method",      // method calls
    "function.method.call", // method calls
    // constructors
    "constructor", // constructor calls and definitions
    // operators
    "operator", // operators
    // -------------------------------------------------------------------------
    // keywords
    // -------------------------------------------------------------------------
    // keywords
    "keyword",                     // keywords not fitting into specific categories
    "keyword.coroutine", // keywords related to coroutines (e.g. `go` in Go, `async/await` in Python)
    "keyword.function",  // keywords that define a function (e.g. `func` in Go, `def` in Python)
    "keyword.operator",  // operators that are English words (e.g. `and` / `or`)
    "keyword.import", // keywords for including or exporting modules (e.g. `import` / `from` in Python)
    "keyword.type",   // keywords describing namespaces and composite types (e.g. `struct`, `enum`)
    "keyword.modifier", // keywords modifying other constructs (e.g. `const`, `static`, `public`)
    "keyword.repeat", // keywords related to loops (e.g. `for` / `while`)
    "keyword.return", // keywords like `return` and `yield`
    "keyword.debug",  // keywords related to debugging
    "keyword.exception", // keywords related to exceptions (e.g. `throw` / `catch`)
    "keyword.conditional", // keywords related to conditionals (e.g. `if` / `else`)
    "keyword.conditional.ternary", // ternary operator (e.g. `?` / `:`)"
    // -------------------------------------------------------------------------
    // punctuation
    // -------------------------------------------------------------------------
    // punctuation
    "punctuation",           // generic punctuation
    "punctuation.bracket",   // brackets (e.g. `()` / `{}` / `[]`)
    "punctuation.delimiter", // delimiters (e.g. `;` / `.` / `,`)
    "punctuation.special",   // special symbols (e.g. `{}` in string interpolation)
    // -------------------------------------------------------------------------
    // comments
    // -------------------------------------------------------------------------
    // comments
    "comment",               // line and block comments
    "comment.documentation", // comments documenting code
    // -------------------------------------------------------------------------
    // tags
    // -------------------------------------------------------------------------
    // tags
    "tag",           // XML-style tag names (and similar)
    "tag.builtin",   // builtin tag names (e.g. HTML5 tags)
    "tag.attribute", // XML-style tag attributes
    "tag.delimiter", // XML-style tag delimiters
];

static BASH_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_bash::language(),
        "bash",
        tree_sitter_bash::HIGHLIGHT_QUERY,
        "",
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static C_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_c::language(),
        "c",
        tree_sitter_c::HIGHLIGHT_QUERY,
        "",
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static CSS_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_css::language(),
        "c",
        tree_sitter_css::HIGHLIGHTS_QUERY,
        "",
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static ELIXIR_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_elixir::language(),
        "elixir",
        tree_sitter_elixir::HIGHLIGHTS_QUERY,
        r#"((sigil
            (sigil_name) @_sigil_name
            (quoted_content) @injection.content)
            (#eq? @_sigil_name "H")
            (#set! injection.language "heex")
            (#set! injection.combined))"#,
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static ERLANG_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_erlang::language(),
        "erlang",
        tree_sitter_erlang::HIGHLIGHTS_QUERY,
        "",
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static GLEAM_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_gleam::language(),
        "gleam",
        tree_sitter_gleam::HIGHLIGHTS_QUERY,
        "",
        tree_sitter_gleam::LOCALS_QUERY,
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static GO_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_go::language(),
        "go",
        tree_sitter_go::HIGHLIGHTS_QUERY,
        "",
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static HTML_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_html::language(),
        "html",
        tree_sitter_html::HIGHLIGHTS_QUERY,
        tree_sitter_html::INJECTIONS_QUERY,
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static JS_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_javascript::language(),
        "javascript",
        tree_sitter_javascript::HIGHLIGHT_QUERY,
        tree_sitter_javascript::INJECTIONS_QUERY,
        tree_sitter_javascript::LOCALS_QUERY,
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static JSON_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_json::language(),
        "json",
        tree_sitter_json::HIGHLIGHTS_QUERY,
        "",
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static PYTHON_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_python::language(),
        "python",
        tree_sitter_python::HIGHLIGHTS_QUERY,
        "",
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static RUST_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_rust::language(),
        "rust",
        tree_sitter_rust::HIGHLIGHTS_QUERY,
        tree_sitter_rust::INJECTIONS_QUERY,
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

fn translate_highlight_error(e: HighlightError) -> NifError {
    NifError::Term(Box::new(match e {
        HighlightError::Cancelled => atoms::highlight_cancelled(),
        HighlightError::InvalidLanguage => atoms::highlight_invalid_language(),
        HighlightError::Unknown => atoms::highlight_unknown(),
    }))
}

fn get_lang_tuples<'a>() -> Vec<(&'a str, &'a Lazy<HighlightConfiguration>)> {
    vec![
        ("bash", &BASH_CONFIG),
        ("c", &C_CONFIG),
        ("css", &CSS_CONFIG),
        ("elixir", &ELIXIR_CONFIG),
        ("erlang", &ERLANG_CONFIG),
        ("gleam", &GLEAM_CONFIG),
        ("go", &GO_CONFIG),
        ("html", &HTML_CONFIG),
        ("javascript", &JS_CONFIG),
        ("json", &JSON_CONFIG),
        ("python", &PYTHON_CONFIG),
        ("rust", &RUST_CONFIG),
    ]
}

#[rustler::nif]
fn get_supported_languages(env: Env) -> Vec<NifTerm> {
    get_lang_tuples()
        .iter()
        .map(|(lang, _)| lang.encode(env))
        .collect()
}

#[rustler::nif]
fn get_highlight_events<'a>(
    env: Env<'a>,
    source_code: &str,
    language: &str,
) -> NifResult<(Atom, Vec<NifTerm<'a>>)> {
    let get_config = |given_lang: &str| {
        for (language, config) in get_lang_tuples() {
            if language == given_lang {
                return Some(Lazy::force(config));
            }
        }
        return None;
    };

    let highlight_config = match get_config(language) {
        Some(c) => c,
        _ => {
            return Err(NifError::Term(Box::new(atoms::unsupported_language())));
        }
    };

    let mut highlighter = Highlighter::new();
    let highlight_result =
        highlighter.highlight(highlight_config, source_code.as_bytes(), None, get_config);

    let events = match highlight_result {
        Ok(events) => events,
        Err(e) => return Err(translate_highlight_error(e)),
    };

    let mut nif_events = Vec::new();

    for event in events {
        match event {
            Ok(HighlightEvent::Source { start, end }) => {
                nif_events.push((atoms::source(), start, end).encode(env));
            }
            Ok(HighlightEvent::HighlightStart(s)) => {
                nif_events.push((atoms::highlight_start(), s.0).encode(env));
            }
            Ok(HighlightEvent::HighlightEnd) => {
                nif_events.push((atoms::highlight_end()).encode(env));
            }
            Err(e) => {
                return Err(translate_highlight_error(e));
            }
        }
    }

    Ok((atoms::ok(), nif_events))
}

#[rustler::nif]
fn get_highlight_name<'a>(index: usize) -> NifResult<&'a str> {
    Ok(HIGHLIGHT_NAMES[index])
}

rustler::init!("libglimra");
