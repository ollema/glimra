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

const HIGHLIGHT_NAMES: [&str; 52] = [
    "attribute",
    "boolean",
    "carriage-return",
    "comment",
    "comment.documentation",
    "constant",
    "constant.builtin",
    "constructor",
    "constructor.builtin",
    "embedded",
    "error",
    "escape",
    "function",
    "function.builtin",
    "keyword",
    "markup",
    "markup.bold",
    "markup.heading",
    "markup.italic",
    "markup.link",
    "markup.link.url",
    "markup.list",
    "markup.list.checked",
    "markup.list.numbered",
    "markup.list.unchecked",
    "markup.list.unnumbered",
    "markup.quote",
    "markup.raw",
    "markup.raw.block",
    "markup.raw.inline",
    "markup.strikethrough",
    "module",
    "number",
    "operator",
    "property",
    "property.builtin",
    "punctuation",
    "punctuation.bracket",
    "punctuation.delimiter",
    "punctuation.special",
    "string",
    "string.escape",
    "string.regexp",
    "string.special",
    "string.special.symbol",
    "tag",
    "type",
    "type.builtin",
    "variable",
    "variable.builtin",
    "variable.member",
    "variable.parameter",
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

static HASKELL_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_haskell::language(),
        "haskell",
        tree_sitter_haskell::HIGHLIGHTS_QUERY,
        tree_sitter_haskell::INJECTIONS_QUERY,
        tree_sitter_haskell::LOCALS_QUERY,
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static HEEX_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_heex::language(),
        "heex",
        tree_sitter_heex::HIGHLIGHTS_QUERY,
        tree_sitter_heex::INJECTIONS_QUERY,
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

static MARKDOWN_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_md::language(),
        "markdown",
        tree_sitter_md::HIGHLIGHT_QUERY_BLOCK,
        tree_sitter_md::INJECTION_QUERY_BLOCK,
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

static TYPESCRIPT_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_typescript::language_typescript(),
        "typescript",
        tree_sitter_typescript::HIGHLIGHTS_QUERY,
        "",
        tree_sitter_typescript::LOCALS_QUERY,
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static YAML_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_yaml::language(),
        "yaml",
        tree_sitter_yaml::HIGHLIGHTS_QUERY,
        "",
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
        ("haskell", &HASKELL_CONFIG),
        ("heex", &HEEX_CONFIG),
        ("html", &HTML_CONFIG),
        ("javascript", &JS_CONFIG),
        ("json", &JSON_CONFIG),
        ("markdown", &MARKDOWN_CONFIG),
        ("python", &PYTHON_CONFIG),
        ("rust", &RUST_CONFIG),
        ("typescript", &TYPESCRIPT_CONFIG),
        ("yaml", &YAML_CONFIG),
    ]
}

#[rustler::nif]
fn get_highlight_types(env: Env) -> Vec<NifTerm> {
    HIGHLIGHT_NAMES
        .iter()
        .map(|name| name.encode(env))
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

rustler::init!("libglans");
