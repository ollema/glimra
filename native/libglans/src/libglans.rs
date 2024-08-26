use once_cell::sync::Lazy;
use rustler::env::Env;
use rustler::{Atom, Error as NifError, NifResult, Term as NifTerm};
use std::fmt::Write;
use tree_sitter_highlight::{
    Error as HighlightError, HighlightConfiguration, Highlighter, HtmlRenderer,
};

mod atoms {
    rustler::atoms! {
       highlight_cancelled,
       highlight_invalid_language,
       highlight_unknown,
       nil,
       ok,
       unsupported_language,
    }
}

const HIGHLIGHT_NAMES: [&str; 22] = [
    "attribute",
    "comment",
    "constant",
    "constant.builtin",
    "constructor",
    "embedded",
    "function",
    "function.builtin",
    "keyword",
    "module",
    "number",
    "operator",
    "property",
    "punctuation.bracket",
    "punctuation.delimiter",
    "string",
    "string.special",
    "tag",
    "type",
    "type.builtin",
    "variable.builtin",
    "variable.parameter",
];

static C_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_c::language(),
        tree_sitter_c::HIGHLIGHT_QUERY,
        "",
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
        tree_sitter_css::HIGHLIGHTS_QUERY,
        "",
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
        tree_sitter_elixir::HIGHLIGHTS_QUERY,
        r#"((sigil
            (sigil_name) @_sigil_name
            (quoted_content) @injection.content)
            (#eq? @_sigil_name "H")
            (#set! injection.language "heex")
            (#set! injection.combined))"#,
        "",
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static GO_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_go::language(),
        tree_sitter_go::HIGHLIGHTS_QUERY,
        "",
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
        tree_sitter_haskell::HIGHLIGHTS_QUERY,
        "",
        tree_sitter_haskell::LOCALS_QUERY,
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static HEEX_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_heex::language(),
        tree_sitter_heex::HIGHLIGHTS_QUERY,
        tree_sitter_heex::INJECTIONS_QUERY,
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
        tree_sitter_html::HIGHLIGHTS_QUERY,
        tree_sitter_html::INJECTIONS_QUERY,
        "",
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static JS_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_javascript::language(),
        tree_sitter_javascript::HIGHLIGHT_QUERY,
        tree_sitter_javascript::INJECTIONS_QUERY,
        tree_sitter_javascript::LOCALS_QUERY,
        "",
    )
    .unwrap();
    c.configure(&HIGHLIGHT_NAMES);
    c
});

static JSON_CONFIG: Lazy<HighlightConfiguration> = Lazy::new(|| {
    let mut c = HighlightConfiguration::new(
        tree_sitter_json::language(),
        tree_sitter_json::HIGHLIGHTS_QUERY,
        "",
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
        tree_sitter_rust::HIGHLIGHTS_QUERY,
        tree_sitter_rust::INJECTIONS_QUERY,
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

fn get_lang_tuples<'a>() -> Vec<(&'a str, &'a Lazy<HighlightConfiguration>, &'a str)> {
    vec![
        ("c", &C_CONFIG, ".c .h"),
        ("css", &CSS_CONFIG, ".css"),
        ("elixir", &ELIXIR_CONFIG, ".ex .exs"),
        ("go", &GO_CONFIG, ".go"),
        ("haskell", &HASKELL_CONFIG, ".hs"),
        ("heex", &HEEX_CONFIG, ".heex"),
        ("html", &HTML_CONFIG, ".html"),
        ("javascript", &JS_CONFIG, ".js .mjs"),
        ("json", &JSON_CONFIG, ".json"),
        ("rust", &RUST_CONFIG, ".rs"),
    ]
}

#[rustler::nif]
fn get_language_from_filename(env: Env, filename: &str) -> Atom {
    for (lang, _, extensions) in get_lang_tuples() {
        let parts = extensions.split(' ');
        for p in parts {
            if filename.ends_with(p) {
                return Atom::from_str(env, lang).unwrap();
            }
        }
    }
    return atoms::nil();
}

#[rustler::nif]
fn get_supported_languages(env: Env) -> Vec<Atom> {
    let mut r = Vec::new();
    for (lang, _, _) in get_lang_tuples() {
        r.push(Atom::from_str(env, lang).unwrap());
    }
    r
}

#[rustler::nif]
fn get_highlight_events<'a>(source_code: &str, l: NifTerm) -> NifResult<(Atom, String)> {
    let lang = l.atom_to_string().unwrap();

    let get_config = |given_lang: &str| {
        for (lang, config, _) in get_lang_tuples() {
            if lang == given_lang {
                return Some(Lazy::force(config));
            }
        }
        return None;
    };

    let highlight_config = match get_config(lang.as_str()) {
        Some(c) => c,
        _ => {
            return Err(NifError::Term(Box::new(atoms::unsupported_language())));
        }
    };

    let html_attrs = HIGHLIGHT_NAMES.map(|s| format!(r#"class="token {}""#, s.replace(".", " ")));
    let mut highlighter = Highlighter::new();

    let source_code_bytes = source_code.as_bytes();
    let highlight_result =
        highlighter.highlight(highlight_config, source_code_bytes, None, get_config);

    let events = match highlight_result {
        Ok(events) => Ok(events),
        Err(e) => Err(translate_highlight_error(e)),
    }?;

    let mut renderer = HtmlRenderer::new();

    match renderer.render(events, source_code_bytes, &|highlight| {
        html_attrs[highlight.0].as_bytes()
    }) {
        Ok(_) => (),
        Err(e) => return Err(translate_highlight_error(e)),
    }

    let mut html = String::new();
    writeln!(html, r#"<pre class="code-block language-{lang}"><code>"#).unwrap();
    for (i, line) in renderer.lines().enumerate() {
        writeln!(
            html,
            r#"<div class="line-wrapper"><span class="line-number">{}</span>{}</div>"#,
            i + 1,
            line,
        )
        .unwrap();
    }
    writeln!(html, "</code></pre>").unwrap();

    Ok((atoms::ok(), html))
}

rustler::init!("libglans");
