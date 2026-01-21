//// Registry - Grammar and Theme management
////
//// This module provides a registry for managing grammars and themes.

import gleam/dict.{type Dict}
import glimra/ffi/onig_scanner.{type OnigScanner}
import glimra/textmate/grammar.{type Grammar}
import glimra/textmate/grammar_compiler
import glimra/textmate/grammar_loader
import glimra/textmate/raw_grammar.{type RawGrammar}
import glimra/textmate/theme.{type Theme}
import glimra/textmate/theme_loader

/// Registry for grammars and themes
pub type Registry {
  Registry(
    /// Compiled grammars by language name
    grammars: Dict(String, Grammar),
    /// Raw grammars by language name (for lazy compilation)
    raw_grammars: Dict(String, RawGrammar),
    /// Raw grammars indexed by scope name (e.g., "source.css" -> RawGrammar)
    /// Used for resolving external grammar references during compilation
    scope_to_raw: Dict(String, RawGrammar),
    /// Loaded themes by name
    themes: Dict(String, Theme),
    /// Factory function for creating OnigScanner instances
    scanner_factory: fn(List(String)) -> OnigScanner,
  )
}

/// Create a new empty registry with a scanner factory
pub fn new(scanner_factory: fn(List(String)) -> OnigScanner) -> Registry {
  Registry(
    grammars: dict.new(),
    raw_grammars: dict.new(),
    scope_to_raw: dict.new(),
    themes: dict.new(),
    scanner_factory: scanner_factory,
  )
}

/// Add a grammar from JSON string
pub fn add_grammar_json(
  registry: Registry,
  lang: String,
  json: String,
) -> Result(Registry, String) {
  case grammar_loader.parse_grammar_json(json) {
    Ok(raw) -> Ok(add_raw_grammar(registry, lang, raw))
    Error(e) -> Error(e)
  }
}

/// Add a raw grammar directly
pub fn add_raw_grammar(
  registry: Registry,
  lang: String,
  raw: RawGrammar,
) -> Registry {
  Registry(
    ..registry,
    raw_grammars: dict.insert(registry.raw_grammars, lang, raw),
    scope_to_raw: dict.insert(registry.scope_to_raw, raw.scope_name, raw),
  )
}

/// Add a theme from JSON string
pub fn add_theme_json(
  registry: Registry,
  name: String,
  json: String,
) -> Result(Registry, String) {
  case theme_loader.parse_theme_json(json) {
    Ok(theme) -> Ok(add_theme(registry, name, theme))
    Error(e) -> Error(e)
  }
}

/// Add a theme directly
pub fn add_theme(registry: Registry, name: String, theme: Theme) -> Registry {
  Registry(..registry, themes: dict.insert(registry.themes, name, theme))
}

/// Get a compiled grammar by language name
/// Compiles the grammar lazily if it exists as a raw grammar
pub fn get_grammar(
  registry: Registry,
  lang: String,
) -> Result(#(Registry, Grammar), String) {
  // Check if already compiled
  case dict.get(registry.grammars, lang) {
    Ok(grammar) -> Ok(#(registry, grammar))
    Error(Nil) -> {
      // Try to compile from raw grammar
      case dict.get(registry.raw_grammars, lang) {
        Ok(raw) -> {
          let grammar =
            grammar_compiler.compile_grammar(
              raw,
              registry.scope_to_raw,
              registry.scanner_factory,
            )
          let new_registry =
            Registry(
              ..registry,
              grammars: dict.insert(registry.grammars, lang, grammar),
            )
          Ok(#(new_registry, grammar))
        }
        Error(Nil) -> Error("Grammar not found: " <> lang)
      }
    }
  }
}

/// Get a theme by name
pub fn get_theme(registry: Registry, name: String) -> Result(Theme, String) {
  case dict.get(registry.themes, name) {
    Ok(theme) -> Ok(theme)
    Error(Nil) -> Error("Theme not found: " <> name)
  }
}

/// Resolve a language alias to its canonical name
/// (Currently returns the input unchanged - aliases not implemented)
pub fn resolve_alias(_registry: Registry, lang: String) -> String {
  lang
}

/// Check if a theme is loaded
pub fn has_theme(registry: Registry, name: String) -> Bool {
  dict.has_key(registry.themes, name)
}
