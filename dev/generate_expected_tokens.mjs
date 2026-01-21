#!/usr/bin/env node
/* eslint-disable no-console */
// Generate expected token files for cross-validation testing
//
// This script loads grammars from priv/grammars/ and themes from priv/themes/
// to ensure the JS Shiki uses the exact same grammars as our Gleam implementation.
//
// Usage:
//   node dev/generate_expected_tokens.mjs '<config-json>'
//
// The config JSON should have the format:
//   { "languages": [{"name": "lua", "file": "lua.json"}, ...], "themes": ["nord", ...] }

import * as fs from 'node:fs'
import * as path from 'node:path'
import { fileURLToPath } from 'node:url'
import { createHighlighterCoreSync } from '@shikijs/core'
import { createJavaScriptRegexEngine } from '@shikijs/engine-javascript'

const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const PRIV_DIR = path.join(__dirname, '..', 'priv')
const GRAMMARS_DIR = path.join(PRIV_DIR, 'grammars')
const THEMES_DIR = path.join(PRIV_DIR, 'themes')
const SNIPPETS_DIR = path.join(__dirname, '..', 'test', 'snippets')

/**
 * Parse config from command line argument
 */
function parseConfig() {
  // eslint-disable-next-line node/prefer-global/process
  const configArg = process.argv[2]
  if (!configArg) {
    console.error('Error: Config JSON argument required.')
    console.error('Usage: node dev/generate_expected_tokens.mjs \'<config-json>\'')
    // eslint-disable-next-line node/prefer-global/process
    process.exit(1)
  }
  return JSON.parse(configArg)
}

/**
 * Load a grammar from priv/grammars/
 */
function loadGrammar(langConfig) {
  const filepath = path.join(GRAMMARS_DIR, langConfig.file)
  if (!fs.existsSync(filepath)) {
    console.warn(`Grammar not found: ${filepath}`)
    return null
  }

  const content = JSON.parse(fs.readFileSync(filepath, 'utf-8'))

  // Return in Shiki's expected format
  // Spread content first, then override name to ensure our name takes precedence
  return {
    ...content,
    name: langConfig.name,
    scopeName: content.scopeName,
  }
}

/**
 * Load a theme from priv/themes/
 */
function loadTheme(themeName) {
  const filepath = path.join(THEMES_DIR, `${themeName}.json`)
  if (!fs.existsSync(filepath)) {
    console.warn(`Theme not found: ${filepath}`)
    return null
  }

  const content = JSON.parse(fs.readFileSync(filepath, 'utf-8'))

  return {
    ...content,
    name: themeName,
  }
}

/**
 * Find snippet file in a language directory
 */
function findSnippetFile(langDir) {
  if (!fs.existsSync(langDir)) {
    return null
  }

  // Look for snippet.txt specifically
  const snippetPath = path.join(langDir, 'snippet.txt')
  if (fs.existsSync(snippetPath)) {
    return 'snippet.txt'
  }

  return null
}

/**
 * Generate tokens for a single language with various options
 * Returns expected tokens for API option tests
 */
function generateOptionsTokens(highlighter, code, langName, themeName, themeObj) {
  const results = {}

  // Base tokens (no options)
  const baseTokens = highlighter.codeToTokensBase(code, {
    lang: langName,
    theme: themeName,
  })
  results.base = {
    fg: themeObj.fg,
    bg: themeObj.bg,
    tokens: baseTokens.map(line =>
      line.map(t => ({
        content: t.content,
        offset: t.offset,
        color: t.color || '',
        fontStyle: t.fontStyle || 0,
      })),
    ),
  }

  // With color replacements - replace the foreground color with a custom one
  const replacementColor = '#FF0000'
  const colorReplacements = {
    [themeObj.fg.toLowerCase()]: replacementColor,
  }

  const replacedTokens = highlighter.codeToTokensBase(code, {
    lang: langName,
    theme: themeName,
    colorReplacements,
  })
  results.color_replacements = {
    fg: themeObj.fg,
    bg: themeObj.bg,
    replacement_map: colorReplacements,
    tokens: replacedTokens.map(line =>
      line.map(t => ({
        content: t.content,
        offset: t.offset,
        color: t.color || '',
        fontStyle: t.fontStyle || 0,
      })),
    ),
  }

  // With max line length - lines longer than this are not tokenized
  // Use a small value (50) so some lines in the snippet are affected
  const maxLineLength = 50
  const maxLineLengthTokens = highlighter.codeToTokensBase(code, {
    lang: langName,
    theme: themeName,
    tokenizeMaxLineLength: maxLineLength,
  })
  results.max_line_length = {
    fg: themeObj.fg,
    bg: themeObj.bg,
    max_line_length: maxLineLength,
    tokens: maxLineLengthTokens.map(line =>
      line.map(t => ({
        content: t.content,
        offset: t.offset,
        color: t.color || '',
        fontStyle: t.fontStyle || 0,
      })),
    ),
  }

  // With includeExplanation - adds scope information to tokens
  const explanationTokens = highlighter.codeToTokensBase(code, {
    lang: langName,
    theme: themeName,
    includeExplanation: true,
  })
  results.include_explanation = {
    fg: themeObj.fg,
    bg: themeObj.bg,
    tokens: explanationTokens.map(line =>
      line.map(t => ({
        content: t.content,
        offset: t.offset,
        color: t.color || '',
        fontStyle: t.fontStyle || 0,
        // Include explanation data if present
        explanation: t.explanation
          ? t.explanation.map(exp => ({
              content: exp.content,
              scopes: exp.scopes.map(s => ({
                scopeName: s.scopeName,
                themeMatches: s.themeMatches
                  ? s.themeMatches.map(m => ({
                      name: m.name || '',
                      scope: m.scope || '',
                      foreground: m.settings?.foreground || '',
                      fontStyle: m.settings?.fontStyle || 0,
                    }))
                  : [],
              })),
            }))
          : [],
      })),
    ),
  }

  return results
}

async function main() {
  const config = parseConfig()

  console.log('Loading vendored grammars and themes...')

  // Load all grammars
  const grammars = []
  for (const lang of config.languages) {
    const grammar = loadGrammar(lang)
    if (grammar) {
      grammars.push(grammar)
    }
  }

  // Load all themes
  const themes = []
  for (const themeName of config.themes) {
    const theme = loadTheme(themeName)
    if (theme) {
      themes.push(theme)
    }
  }

  console.log(`Loaded ${grammars.length} grammars`)
  console.log(`Loaded ${themes.length} themes`)

  if (grammars.length === 0 || themes.length === 0) {
    console.error('Error: No grammars or themes loaded.')
    console.error('Make sure priv/grammars/ and priv/themes/ contain the vendored files.')
    // eslint-disable-next-line node/prefer-global/process
    process.exit(1)
  }

  // Create highlighter with JavaScript regex engine
  console.log('Creating highlighter with JavaScript regex engine...')
  const engine = createJavaScriptRegexEngine()

  const highlighter = createHighlighterCoreSync({
    themes,
    langs: grammars,
    engine,
  })

  console.log('')
  console.log(`Processing ${config.languages.length} languages...`)
  console.log('')

  let successCount = 0

  for (const lang of config.languages) {
    const langDir = path.join(SNIPPETS_DIR, lang.name)
    const snippetFile = findSnippetFile(langDir)

    if (!snippetFile) {
      console.warn(`  Skipping ${lang.name}: no snippet file found`)
      continue
    }

    const code = fs.readFileSync(path.join(langDir, snippetFile), 'utf-8')

    // Generate tokens for all themes
    const consolidatedTokens = {}

    for (const theme of themes) {
      try {
        const tokens = highlighter.codeToTokensBase(code, {
          lang: lang.name,
          theme: theme.name,
        })
        const themeObj = highlighter.getTheme(theme.name)

        consolidatedTokens[theme.name] = {
          fg: themeObj.fg,
          bg: themeObj.bg,
          tokens: tokens.map(line =>
            line.map(t => ({
              content: t.content,
              offset: t.offset,
              color: t.color || '',
              fontStyle: t.fontStyle || 0,
            })),
          ),
        }
      }
      catch (err) {
        console.error(`  Error with ${lang.name}/${theme.name}: ${err.message}`)
      }
    }

    // Write output
    const outputPath = path.join(langDir, 'expected_tokens.json')
    // eslint-disable-next-line prefer-template
    fs.writeFileSync(outputPath, JSON.stringify(consolidatedTokens, null, 2) + '\n')
    console.log(`  Generated: ${lang.name}/expected_tokens.json`)
    successCount++
  }

  // Generate options test tokens for javascript (as representative language)
  console.log('')
  console.log('Generating API options test tokens for javascript...')
  const jsLangDir = path.join(SNIPPETS_DIR, 'javascript')
  const jsSnippetFile = findSnippetFile(jsLangDir)
  if (jsSnippetFile) {
    const jsCode = fs.readFileSync(path.join(jsLangDir, jsSnippetFile), 'utf-8')
    const optionsTokens = {}

    // Use nord theme for options tests (it's commonly used)
    const testThemeName = 'nord'
    const themeObj = highlighter.getTheme(testThemeName)
    if (themeObj) {
      optionsTokens[testThemeName] = generateOptionsTokens(
        highlighter,
        jsCode,
        'javascript',
        testThemeName,
        themeObj,
      )

      const optionsOutputPath = path.join(jsLangDir, 'expected_tokens_options.json')
      // eslint-disable-next-line prefer-template
      fs.writeFileSync(optionsOutputPath, JSON.stringify(optionsTokens, null, 2) + '\n')
      console.log(`  Generated: javascript/expected_tokens_options.json`)
    }
  }

  console.log('')
  console.log(`Done! Generated ${successCount} expected_tokens.json files.`)
}

main().catch(console.error)
