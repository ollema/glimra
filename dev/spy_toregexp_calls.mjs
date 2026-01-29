#!/usr/bin/env node
// Spy on toRegExp/parse/transform calls during highlighting
//
// Usage: node dev/spy_toregexp_calls.mjs '<config-json>'

import * as fs from 'node:fs'
import * as path from 'node:path'
import sinon from 'sinon'
import { createHighlighterCoreSync } from '@shikijs/core'
import { JavaScriptScanner } from '@shikijs/engine-javascript'
import { toRegExp } from 'oniguruma-to-es'

const PROJECT_ROOT = process.cwd()
const PRIV_DIR = path.join(PROJECT_ROOT, 'priv')
const GRAMMARS_DIR = path.join(PRIV_DIR, 'grammars')
const THEMES_DIR = path.join(PRIV_DIR, 'themes')
const SNIPPETS_DIR = path.join(PROJECT_ROOT, 'test', 'snippets')

const wrappers = {
  toRegExp: (pattern, options) => toRegExp(pattern, options),
}

const SHIKI_DEFAULT_OPTIONS = {
  global: true,
  hasIndices: true,
  lazyCompileLength: 3000,
  rules: {
    allowOrphanBackrefs: true,
    asciiWordBoundaries: true,
    captureGroup: true,
    recursionLimit: 5,
    singleline: true,
  },
}

function createSpyingRegexEngine(spy, engineOptions = {}) {
  const cache = new Map()
  const target = engineOptions.target || 'auto'
  const regexConstructor = (pattern) => {
    const opts = { ...SHIKI_DEFAULT_OPTIONS, target }
    return spy(pattern, opts)
  }
  return {
    createScanner(patterns) {
      return new JavaScriptScanner(patterns, { target, cache, regexConstructor })
    },
    createString(s) {
      return { content: s }
    },
  }
}

function deriveParseAndTransformOptions(opts) {
  return {
    parseOpts: {
      flags: opts.flags || '',
      normalizeUnknownPropertyNames: true,
      rules: {
        captureGroup: opts.rules?.captureGroup ?? false,
        singleline: opts.rules?.singleline ?? false,
      },
      skipBackrefValidation: opts.rules?.allowOrphanBackrefs ?? false,
    },
    transformOpts: {
      accuracy: opts.accuracy || 'default',
      asciiWordBoundaries: opts.rules?.asciiWordBoundaries ?? false,
      avoidSubclass: opts.avoidSubclass ?? false,
      bestEffortTarget: opts.target,
    },
  }
}

function loadGrammar(langConfig) {
  const filepath = path.join(GRAMMARS_DIR, langConfig.file)
  if (!fs.existsSync(filepath))
    return null
  const content = JSON.parse(fs.readFileSync(filepath, 'utf-8'))
  return { ...content, name: langConfig.name, scopeName: content.scopeName }
}

function loadTheme(themeName) {
  const filepath = path.join(THEMES_DIR, `${themeName}.json`)
  if (!fs.existsSync(filepath))
    return null
  const content = JSON.parse(fs.readFileSync(filepath, 'utf-8'))
  return { ...content, name: themeName }
}

function findSnippetFile(langDir) {
  const snippetPath = path.join(langDir, 'snippet.txt')
  return fs.existsSync(snippetPath) ? 'snippet.txt' : null
}

function main() {
  const configJson = process.argv[2]
  if (!configJson) {
    console.error('Usage: node dev/spy_toregexp_calls.mjs \'<config-json>\'')
    process.exit(1)
  }

  const config = JSON.parse(configJson)
  const toRegExpSpy = sinon.spy(wrappers, 'toRegExp')

  const grammars = config.languages.map(loadGrammar).filter(Boolean)
  const themes = config.themes.map(loadTheme).filter(Boolean)

  console.log('Loading vendored grammars and themes...')
  console.log(`Loaded ${grammars.length} grammars, ${themes.length} themes`)
  console.log('Creating highlighter with spying regex engine...')
  console.log('')
  console.log(`Processing ${config.languages.length} languages...`)

  if (grammars.length === 0 || themes.length === 0) {
    console.error('No grammars or themes loaded.')
    process.exit(1)
  }

  const engine = createSpyingRegexEngine(wrappers.toRegExp, { target: 'auto' })
  const highlighter = createHighlighterCoreSync({ themes, langs: grammars, engine })

  const errors = []
  for (const lang of config.languages) {
    const langDir = path.join(SNIPPETS_DIR, lang.name)
    const snippetFile = findSnippetFile(langDir)
    if (!snippetFile)
      continue

    const code = fs.readFileSync(path.join(langDir, snippetFile), 'utf-8')
    for (const theme of themes) {
      try {
        highlighter.codeToTokensBase(code, { lang: lang.name, theme: theme.name })
      }
      catch (err) {
        errors.push({ lang: lang.name, theme: theme.name, error: err.message })
      }
    }
    console.log(`  Processed: ${lang.name}`)
  }

  const toRegExpOptionSets = new Map()
  for (const call of toRegExpSpy.getCalls()) {
    const [pattern, options] = call.args
    const optionsKey = JSON.stringify(options, Object.keys(options || {}).sort())
    if (!toRegExpOptionSets.has(optionsKey)) {
      toRegExpOptionSets.set(optionsKey, {
        options,
        count: 0,
        samples: [],
        derived: deriveParseAndTransformOptions(options),
      })
    }
    const entry = toRegExpOptionSets.get(optionsKey)
    entry.count++
    if (entry.samples.length < 5)
      entry.samples.push(pattern)
  }

  const uniquePatterns = new Set(toRegExpSpy.getCalls().map(c => c.args[0]))
  const analysis = {
    summary: {
      totalCalls: toRegExpSpy.callCount,
      uniquePatterns: uniquePatterns.size,
      uniqueOptionCombinations: toRegExpOptionSets.size,
    },
    optionCombinations: Array.from(toRegExpOptionSets.values()).map(entry => ({
      toRegExpOptions: entry.options,
      derivedParseOptions: entry.derived.parseOpts,
      derivedTransformOptions: entry.derived.transformOpts,
      callCount: entry.count,
      samplePatterns: entry.samples,
    })),
    errors,
  }

  const outputPath = path.join(PROJECT_ROOT, 'spy_results.json')
  fs.writeFileSync(outputPath, JSON.stringify(analysis, null, 2) + '\n')

  console.log('')
  console.log('=== Results ===')
  console.log(`Total toRegExp calls: ${analysis.summary.totalCalls}`)
  console.log(`Unique patterns: ${analysis.summary.uniquePatterns}`)
  console.log(`Unique option combinations: ${analysis.summary.uniqueOptionCombinations}`)

  if (analysis.optionCombinations.length > 0) {
    console.log('')
    console.log('Option combinations found:')
    for (const combo of analysis.optionCombinations) {
      console.log(`  - ${combo.callCount} calls with:`)
      console.log(`    toRegExp options: ${JSON.stringify(combo.toRegExpOptions)}`)
      console.log(`    derived parse options: ${JSON.stringify(combo.derivedParseOptions)}`)
      console.log(`    derived transform options: ${JSON.stringify(combo.derivedTransformOptions)}`)
    }
  }

  if (errors.length > 0) {
    console.log('')
    console.log(`Errors encountered: ${errors.length}`)
    for (const err of errors.slice(0, 5)) {
      console.log(`  - ${err.lang}/${err.theme}: ${err.error}`)
    }
    if (errors.length > 5)
      console.log(`  ... and ${errors.length - 5} more`)
  }

  console.log('')
  console.log(`Full results written to: ${outputPath}`)

  toRegExpSpy.restore()
}

main()
