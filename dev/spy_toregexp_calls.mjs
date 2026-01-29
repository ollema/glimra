#!/usr/bin/env node
// Spy on toRegExp/parse/transform/recursion calls during highlighting
//
// Usage: node dev/spy_toregexp_calls.mjs '<config-json>'

import * as fs from 'node:fs'
import * as path from 'node:path'
import sinon from 'sinon'
import { createHighlighterCoreSync } from '@shikijs/core'
import { JavaScriptScanner } from '@shikijs/engine-javascript'
import { toRegExp } from 'oniguruma-to-es'
import { recursion } from 'regex-recursion'

// Import from oniguruma-parser and oniguruma-to-es source for manual pipeline
import { parse } from 'oniguruma-parser/parser'
import { transform } from '../js_reference/oniguruma-to-es/src/transform.js'
import { generate } from '../js_reference/oniguruma-to-es/src/generate.js'
import { JsUnicodePropertyMap } from '../js_reference/oniguruma-to-es/src/unicode.js'

const PROJECT_ROOT = process.cwd()
const PRIV_DIR = path.join(PROJECT_ROOT, 'priv')
const GRAMMARS_DIR = path.join(PRIV_DIR, 'grammars')
const THEMES_DIR = path.join(PRIV_DIR, 'themes')
const SNIPPETS_DIR = path.join(PROJECT_ROOT, 'test', 'snippets')

const wrappers = {
  toRegExp: (pattern, options) => toRegExp(pattern, options),
  recursion: (pattern, options) => recursion(pattern, options),
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

function deriveGenerateOptions(opts) {
  return {
    accuracy: opts.accuracy || 'default',
    target: opts.target,
    verbose: opts.verbose ?? false,
    rules: {
      recursionLimit: opts.rules?.recursionLimit ?? 20,
    },
  }
}

/**
 * Process a pattern through parse -> transform -> generate -> recursion
 * and return recursion stats
 */
function analyzeRecursionForPattern(pattern) {
  try {
    const ast = parse(pattern, {
      flags: '',
      normalizeUnknownPropertyNames: true,
      rules: {
        captureGroup: true,
        singleline: true,
      },
      skipBackrefValidation: true,
      unicodePropertyMap: JsUnicodePropertyMap,
    })

    const regexPlusAst = transform(ast, {
      accuracy: 'default',
      asciiWordBoundaries: true,
      avoidSubclass: false,
      bestEffortTarget: 'ES2025',
    })

    const generated = generate(regexPlusAst, {
      accuracy: 'default',
      target: 'ES2025',
      verbose: false,
      rules: {
        recursionLimit: 5,
      },
    })

    // Track what recursion will receive
    const recursionOptions = {
      captureTransfers: generated._captureTransfers,
      hiddenCaptures: generated._hiddenCaptures,
      mode: 'external',
    }

    // Call recursion with the same parameters that oniguruma-to-es uses
    const recursionResult = recursion(generated.pattern, recursionOptions)

    return {
      success: true,
      mode: recursionOptions.mode,
      captureTransfersSize: generated._captureTransfers.size,
      hiddenCapturesLength: generated._hiddenCaptures.length,
      inputPattern: generated.pattern,
      outputPattern: recursionResult,
    }
  }
  catch (err) {
    return {
      success: false,
      error: err.message,
    }
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

  // Analyze recursion for all unique patterns
  console.log('')
  console.log('Analyzing recursion for unique patterns...')
  const recursionStats = {
    totalCalls: 0,
    successful: 0,
    failed: 0,
    modeValues: {},
    captureTransfersCounts: { empty: 0, nonEmpty: 0 },
    hiddenCapturesCounts: { empty: 0, nonEmpty: 0 },
    samplePatterns: [],
    sampleErrors: [],
  }

  for (const pattern of uniquePatterns) {
    const result = analyzeRecursionForPattern(pattern)
    recursionStats.totalCalls++

    if (result.success) {
      recursionStats.successful++

      // Track mode values
      recursionStats.modeValues[result.mode] = (recursionStats.modeValues[result.mode] || 0) + 1

      // Track captureTransfers counts
      if (result.captureTransfersSize === 0) {
        recursionStats.captureTransfersCounts.empty++
      }
      else {
        recursionStats.captureTransfersCounts.nonEmpty++
      }

      // Track hiddenCaptures counts
      if (result.hiddenCapturesLength === 0) {
        recursionStats.hiddenCapturesCounts.empty++
      }
      else {
        recursionStats.hiddenCapturesCounts.nonEmpty++
      }

      // Sample patterns (store first 5)
      if (recursionStats.samplePatterns.length < 5) {
        recursionStats.samplePatterns.push({
          input: result.inputPattern,
          output: result.outputPattern,
          captureTransfersSize: result.captureTransfersSize,
          hiddenCapturesLength: result.hiddenCapturesLength,
        })
      }
    }
    else {
      recursionStats.failed++
      if (recursionStats.sampleErrors.length < 5) {
        recursionStats.sampleErrors.push({
          pattern,
          error: result.error,
        })
      }
    }
  }

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
      derivedGenerateOptions: deriveGenerateOptions(entry.options),
      callCount: entry.count,
      samplePatterns: entry.samples,
    })),
    recursion: recursionStats,
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

  console.log('')
  console.log('=== Recursion Analysis ===')
  console.log(`Total patterns analyzed: ${recursionStats.totalCalls}`)
  console.log(`Successful: ${recursionStats.successful}, Failed: ${recursionStats.failed}`)
  console.log(`Mode values: ${JSON.stringify(recursionStats.modeValues)}`)
  console.log(`CaptureTransfers: empty=${recursionStats.captureTransfersCounts.empty}, nonEmpty=${recursionStats.captureTransfersCounts.nonEmpty}`)
  console.log(`HiddenCaptures: empty=${recursionStats.hiddenCapturesCounts.empty}, nonEmpty=${recursionStats.hiddenCapturesCounts.nonEmpty}`)

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
