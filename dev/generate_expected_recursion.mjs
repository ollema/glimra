#!/usr/bin/env node
// Generate expected recursion output for regex patterns using JS regex-recursion
//
// This script parses extracted patterns, transforms them, generates them,
// and then runs them through the recursion function to generate expected
// output JSON for testing the Gleam implementation.
//
// Usage:
//   node dev/generate_expected_recursion.mjs '<config-json>'
//
// The config JSON should have the format:
//   { "languages": [{"name": "lua", "file": "lua.json"}, ...] }

import * as fs from 'node:fs'
import * as path from 'node:path'
import { fileURLToPath } from 'node:url'

// Import from the installed oniguruma-parser package
import { parse } from 'oniguruma-parser/parser'
// Import directly from source (transform and generate are not exported from package)
import { transform } from '../js_reference/oniguruma-to-es/src/transform.js'
import { generate } from '../js_reference/oniguruma-to-es/src/generate.js'
import { JsUnicodePropertyMap } from '../js_reference/oniguruma-to-es/src/unicode.js'
// Import recursion from regex-recursion
import { recursion } from 'regex-recursion'

const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const EXPECTED_DIR = path.join(__dirname, '..', 'test', 'snippets')

/**
 * Parse config from command line argument
 */
function parseConfig() {
  // eslint-disable-next-line node/prefer-global/process
  const configArg = process.argv[2]
  if (!configArg) {
    console.error('Error: Config JSON argument required.')
    console.error('Usage: node dev/generate_expected_recursion.mjs \'<config-json>\'')
    // eslint-disable-next-line node/prefer-global/process
    process.exit(1)
  }
  return JSON.parse(configArg)
}

/**
 * Convert recursion output to JSON, handling Map objects
 */
function recursionToJson(recursionResult) {
  return {
    pattern: recursionResult.pattern,
    captureTransfers: Array.from(recursionResult.captureTransfers.entries()),
    hiddenCaptures: recursionResult.hiddenCaptures,
  }
}

/**
 * Parse, transform, generate, and run recursion for a single pattern
 */
function parseTransformGenerateRecursion(pattern) {
  try {
    // Use options that match how oniguruma-to-es/Shiki calls the parser
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

    // Call recursion with the same parameters that oniguruma-to-es uses
    const recursionResult = recursion(generated.pattern, {
      captureTransfers: generated._captureTransfers,
      hiddenCaptures: generated._hiddenCaptures,
      mode: 'external',
    })

    return {
      success: true,
      recursion: recursionToJson(recursionResult),
    }
  }
  catch (err) {
    return {
      success: false,
      error: err.message,
    }
  }
}

/**
 * Process patterns for a single language
 */
function processLanguage(langName) {
  const langDir = path.join(EXPECTED_DIR, langName)
  const patternsFile = path.join(langDir, 'patterns.json')

  if (!fs.existsSync(patternsFile)) {
    console.warn(`  Skipping ${langName}: no patterns.json found`)
    return null
  }

  const patternsData = JSON.parse(fs.readFileSync(patternsFile, 'utf-8'))
  const patterns = patternsData.patterns

  const results = {
    language: langName,
    total: patterns.length,
    successful: 0,
    failed: 0,
    patterns: [],
  }

  for (const pattern of patterns) {
    const result = parseTransformGenerateRecursion(pattern)
    results.patterns.push({
      pattern,
      ...result,
    })
    if (result.success) {
      results.successful++
    }
    else {
      results.failed++
    }
  }

  return results
}

async function main() {
  const config = parseConfig()

  console.log('Generating expected recursion output for patterns...')
  console.log('')

  let totalPatterns = 0
  let totalSuccess = 0
  let totalFailed = 0
  let successCount = 0

  for (const lang of config.languages) {
    const results = processLanguage(lang.name)
    if (results === null) {
      continue
    }

    // Write results file
    const langDir = path.join(EXPECTED_DIR, lang.name)
    const outputPath = path.join(langDir, 'expected_recursion.json')
    fs.writeFileSync(outputPath, JSON.stringify(results, null, 2) + '\n')

    console.log(`  ${lang.name}: ${results.successful}/${results.total} patterns processed successfully`)
    if (results.failed > 0) {
      console.log(`    (${results.failed} failed)`)
    }

    totalPatterns += results.total
    totalSuccess += results.successful
    totalFailed += results.failed
    successCount++
  }

  console.log('')
  console.log(`Done! Generated recursion output for ${successCount} languages.`)
  console.log(`Total: ${totalSuccess}/${totalPatterns} patterns processed successfully`)
  if (totalFailed > 0) {
    console.log(`Failed: ${totalFailed} patterns`)
  }
}

main().catch(console.error)
