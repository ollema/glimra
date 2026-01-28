#!/usr/bin/env node
// Generate expected RegexPlusAst for regex patterns using JS oniguruma-to-es transform
//
// This script parses extracted patterns and runs them through the transform function
// to generate expected RegexPlusAst JSON for testing the Gleam implementation.
//
// Usage:
//   node dev/generate_expected_regex_plus_ast.mjs '<config-json>'
//
// The config JSON should have the format:
//   { "languages": [{"name": "lua", "file": "lua.json"}, ...] }

import * as fs from 'node:fs'
import * as path from 'node:path'
import { fileURLToPath } from 'node:url'

// Import from the installed oniguruma-parser package
import { parse } from 'oniguruma-parser/parser'
// Import directly from source (transform is not exported from package)
import { transform } from '../js_reference/oniguruma-to-es/src/transform.js'
import { JsUnicodePropertyMap } from '../js_reference/oniguruma-to-es/src/unicode.js'

const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const EXPECTED_AST_DIR = path.join(__dirname, '..', 'test', 'snippets')

/**
 * Parse config from command line argument
 */
function parseConfig() {
  // eslint-disable-next-line node/prefer-global/process
  const configArg = process.argv[2]
  if (!configArg) {
    console.error('Error: Config JSON argument required.')
    console.error('Usage: node dev/generate_expected_regex_plus_ast.mjs \'<config-json>\'')
    // eslint-disable-next-line node/prefer-global/process
    process.exit(1)
  }
  return JSON.parse(configArg)
}

/**
 * Recursively convert a RegexPlusAst node to a plain JSON object
 * This handles Map objects and includes underscore properties
 */
function regexPlusAstToJson(node) {
  if (node === null || node === undefined) {
    return null
  }
  if (typeof node === 'string' || typeof node === 'number' || typeof node === 'boolean') {
    return node
  }
  if (Array.isArray(node)) {
    return node.map(regexPlusAstToJson)
  }

  // Handle Map objects (like _originMap)
  if (node instanceof Map) {
    return Array.from(node.entries()).map(([k, v]) => [
      regexPlusAstToJson(k),
      regexPlusAstToJson(v),
    ])
  }

  if (typeof node === 'object') {
    const result = {}
    for (const [key, value] of Object.entries(node)) {
      // Skip 'parent' property to avoid circular references
      // (transform adds parent to all nodes for traversal)
      if (key === 'parent') {
        continue
      }
      // Include underscore properties (_originMap, _strategy) - don't skip them
      result[key] = regexPlusAstToJson(value)
    }
    return result
  }
  return node
}

/**
 * Parse and transform a single pattern, returning the RegexPlusAst or error
 */
function parseAndTransform(pattern) {
  try {
    // Use options that match how oniguruma-to-es calls the parser
    const ast = parse(pattern, {
      flags: '', // TODO: is it a fair assumption that no flags are used?
      rules: {
        captureGroup: true,
        singleline: true,
      },
      skipBackrefValidation: true,
      unicodePropertyMap: JsUnicodePropertyMap,
    })

    const regexPlusAst = transform(ast, {
      accuracy: 'default',
      asciiWordBoundaries: false,
      avoidSubclass: false,
      bestEffortTarget: 'ES2025',
    })

    return {
      success: true,
      regexPlusAst: regexPlusAstToJson(regexPlusAst),
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
  const langDir = path.join(EXPECTED_AST_DIR, langName)
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
    const result = parseAndTransform(pattern)
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

  console.log('Generating expected RegexPlusAst for patterns...')
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
    const langDir = path.join(EXPECTED_AST_DIR, lang.name)
    const outputPath = path.join(langDir, 'expected_regex_plus_ast.json')
    fs.writeFileSync(outputPath, JSON.stringify(results, null, 2) + '\n')

    console.log(`  ${lang.name}: ${results.successful}/${results.total} patterns transformed successfully`)
    if (results.failed > 0) {
      console.log(`    (${results.failed} failed)`)
    }

    totalPatterns += results.total
    totalSuccess += results.successful
    totalFailed += results.failed
    successCount++
  }

  console.log('')
  console.log(`Done! Generated RegexPlusAst for ${successCount} languages.`)
  console.log(`Total: ${totalSuccess}/${totalPatterns} patterns transformed successfully`)
  if (totalFailed > 0) {
    console.log(`Failed: ${totalFailed} patterns`)
  }
}

main().catch(console.error)
