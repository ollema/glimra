#!/usr/bin/env node
// Generate expected AST for regex patterns using JS oniguruma-parser
//
// This script parses extracted patterns and generates expected AST JSON
// for testing the Gleam oniguruma-parser implementation.
//
// Usage:
//   node dev/generate_expected_ast.mjs '<config-json>'
//
// The config JSON should have the format:
//   { "languages": [{"name": "lua", "file": "lua.json"}, ...] }

import * as fs from 'node:fs'
import * as path from 'node:path'
import { fileURLToPath } from 'node:url'

// Import from the installed oniguruma-parser package
import { parse } from 'oniguruma-parser/parser'
import { OnigUnicodePropertyMap } from 'oniguruma-parser/unicode'

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
    console.error('Usage: node dev/generate_expected_ast.mjs \'<config-json>\'')
    // eslint-disable-next-line node/prefer-global/process
    process.exit(1)
  }
  return JSON.parse(configArg)
}

/**
 * Recursively convert an AST node to a plain JSON object
 * This handles the conversion of special values and ensures consistent output
 */
function astToJson(node) {
  if (node === null || node === undefined) {
    return null
  }
  if (typeof node === 'string' || typeof node === 'number' || typeof node === 'boolean') {
    return node
  }
  if (Array.isArray(node)) {
    return node.map(astToJson)
  }
  if (typeof node === 'object') {
    const result = {}
    for (const [key, value] of Object.entries(node)) {
      // Skip internal properties that start with underscore
      if (key.startsWith('_')) {
        continue
      }
      result[key] = astToJson(value)
    }
    return result
  }
  return node
}

/**
 * Parse a single pattern and return the AST or error
 */
function parsePattern(pattern) {
  try {
    // Use options that match how oniguruma-to-es calls the parser
    const ast = parse(pattern, {
      flags: '',
      rules: {
        captureGroup: true,
        singleline: true,
      },
      skipBackrefValidation: true,
      unicodePropertyMap: OnigUnicodePropertyMap,
    })
    return {
      success: true,
      ast: astToJson(ast),
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
    const result = parsePattern(pattern)
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

  console.log('Generating expected AST for patterns...')
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
    const outputPath = path.join(langDir, 'expected_ast.json')
    fs.writeFileSync(outputPath, JSON.stringify(results, null, 2) + '\n')

    console.log(`  ${lang.name}: ${results.successful}/${results.total} patterns parsed successfully`)
    if (results.failed > 0) {
      console.log(`    (${results.failed} failed)`)
    }

    totalPatterns += results.total
    totalSuccess += results.successful
    totalFailed += results.failed
    successCount++
  }

  console.log('')
  console.log(`Done! Generated AST for ${successCount} languages.`)
  console.log(`Total: ${totalSuccess}/${totalPatterns} patterns parsed successfully`)
  if (totalFailed > 0) {
    console.log(`Failed: ${totalFailed} patterns`)
  }
}

main().catch(console.error)
