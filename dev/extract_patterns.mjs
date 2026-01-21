#!/usr/bin/env node
// Extract regex patterns from TextMate grammar JSON files
//
// This script extracts all patterns from grammar files for testing
// the Gleam oniguruma-parser implementation.
//
// Usage:
//   node dev/extract_patterns.mjs '<config-json>'
//
// The config JSON should have the format:
//   { "languages": [{"name": "lua", "file": "lua.json"}, ...] }

import * as fs from 'node:fs'
import * as path from 'node:path'
import { fileURLToPath } from 'node:url'

const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const PRIV_DIR = path.join(__dirname, '..', 'priv')
const GRAMMARS_DIR = path.join(PRIV_DIR, 'grammars')
const OUTPUT_DIR = path.join(__dirname, '..', 'test', 'snippets')

/**
 * Parse config from command line argument
 */
function parseConfig() {
  // eslint-disable-next-line node/prefer-global/process
  const configArg = process.argv[2]
  if (!configArg) {
    console.error('Error: Config JSON argument required.')
    console.error('Usage: node dev/extract_patterns.mjs \'<config-json>\'')
    // eslint-disable-next-line node/prefer-global/process
    process.exit(1)
  }
  return JSON.parse(configArg)
}

/**
 * Recursively extract patterns from a grammar object
 */
function extractPatterns(obj, patterns = new Set()) {
  if (!obj || typeof obj !== 'object') {
    return patterns
  }

  // Extract regex pattern fields
  const patternFields = ['match', 'begin', 'end', 'while']
  for (const field of patternFields) {
    if (typeof obj[field] === 'string' && obj[field].trim().length > 0) {
      patterns.add(obj[field])
    }
  }

  // Recurse into nested structures
  if (Array.isArray(obj)) {
    for (const item of obj) {
      extractPatterns(item, patterns)
    }
  }
  else {
    for (const value of Object.values(obj)) {
      if (typeof value === 'object') {
        extractPatterns(value, patterns)
      }
    }
  }

  return patterns
}

/**
 * Load a grammar and extract its patterns
 */
function loadAndExtractPatterns(langConfig) {
  const filepath = path.join(GRAMMARS_DIR, langConfig.file)
  if (!fs.existsSync(filepath)) {
    console.warn(`Grammar not found: ${filepath}`)
    return null
  }

  try {
    const content = JSON.parse(fs.readFileSync(filepath, 'utf-8'))
    const patterns = extractPatterns(content)
    return Array.from(patterns).sort()
  }
  catch (err) {
    console.error(`Error processing ${langConfig.file}: ${err.message}`)
    return null
  }
}

async function main() {
  const config = parseConfig()

  console.log('Extracting patterns from grammars...')
  console.log('')

  // Ensure output directory exists
  fs.mkdirSync(OUTPUT_DIR, { recursive: true })

  let totalPatterns = 0
  let successCount = 0

  for (const lang of config.languages) {
    const patterns = loadAndExtractPatterns(lang)
    if (patterns === null) {
      continue
    }

    // Create language output directory
    const langDir = path.join(OUTPUT_DIR, lang.name)
    fs.mkdirSync(langDir, { recursive: true })

    // Write patterns file
    const outputPath = path.join(langDir, 'patterns.json')
    const output = {
      language: lang.name,
      grammar_file: lang.file,
      pattern_count: patterns.length,
      patterns,
    }
    fs.writeFileSync(outputPath, JSON.stringify(output, null, 2) + '\n')

    console.log(`  ${lang.name}: ${patterns.length} patterns`)
    totalPatterns += patterns.length
    successCount++
  }

  console.log('')
  console.log(`Done! Extracted patterns from ${successCount} languages.`)
  console.log(`Total unique patterns: ${totalPatterns}`)
}

main().catch(console.error)
