/* eslint-disable jsdoc/require-returns-description */
// FFI implementation for OnigScanner using oniguruma-to-es (pure JavaScript)
// This provides the regex interface needed for TextMate tokenization without WASM.

import { toRegExp } from 'oniguruma-to-es'
import { None, Some } from '../../../gleam_stdlib/gleam/option.mjs'
import { CustomType, toList } from '../../gleam.mjs'

// Maximum value for undefined capture indices
const MAX = 4294967295

// Cache compiled regexes to avoid recompilation
const regexCache = new Map()

// Cache complete scanners by pattern list hash
const scannerCache = new Map()

/**
 * Create a cache key from a list of patterns
 * Uses a simple hash to avoid storing entire pattern strings as keys
 */
function patternsToKey(patternsArray) {
  // Simple but fast string hash
  let hash = 0
  const str = patternsArray.join('\x00')
  for (let i = 0; i < str.length; i++) {
    const char = str.charCodeAt(i)
    hash = ((hash << 5) - hash) + char
    hash = hash & hash // Convert to 32bit integer
  }
  // Also include length to reduce collisions
  return `${hash}:${patternsArray.length}`
}

/**
 * MatchResult type matching the Gleam definition
 */
class MatchResult extends CustomType {
  constructor(index, captures) {
    super()
    this.index = index
    this.captures = captures
  }
}

/**
 * CaptureIndex type matching the Gleam definition
 */
class CaptureIndex extends CustomType {
  constructor(start, end) {
    super()
    this.start = start
    this.end = end
  }
}

/**
 * Compile an Oniguruma pattern to a JavaScript RegExp using oniguruma-to-es
 * @param {string} pattern - The Oniguruma regex pattern
 * @returns {RegExp|null} - The compiled regex, or null if compilation fails
 */
function compilePattern(pattern) {
  // Check cache first
  const cached = regexCache.get(pattern)
  if (cached !== undefined) {
    if (cached instanceof Error) {
      return null
    }
    return cached
  }

  try {
    const regex = toRegExp(pattern, {
      global: true,
      hasIndices: true,
      rules: {
        // Needed since TextMate grammars merge backrefs across patterns
        allowOrphanBackrefs: true,
        // Improves search performance for generated regexes
        asciiWordBoundaries: true,
        // Follow vscode-oniguruma which enables this Oniguruma option by default
        captureGroup: true,
        // Oniguruma uses depth limit 20; lowered for shorter/faster regexes
        recursionLimit: 5,
        // Oniguruma option for ^->\A, $->\Z; improves search performance
        // since TM grammars search line by line
        singleline: true,
      },
    })
    regexCache.set(pattern, regex)
    return regex
  }
  catch (e) {
    // Cache the error to avoid repeated failed compilations
    regexCache.set(pattern, e)
    return null
  }
}

/**
 * Create a new scanner from a list of patterns
 * @param {import('../gleam.mjs').List} patterns - Gleam List of pattern strings
 * @returns {object} Scanner object with patterns and compiled regexps
 */
export function createScanner(patterns) {
  const patternsArray = patterns.toArray()

  // Check scanner cache first
  const cacheKey = patternsToKey(patternsArray)
  const cached = scannerCache.get(cacheKey)
  if (cached !== undefined) {
    // Verify it's actually the same patterns (handle hash collisions)
    if (cached.patterns.length === patternsArray.length
      && cached.patterns.every((p, i) => p === patternsArray[i])) {
      return cached
    }
  }

  // Compile all patterns
  const regexps = patternsArray.map(p => compilePattern(p))
  const scanner = { patterns: patternsArray, regexps }

  // Cache the scanner
  scannerCache.set(cacheKey, scanner)

  return scanner
}

/**
 * Find the next match in the text starting from the given position
 * @param {object} scanner - The scanner created by createScanner
 * @param {string} text - The text to search in
 * @param {number} startPos - The position to start searching from
 * @param {number} _options - Bitmask of FindOption values (not fully implemented)
 * @returns {Some|None} Option(MatchResult)
 */
export function findNextMatchSync(scanner, text, startPos, _options) {
  const { regexps } = scanner
  const pending = [] // [patternIndex, match][]

  /**
   * Convert a RegExpExecArray with indices to MatchResult
   * @param {number} index - Pattern index
   * @param {RegExpExecArray} match - The regex match result
   * @returns {MatchResult}
   */
  function toResult(index, match) {
    const captures = match.indices.map((indice) => {
      if (indice == null) {
        // Undefined capture group - use MAX values
        return new CaptureIndex(MAX, MAX)
      }
      return new CaptureIndex(indice[0], indice[1])
    })
    return new MatchResult(index, toList(captures))
  }

  // Search for matches with each pattern
  for (let i = 0; i < regexps.length; i++) {
    const regexp = regexps[i]
    if (!regexp)
      continue // Skip patterns that failed to compile

    try {
      regexp.lastIndex = startPos
      const match = regexp.exec(text)

      if (!match)
        continue

      // If the match is at the start position, return it immediately
      // (this is an optimization - earliest possible match)
      if (match.index === startPos) {
        return new Some(toResult(i, match))
      }

      // Otherwise, store it for later comparison
      pending.push([i, match])
    }
    catch {
      // Skip patterns that fail during execution
      continue
    }
  }

  // Find the match closest to the start position
  if (pending.length) {
    const minIndex = Math.min(...pending.map(([_, m]) => m.index))
    for (const [i, match] of pending) {
      if (match.index === minIndex) {
        return new Some(toResult(i, match))
      }
    }
  }

  return new None()
}
