// FFI for caching Gleam values using JavaScript Map
// This provides global caching that persists across Gleam function calls

import { None, Some } from '../../../gleam_stdlib/gleam/option.mjs'

// Global cache for arbitrary Gleam values
const cache = new Map()

/**
 * Get a value from the cache
 * @param {string} key - Cache key
 * @returns {Some|None} Option containing the cached value
 */
export function get(key) {
  const cached = cache.get(key)
  if (cached !== undefined) {
    return new Some(cached)
  }
  return new None()
}

/**
 * Put a value in the cache
 * @param {string} key - Cache key
 * @param {*} value - Value to cache
 */
export function put(key, value) {
  cache.set(key, value)
}
