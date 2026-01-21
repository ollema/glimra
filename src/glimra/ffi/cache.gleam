//// Global value cache using JavaScript Map
////
//// Provides caching for expensive computations that would otherwise
//// be recomputed due to Gleam's immutability.

import gleam/option.{type Option}

/// Get a value from the cache
@external(javascript, "./cache_ffi.mjs", "get")
pub fn get(key: String) -> Option(a)

/// Put a value in the cache
@external(javascript, "./cache_ffi.mjs", "put")
pub fn put(key: String, value: a) -> Nil
