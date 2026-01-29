//// Minimal FFI interface for OnigScanner (regex matching)
////
//// This module provides the minimal FFI needed for TextMate tokenization.
//// Only two external functions are needed: creating a scanner from patterns
//// and finding the next match.
////
//// The OnigScanner uses Oniguruma regex syntax which is required for
//// TextMate grammar compatibility.

import gleam/option.{type Option}
import glimra/textmate/rule.{type CaptureIndex}

/// Opaque type representing an Oniguruma regex scanner.
/// This wraps the vscode-oniguruma OnigScanner class.
pub type OnigScanner

/// Result of a regex match containing the pattern index and capture positions
pub type MatchResult {
  MatchResult(
    /// Index of the pattern that matched (0-indexed into the patterns list)
    index: Int,
    /// List of capture positions (index 0 is the full match, 1+ are capture groups)
    captures: List(CaptureIndex),
  )
}

/// Create a new OnigScanner from a list of regex patterns.
/// Each pattern is compiled and can be matched against text.
/// The index of the matching pattern is returned in MatchResult.
@external(javascript, "./onig_scanner_ffi.mjs", "createScanner")
pub fn create_scanner(patterns: List(String)) -> OnigScanner

/// Find the next match in the text starting from the given position.
/// Returns Some(MatchResult) if a match is found, None otherwise.
///
/// Parameters:
/// - scanner: The OnigScanner to use
/// - text: The text to search in
/// - start_pos: The position to start searching from (byte offset)
/// - options: Bitmask of FindOption values controlling anchor behavior
@external(javascript, "./onig_scanner_ffi.mjs", "findNextMatchSync")
pub fn find_next_match(
  scanner: OnigScanner,
  text: String,
  start_pos: Int,
  options: Int,
) -> Option(MatchResult)
