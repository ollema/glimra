//// Type definitions for the generate module.

import gleam/dict.{type Dict}
import gleam/option.{type Option}
import glimra/oniguruma_parser/parser/ast_types.{
  type AlternativeElement, type CapturingGroupNode, type CharacterClassElement,
}
import glimra/oniguruma_to_es/transform/types.{
  type Accuracy, type DisableOptions, type ForceOptions,
}

// ============================================================================
// Generated Output Types
// ============================================================================

/// The generated output from the generate function
pub type Generated {
  Generated(
    pattern: String,
    flags: String,
    options: GeneratedOptions,
    capture_transfers: Dict(Int, List(Int)),
    hidden_captures: List(Int),
  )
}

/// Options that are output in the generated result
pub type GeneratedOptions {
  GeneratedOptions(
    disable: DisableOptions,
    force: ForceOptions,
    unicode_sets_plugin: Option(Bool),
  )
}

// ============================================================================
// Configuration Types
// ============================================================================

/// Configuration for the generate function
pub type GenerateConfig {
  GenerateConfig(
    accuracy: Accuracy,
    target: Target,
    verbose: Bool,
    recursion_limit: Int,
  )
}

/// Target JavaScript version for generation
pub type Target {
  ES2024
  ES2025
}

// ============================================================================
// State Types
// ============================================================================

/// State maintained during generation
pub type GenerateState {
  GenerateState(
    accuracy: Accuracy,
    applied_global_flags: AppliedFlags,
    capture_map: Dict(Int, CaptureData),
    current_flags: CurrentFlags,
    in_char_class: Bool,
    in_quantifier_body: Bool,
    last_node_was_backref: Bool,
    origin_map: List(#(CapturingGroupNode, CapturingGroupNode)),
    recursion_limit: Int,
    use_applied_ignore_case: Bool,
    use_flag_mods: Bool,
    use_flag_v: Bool,
    verbose: Bool,
  )
}

/// Applied global flags (dotAll, ignoreCase)
pub type AppliedFlags {
  AppliedFlags(dot_all: Bool, ignore_case: Bool)
}

/// Current flags state
pub type CurrentFlags {
  CurrentFlags(dot_all: Bool, ignore_case: Bool)
}

/// Data stored for each capture group
pub type CaptureData {
  CaptureData(ignore_case: Bool, hidden: Bool, transfer_to: Option(Int))
}

// ============================================================================
// Work Item Types (for iterative generation)
// ============================================================================

/// Work item for iterative generation
pub type GenWorkItem {
  /// Process an alternative element and push its result
  ProcessElement(element: AlternativeElement)
  /// Process a character class element
  ProcessCharClassElement(element: CharacterClassElement)
  /// Process the body of a quantifier (which is a QuantifiableNode)
  ProcessQuantifierBody(body: ast_types.QuantifiableNode)

  /// Control flow: join N results with a separator
  JoinResults(count: Int, separator: String)
  /// Control flow: wrap top result with prefix and suffix
  WrapResult(prefix: String, suffix: String)
  /// Control flow: push a literal result directly
  PushResult(value: String)

  /// State management: push current flags and set new flags
  PushFlags(flags: CurrentFlags)
  /// State management: pop flags from stack
  PopFlags
  /// State management: set in_char_class state
  SetInCharClass(value: Bool)
  /// State management: set in_quantifier_body state
  SetInQuantifierBody(value: Bool)
  /// State management: set last_node_was_backref
  SetLastNodeWasBackref(value: Bool)

  /// Capture tracking: record capture metadata
  RecordCapture(number: Int, data: CaptureData)
}

/// Generation loop state
pub type GenLoopState {
  GenLoopState(
    work: List(GenWorkItem),
    results: List(String),
    state: GenerateState,
    flags_stack: List(CurrentFlags),
  )
}
