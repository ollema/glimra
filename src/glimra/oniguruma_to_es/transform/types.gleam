/// Shared types for the transform module.
import gleam/dict.{type Dict}
import gleam/option.{type Option}
import glimra/oniguruma_parser/parser/ast_types.{
  type CapturingGroupNode, type RegexNode,
}

// ============================================================================
// Result Types
// ============================================================================

/// The transformed AST with additional metadata
pub type RegexPlusAst {
  RegexPlusAst(
    ast: RegexNode,
    flags: RegexPlusFlags,
    options: TransformOptions,
    origin_map: Dict(Int, Int),
    strategy: Option(Strategy),
  )
}

/// Flags for the Regex+ output
pub type RegexPlusFlags {
  RegexPlusFlags(
    ignore_case: Bool,
    dot_all: Bool,
    global: Bool,
    has_indices: Bool,
    multiline: Bool,
    sticky: Bool,
  )
}

/// Options for Regex+ flag handling
pub type TransformOptions {
  TransformOptions(disable: DisableOptions, force: ForceOptions)
}

/// Options to disable in Regex+
pub type DisableOptions {
  DisableOptions(x: Bool, n: Bool)
}

/// Options to force in Regex+
pub type ForceOptions {
  ForceOptions(v: Bool)
}

/// Strategy for \G emulation
pub type Strategy {
  ClipSearch
}

// ============================================================================
// Configuration Types
// ============================================================================

/// Accuracy level for transformation
pub type Accuracy {
  DefaultAccuracy
  StrictAccuracy
}

/// Target JavaScript version
pub type Target {
  ES2024
  ES2025
}

/// Configuration for the transform function
pub type TransformConfig {
  TransformConfig(
    accuracy: Accuracy,
    ascii_word_boundaries: Bool,
    avoid_subclass: Bool,
    best_effort_target: Target,
  )
}

// ============================================================================
// Subroutine Reference Key
// ============================================================================

/// Key for subroutine reference map (number or name)
pub type SubroutineRefKey {
  NumberedKey(Int)
  NamedKey(String)
}

// ============================================================================
// Group Name Info
// ============================================================================

/// Info about a group's name status
pub type GroupNameInfo {
  GroupNameInfo(node: CapturingGroupNode, has_duplicate_name_to_remove: Bool)
}
