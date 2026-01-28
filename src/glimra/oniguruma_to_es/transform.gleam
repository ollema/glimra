/// Transform module for converting Oniguruma AST to Regex+ AST.
///
/// Transforms an Oniguruma AST in-place to a Regex+ AST suitable for JavaScript
/// regex generation. The transformation uses a 3-pass visitor system:
///
/// - First Pass: Syntactic transformations (assertions, character sets, POSIX
///   classes, flag directives, \K, \G)
/// - Second Pass: Subroutine expansion, recursion detection, backref multiplexing
///   setup
/// - Third Pass: Group renumbering, backref finalization, orphan backref handling
import gleam/dict.{type Dict}
import gleam/option.{None}
import gleam/set
import glimra/oniguruma_parser/parser/ast_types.{
  type CapturingGroupNode, type RegexNode,
}
import glimra/oniguruma_to_es/transform/first_pass
import glimra/oniguruma_to_es/transform/second_pass
import glimra/oniguruma_to_es/transform/third_pass
import glimra/oniguruma_to_es/transform/types.{
  type Accuracy, type RegexPlusAst, type RegexPlusFlags, type Strategy,
  type Target, type TransformConfig, type TransformOptions, DefaultAccuracy,
  DisableOptions, ES2024, ES2025, ForceOptions, RegexPlusAst, RegexPlusFlags,
  TransformConfig, TransformOptions,
}

// Re-export types for external use
pub type RegexPlusAstExport =
  RegexPlusAst

pub type RegexPlusFlagsExport =
  RegexPlusFlags

pub type TransformOptionsExport =
  TransformOptions

pub type StrategyExport =
  Strategy

pub type TransformConfigExport =
  TransformConfig

pub type AccuracyExport =
  Accuracy

pub type TargetExport =
  Target

// ============================================================================
// Default Configuration
// ============================================================================

/// Create default transform configuration
pub fn default_config() -> TransformConfig {
  TransformConfig(
    accuracy: DefaultAccuracy,
    ascii_word_boundaries: False,
    avoid_subclass: False,
    best_effort_target: ES2025,
  )
}

// ============================================================================
// Main Transform Function
// ============================================================================

/// Transform an Oniguruma AST to a Regex+ AST
pub fn transform(
  ast: RegexNode,
  config: TransformConfig,
) -> Result(RegexPlusAst, String) {
  // First pass: syntactic transformations
  // Initial current_flags: #(dotAll, ignoreCase) from AST flags
  let initial_flags = #(ast.flags.dot_all, ast.flags.ignore_case)
  let first_pass_state =
    first_pass.FirstPassState(
      accuracy: config.accuracy,
      ascii_word_boundaries: config.ascii_word_boundaries,
      avoid_subclass: config.avoid_subclass,
      min_target_es2024: is_min_target_es2024(config.best_effort_target),
      current_flags: initial_flags,
      digit_is_ascii: ast.flags.digit_is_ascii,
      space_is_ascii: ast.flags.space_is_ascii,
      word_is_ascii: ast.flags.word_is_ascii,
      flag_directives_by_alt: dict.new(),
      js_group_name_map: dict.new(),
      passed_lookbehind: False,
      strategy: None,
      subroutine_ref_map: dict.new(),
      supported_g_nodes: [],
    )

  let #(ast_after_first, first_state_result) =
    first_pass.run(ast, first_pass_state)

  // Global flags after first pass
  // sticky is ONLY set if supported_g_nodes is non-empty (native sticky mode)
  // NOT for clip_search strategy (which is a runtime emulation, not native sticky)
  let has_supported_g = first_state_result.supported_g_nodes != []
  let global_flags =
    RegexPlusFlags(
      ignore_case: ast_after_first.flags.ignore_case,
      dot_all: ast_after_first.flags.dot_all,
      global: False,
      has_indices: False,
      multiline: False,
      sticky: has_supported_g,
    )

  // Second pass: subroutine expansion, recursion detection, backref multiplexing
  let second_pass_state =
    second_pass.SecondPassState(
      current_flags: #(global_flags.dot_all, global_flags.ignore_case),
      prev_flags: None,
      global_flags: #(global_flags.dot_all, global_flags.ignore_case),
      group_origin_by_copy: dict.new(),
      groups_by_name: dict.new(),
      multiplex_captures_to_left_by_ref: dict.new(),
      open_refs: dict.new(),
      reffed_nodes_by_referencer: dict.new(),
      subroutine_ref_map: first_state_result.subroutine_ref_map,
    )

  let #(ast_after_second, second_state_result) =
    second_pass.run(ast_after_first, second_pass_state)

  // Third pass: group renumbering, backref finalization
  let third_pass_state =
    third_pass.ThirdPassState(
      emitted_names: set.new(),
      groups_by_name: second_state_result.groups_by_name,
      highest_orphan_backref: 0,
      num_captures_to_left: 0,
      open_groups: [],
      reffed_nodes_by_referencer: second_state_result.reffed_nodes_by_referencer,
    )

  let #(final_ast, _third_state_result) =
    third_pass.run(ast_after_second, third_pass_state)

  // Build the RegexPlusAst result
  let options =
    TransformOptions(
      disable: DisableOptions(x: True, n: True),
      force: ForceOptions(v: True),
    )

  // Convert origin map from CapturingGroupNode to Int (group numbers)
  let origin_map = build_origin_map(second_state_result.group_origin_by_copy)

  Ok(RegexPlusAst(
    ast: final_ast,
    flags: global_flags,
    options: options,
    origin_map: origin_map,
    strategy: first_state_result.strategy,
  ))
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Check if the target is at least ES2024
fn is_min_target_es2024(target: Target) -> Bool {
  case target {
    ES2024 | ES2025 -> True
  }
}

/// Build origin map from capturing group nodes to their numbers
fn build_origin_map(
  group_origin_by_copy: Dict(CapturingGroupNode, CapturingGroupNode),
) -> Dict(Int, Int) {
  dict.fold(group_origin_by_copy, dict.new(), fn(acc, copy, origin) {
    dict.insert(acc, copy.number, origin.number)
  })
}
