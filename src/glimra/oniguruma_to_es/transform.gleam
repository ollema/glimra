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
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import glimra/oniguruma_parser/parser/ast_types.{
  type AlternativeElement, type AlternativeNode, type CapturingGroupNode,
  type CharacterClassElement, type QuantifiableNode, type RegexNode,
  AbsenceFunctionE, AbsenceFunctionQ, BackreferenceQ, CapturingGroupE,
  CapturingGroupQ, CharacterCCE, CharacterClassCCE, CharacterClassE,
  CharacterClassQ, CharacterClassRangeCCE, CharacterQ, CharacterSetCCE,
  CharacterSetQ, GroupE, GroupQ, LookaroundAssertionE, QuantifierE, QuantifierQ,
  SubroutineQ,
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
import glimra/oniguruma_to_es/transform/utils.{new_clone_state_from}

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
      next_transform_id: 1,
      passed_lookbehind: False,
      strategy: None,
      subroutine_ref_map: dict.new(),
      subroutine_target_numbers: set.new(),
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
  // Initialize clone_state with the next ID from first pass to avoid conflicts
  let second_pass_state =
    second_pass.SecondPassState(
      current_flags: #(global_flags.dot_all, global_flags.ignore_case),
      prev_flags: None,
      global_flags: #(global_flags.dot_all, global_flags.ignore_case),
      clone_state: new_clone_state_from(first_state_result.next_transform_id),
      groups_by_name: dict.new(),
      multiplex_captures_to_left_by_ref: dict.new(),
      open_refs: dict.new(),
      reffed_nodes_by_referencer: dict.new(),
      subroutine_ref_map: first_state_result.subroutine_ref_map,
      subroutine_target_numbers: first_state_result.subroutine_target_numbers,
    )

  let #(ast_after_second, second_state_result) =
    second_pass.run(ast_after_first, second_pass_state)

  // Third pass: group renumbering, backref finalization
  // Pass clone_state for ID-based origin tracking
  let third_pass_state =
    third_pass.ThirdPassState(
      emitted_names: set.new(),
      groups_by_name: second_state_result.groups_by_name,
      clone_state: second_state_result.clone_state,
      highest_orphan_backref: 0,
      num_captures_to_left: 0,
      open_groups: [],
      reffed_nodes_by_referencer: second_state_result.reffed_nodes_by_referencer,
      captures_in_path: [],
    )

  let #(final_ast, _third_state_result) =
    third_pass.run(ast_after_second, third_pass_state)

  // Build the RegexPlusAst result
  let options =
    TransformOptions(
      disable: DisableOptions(x: True, n: True),
      force: ForceOptions(v: True),
    )

  // Build origin_map from clone_state.origin_entries
  // Each entry is (clone_node, origin_id) where:
  // - clone_node is stored at cloning time (will be looked up for transformed version)
  // - origin_id is used to look up origin from final AST
  //
  // Both clone and origin are looked up from final_ast (after third pass) because:
  // - In JS, subroutine refs get updated during third pass (mutable nodes)
  // - In Gleam, we need to look up the final version of both nodes
  //
  // For non-recursive clones: they stay in the AST and get transformed by third pass
  //   (name removed as duplicate) - use the final AST version for clone
  // For recursive clones: they become SubroutineE markers and are NOT in the final AST
  //   - use the stored clone_node which preserves the name
  let groups_by_id_final = collect_capturing_groups_by_id(final_ast)

  // origin_entries are already in correct order (using append during cloning)
  let origin_map =
    list.filter_map(second_state_result.clone_state.origin_entries, fn(entry) {
      let #(clone_node, origin_id) = entry
      // Look up origin from final AST to get updated subroutine refs
      case dict.get(groups_by_id_final, origin_id) {
        Ok(origin_node) -> {
          // Look up clone from final AST if available (non-recursive case)
          // If not found, use stored clone (recursive case - became SubroutineE)
          let final_clone = case clone_node.transform_id {
            Some(clone_id) ->
              case dict.get(groups_by_id_final, clone_id) {
                Ok(transformed_clone) -> transformed_clone
                Error(_) -> clone_node
              }
            None -> clone_node
          }
          Ok(#(final_clone, origin_node))
        }
        Error(_) -> Error(Nil)
      }
    })

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

/// Collect all CapturingGroups from an AST indexed by their transform_id
fn collect_capturing_groups_by_id(
  ast: RegexNode,
) -> dict.Dict(Int, CapturingGroupNode) {
  collect_from_alternatives(ast.body, dict.new())
}

fn collect_from_alternatives(
  alts: List(AlternativeNode),
  acc: dict.Dict(Int, CapturingGroupNode),
) -> dict.Dict(Int, CapturingGroupNode) {
  list.fold(alts, acc, fn(a, alt) { collect_from_elements(alt.body, a) })
}

fn collect_from_elements(
  elements: List(AlternativeElement),
  acc: dict.Dict(Int, CapturingGroupNode),
) -> dict.Dict(Int, CapturingGroupNode) {
  list.fold(elements, acc, fn(a, elem) { collect_from_element(elem, a) })
}

fn collect_from_element(
  elem: AlternativeElement,
  acc: dict.Dict(Int, CapturingGroupNode),
) -> dict.Dict(Int, CapturingGroupNode) {
  case elem {
    CapturingGroupE(node) -> {
      let new_acc = case node.transform_id {
        Some(id) -> dict.insert(acc, id, node)
        None -> acc
      }
      collect_from_alternatives(node.body, new_acc)
    }
    GroupE(node) -> collect_from_alternatives(node.body, acc)
    LookaroundAssertionE(node) -> collect_from_alternatives(node.body, acc)
    AbsenceFunctionE(node) -> collect_from_alternatives(node.body, acc)
    QuantifierE(node) -> collect_from_quantifiable(node.body, acc)
    CharacterClassE(node) -> collect_from_cc_elements(node.body, acc)
    _ -> acc
  }
}

fn collect_from_quantifiable(
  node: QuantifiableNode,
  acc: dict.Dict(Int, CapturingGroupNode),
) -> dict.Dict(Int, CapturingGroupNode) {
  case node {
    CapturingGroupQ(cg) -> {
      let new_acc = case cg.transform_id {
        Some(id) -> dict.insert(acc, id, cg)
        None -> acc
      }
      collect_from_alternatives(cg.body, new_acc)
    }
    GroupQ(g) -> collect_from_alternatives(g.body, acc)
    AbsenceFunctionQ(af) -> collect_from_alternatives(af.body, acc)
    QuantifierQ(q) -> collect_from_quantifiable(q.body, acc)
    CharacterClassQ(cc) -> collect_from_cc_elements(cc.body, acc)
    CharacterQ(_) | CharacterSetQ(_) | BackreferenceQ(_) | SubroutineQ(_) -> acc
  }
}

fn collect_from_cc_elements(
  elements: List(CharacterClassElement),
  acc: dict.Dict(Int, CapturingGroupNode),
) -> dict.Dict(Int, CapturingGroupNode) {
  list.fold(elements, acc, fn(a, elem) {
    case elem {
      CharacterClassCCE(node) -> collect_from_cc_elements(node.body, a)
      CharacterCCE(_) | CharacterClassRangeCCE(_) | CharacterSetCCE(_) -> a
    }
  })
}
