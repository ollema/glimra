//// Generate module for converting a Regex+ AST to a regex pattern string.
////
//// This is the third step in the pipeline: `parse` -> `transform` -> `generate`.
////
//// Takes a `RegexPlusAst` (from transform) and `GenerateConfig` and produces
//// a `Generated` result containing the pattern, flags, and metadata.

import gleam/dict
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import glimra/oniguruma_parser/parser/ast_types.{type RegexNode}
import glimra/oniguruma_to_es/generate/handlers
import glimra/oniguruma_to_es/generate/types.{
  type CaptureData, type GenLoopState, type GenWorkItem, type GenerateConfig,
  type GenerateState, type Generated, type GeneratedOptions, type Target,
  AppliedFlags, CurrentFlags, ES2024, ES2025, GenLoopState, GenerateConfig,
  GenerateState, Generated, GeneratedOptions, JoinResults, PopFlags,
  ProcessCharClassElement, ProcessElement, ProcessQuantifierBody, PushFlags,
  PushResult, RecordCapture, SetInCharClass, SetInIntersection,
  SetInQuantifierBody, SetLastNodeWasBackref, WrapResult,
}
import glimra/oniguruma_to_es/transform/types as transform_types

// ============================================================================
// Public API
// ============================================================================

/// Generate a Regex+ compatible pattern, flags, and options from a Regex+ AST.
pub fn generate(
  ast: transform_types.RegexPlusAst,
  config: GenerateConfig,
) -> Result(Generated, String) {
  // Validate recursion limit
  case config.recursion_limit >= 2 && config.recursion_limit <= 20 {
    False -> Error("Invalid recursionLimit; use 2-20")
    True -> do_generate(ast, config)
  }
}

/// Default configuration for generate
pub fn default_config() -> GenerateConfig {
  GenerateConfig(
    accuracy: transform_types.DefaultAccuracy,
    target: ES2025,
    verbose: False,
    recursion_limit: 5,
  )
}

// ============================================================================
// Internal Implementation
// ============================================================================

fn do_generate(
  ast: transform_types.RegexPlusAst,
  config: GenerateConfig,
) -> Result(Generated, String) {
  let min_target_es2024 = is_min_target_es2024(config.target)
  let min_target_es2025 = is_min_target_es2025(config.target)

  // For ES2025+, we can use flag groups, so no need for the flag modifier pre-pass
  // Since Shiki always uses ES2025, we skip the pre-pass complexity
  let applied_global_flags =
    AppliedFlags(dot_all: ast.flags.dot_all, ignore_case: ast.flags.ignore_case)

  // Initialize state
  let state =
    GenerateState(
      accuracy: config.accuracy,
      applied_global_flags: applied_global_flags,
      capture_map: dict.new(),
      current_flags: CurrentFlags(
        dot_all: ast.flags.dot_all,
        ignore_case: ast.flags.ignore_case,
      ),
      in_char_class: False,
      in_intersection: False,
      in_quantifier_body: False,
      last_node_was_backref: False,
      origin_map: ast.origin_map,
      recursion_limit: config.recursion_limit,
      // For ES2025, we don't need case expansion
      use_applied_ignore_case: False,
      use_flag_mods: min_target_es2025,
      use_flag_v: min_target_es2024,
      verbose: config.verbose,
    )

  // Generate pattern from AST body (list of alternatives)
  use #(pattern, final_state) <- result.try(generate_pattern(ast.ast, state))

  // Generate flags string
  let flags = generate_flags(ast.flags, final_state)

  // Build options (modify for < ES2024)
  let options = build_options(ast.options, min_target_es2024)

  // Build capture_transfers and hidden_captures from capture_map
  let #(capture_transfers, hidden_captures) =
    build_capture_metadata(final_state.capture_map)

  Ok(Generated(
    pattern: pattern,
    flags: flags,
    options: options,
    capture_transfers: capture_transfers,
    hidden_captures: hidden_captures,
  ))
}

fn is_min_target_es2024(target: Target) -> Bool {
  case target {
    ES2024 -> True
    ES2025 -> True
  }
}

fn is_min_target_es2025(target: Target) -> Bool {
  case target {
    ES2024 -> False
    ES2025 -> True
  }
}

// ============================================================================
// Pattern Generation
// ============================================================================

fn generate_pattern(
  ast: RegexNode,
  state: GenerateState,
) -> Result(#(String, GenerateState), String) {
  let alts = ast.body
  let count = list.length(alts)

  case count {
    0 -> Ok(#("", state))
    _ -> {
      // Build work items for all alternatives
      // Work items are processed in order (first item processed first)
      // So we need: elements first, then control items (JoinResults)
      let alt_work_items =
        alts
        |> list.map(fn(alt) {
          let el_count = list.length(alt.body)
          let el_items =
            alt.body
            |> list.map(fn(el) { ProcessElement(el) })
          // Elements first, then join
          list.append(el_items, [JoinResults(el_count, "")])
        })
        |> list.flatten

      // Elements first, then top-level join
      let work = list.append(alt_work_items, [JoinResults(count, "|")])

      // Run the generation loop
      let loop_state =
        GenLoopState(work: work, results: [], state: state, flags_stack: [])

      gen_loop(loop_state)
    }
  }
}

// ============================================================================
// Generation Loop (JavaScript FFI for stack overflow prevention)
// ============================================================================

/// External FFI function to run the loop iteratively in JavaScript
@external(javascript, "./generate/loop_ffi.mjs", "run_gen_loop")
fn run_gen_loop(
  initial_state: GenLoopState,
  process_fn: fn(GenWorkItem, GenLoopState) -> Result(GenLoopState, String),
) -> Result(#(String, GenerateState), String)

fn gen_loop(
  loop_state: GenLoopState,
) -> Result(#(String, GenerateState), String) {
  run_gen_loop(loop_state, process_work_item)
}

fn process_work_item(
  item: GenWorkItem,
  loop_state: GenLoopState,
) -> Result(GenLoopState, String) {
  case item {
    ProcessElement(element) -> {
      use #(new_items, new_state) <- result.try(handlers.push_element(
        element,
        loop_state.state,
      ))
      // Prepend new items to work stack
      Ok(
        GenLoopState(
          ..loop_state,
          work: list.append(new_items, loop_state.work),
          state: new_state,
        ),
      )
    }

    ProcessCharClassElement(element) -> {
      use #(new_items, new_state) <- result.try(handlers.push_cc_element(
        element,
        loop_state.state,
      ))
      Ok(
        GenLoopState(
          ..loop_state,
          work: list.append(new_items, loop_state.work),
          state: new_state,
        ),
      )
    }

    ProcessQuantifierBody(body) -> {
      use #(new_items, new_state) <- result.try(handlers.push_quantifiable(
        body,
        loop_state.state,
      ))
      Ok(
        GenLoopState(
          ..loop_state,
          work: list.append(new_items, loop_state.work),
          state: new_state,
        ),
      )
    }

    PushResult(value) -> {
      Ok(GenLoopState(..loop_state, results: [value, ..loop_state.results]))
    }

    JoinResults(count, separator) -> {
      let #(to_join, rest) = list.split(loop_state.results, count)
      let joined = string.join(list.reverse(to_join), separator)
      Ok(GenLoopState(..loop_state, results: [joined, ..rest]))
    }

    WrapResult(prefix, suffix) -> {
      case loop_state.results {
        [top, ..rest] -> {
          let wrapped = prefix <> top <> suffix
          Ok(GenLoopState(..loop_state, results: [wrapped, ..rest]))
        }
        [] -> Error("WrapResult: no result on stack")
      }
    }

    PushFlags(flags) -> {
      let new_stack = [loop_state.state.current_flags, ..loop_state.flags_stack]
      let new_state = GenerateState(..loop_state.state, current_flags: flags)
      Ok(GenLoopState(..loop_state, state: new_state, flags_stack: new_stack))
    }

    PopFlags -> {
      case loop_state.flags_stack {
        [old_flags, ..rest] -> {
          let new_state =
            GenerateState(..loop_state.state, current_flags: old_flags)
          Ok(GenLoopState(..loop_state, state: new_state, flags_stack: rest))
        }
        [] -> Error("PopFlags: flags stack is empty")
      }
    }

    SetInCharClass(value) -> {
      let new_state = GenerateState(..loop_state.state, in_char_class: value)
      Ok(GenLoopState(..loop_state, state: new_state))
    }

    SetInIntersection(value) -> {
      let new_state = GenerateState(..loop_state.state, in_intersection: value)
      Ok(GenLoopState(..loop_state, state: new_state))
    }

    SetInQuantifierBody(value) -> {
      let new_state =
        GenerateState(..loop_state.state, in_quantifier_body: value)
      Ok(GenLoopState(..loop_state, state: new_state))
    }

    SetLastNodeWasBackref(value) -> {
      let new_state =
        GenerateState(..loop_state.state, last_node_was_backref: value)
      Ok(GenLoopState(..loop_state, state: new_state))
    }

    RecordCapture(number, data) -> {
      let new_map = dict.insert(loop_state.state.capture_map, number, data)
      let new_state = GenerateState(..loop_state.state, capture_map: new_map)
      Ok(GenLoopState(..loop_state, state: new_state))
    }
  }
}

// ============================================================================
// Flags Generation
// ============================================================================

fn generate_flags(
  flags: transform_types.RegexPlusFlags,
  state: GenerateState,
) -> String {
  let i = case state.applied_global_flags.ignore_case {
    True -> "i"
    False -> ""
  }
  let s = case flags.dot_all {
    True -> "s"
    False -> ""
  }
  let y = case flags.sticky {
    True -> "y"
    False -> ""
  }
  i <> s <> y
}

// ============================================================================
// Options Building
// ============================================================================

fn build_options(
  transform_opts: transform_types.TransformOptions,
  min_target_es2024: Bool,
) -> GeneratedOptions {
  case min_target_es2024 {
    True ->
      GeneratedOptions(
        disable: transform_opts.disable,
        force: transform_opts.force,
        unicode_sets_plugin: option.None,
      )
    False ->
      // For pre-ES2024, switch from flag v to u
      GeneratedOptions(
        disable: transform_types.DisableOptions(
          x: transform_opts.disable.x,
          n: transform_opts.disable.n,
        ),
        force: transform_types.ForceOptions(v: False),
        unicode_sets_plugin: option.Some(False),
      )
  }
}

// ============================================================================
// Capture Metadata Building
// ============================================================================

fn build_capture_metadata(
  capture_map: dict.Dict(Int, CaptureData),
) -> #(dict.Dict(Int, List(Int)), List(Int)) {
  let entries = dict.to_list(capture_map)

  let hidden_captures =
    entries
    |> list.filter_map(fn(entry) {
      let #(key, data) = entry
      case data.hidden {
        True -> Ok(key)
        False -> Error(Nil)
      }
    })
    |> list.sort(int.compare)

  let capture_transfers =
    entries
    |> list.filter_map(fn(entry) {
      let #(key, data) = entry
      case data.transfer_to {
        option.Some(target) -> Ok(#(target, key))
        option.None -> Error(Nil)
      }
    })
    |> list.fold(dict.new(), fn(acc, pair) {
      let #(target, source) = pair
      let existing = dict.get(acc, target) |> result.unwrap([])
      dict.insert(acc, target, [source, ..existing])
    })
    // Sort each value list
    |> dict.map_values(fn(_key, values) { list.sort(values, int.compare) })

  #(capture_transfers, hidden_captures)
}
