// Iterative generation loop to avoid JavaScript stack overflow
// Uses JavaScript while loop instead of Gleam recursion

import { Ok, Error as GleamError } from "../../../gleam.mjs";
import { GenLoopState } from "./types.mjs";
import { toList } from "../../../gleam.mjs";

/**
 * Convert Gleam list to JS array (iteratively)
 */
function listToArray(gleamList) {
  const arr = [];
  let current = gleamList;
  while (current && current.head !== undefined) {
    arr.push(current.head);
    current = current.tail;
  }
  return arr;
}

/**
 * Convert JS array to Gleam list using toList
 */
function arrayToGleamList(arr) {
  return toList(arr);
}

/**
 * Run the generation loop iteratively
 *
 * @param {Object} initial_state - GenLoopState { work, results, state, flags_stack }
 * @param {Function} process_fn - function(item, loop_state) -> Result(GenLoopState, String)
 * @returns Result(#(String, GenerateState), String)
 */
export function run_gen_loop(initial_state, process_fn) {
  // Convert Gleam lists to JS arrays for efficient manipulation
  let work = listToArray(initial_state.work);
  let results = listToArray(initial_state.results);
  let flags_stack = listToArray(initial_state.flags_stack);
  let state = initial_state.state;

  while (work.length > 0) {
    // Pop first work item
    const item = work.shift();

    // Create loop state with Gleam lists for the process function
    const loop_state = new GenLoopState(
      arrayToGleamList(work),
      arrayToGleamList(results),
      state,
      arrayToGleamList(flags_stack)
    );

    // Process the work item
    const result = process_fn(item, loop_state);

    if (!result.isOk()) {
      return result;
    }

    // Extract updated state from result
    const new_loop_state = result[0];

    // Convert back to arrays for next iteration
    work = listToArray(new_loop_state.work);
    results = listToArray(new_loop_state.results);
    flags_stack = listToArray(new_loop_state.flags_stack);
    state = new_loop_state.state;
  }

  // Done - check results
  if (results.length === 0) {
    return new Ok(["", state]);
  }
  if (results.length === 1) {
    return new Ok([results[0], state]);
  }
  return new GleamError("Generation loop ended with multiple results on stack");
}
