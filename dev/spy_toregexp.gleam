//// Spy on toRegExp/parse/transform calls during highlighting
////
//// This script runs the same highlighting flow as generate_expected_tokens
//// but with instrumented versions to capture and analyze the options being passed.
////
//// ## Usage
////
////   gleam run -m spy_toregexp
////
//// ## Output
////
//// Results are written to spy_results.json in the project root.

import codegen
import gleam/io

pub fn main() {
  io.println("Spying on toRegExp calls for all languages...")
  io.println("")

  let config_json = codegen.build_config_json()

  case codegen.run_spy_toregexp_calls_ffi(config_json) {
    Ok(output) -> io.println(output)
    Error(error_msg) -> {
      io.println("Error running spy_toregexp_calls.mjs:")
      io.println(error_msg)
      io.println("")
      io.println("Make sure you have Node.js installed and dependencies:")
      io.println("  pnpm install")
    }
  }
}
