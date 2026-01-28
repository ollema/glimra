/// FFI module for JSON operations to avoid stack overflow in large AST handling
import gleam/dynamic/decode

@external(javascript, "./json_compare_ffi.mjs", "compare_json")
pub fn compare_json(a: String, b: String) -> Bool

/// Convert a dynamic value to a JSON string
/// This uses JavaScript's native JSON.stringify to avoid stack overflow
@external(javascript, "./json_compare_ffi.mjs", "stringify_dynamic")
pub fn stringify_dynamic(value: decode.Dynamic) -> String
