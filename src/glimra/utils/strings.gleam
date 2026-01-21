//// String manipulation utilities

import gleam/list
import gleam/string

/// A line with its content and offset in the original string
pub type Line =
  #(String, Int)

/// Split a string into lines, each line without the line ending.
/// Returns a list of tuples containing (line content, offset index).
///
/// ## Examples
///
/// ```gleam
/// split_lines("hello\nworld")
/// // => [#("hello", 0), #("world", 6)]
///
/// split_lines("")
/// // => [#("", 0)]
/// ```
pub fn split_lines(code: String) -> List(Line) {
  split_lines_impl(code, 0, "", [])
  |> list.reverse
}

fn split_lines_impl(
  remaining: String,
  offset: Int,
  current_line: String,
  acc: List(Line),
) -> List(Line) {
  case string.pop_grapheme(remaining) {
    // End of string - emit final line
    Error(Nil) -> [#(current_line, offset - string.length(current_line)), ..acc]

    // CRLF (treated as single grapheme by pop_grapheme)
    Ok(#("\r\n", rest)) -> {
      let line_start = offset - string.length(current_line)
      split_lines_impl(rest, offset + 2, "", [
        #(current_line, line_start),
        ..acc
      ])
    }

    // CR only
    Ok(#("\r", rest)) -> {
      let line_start = offset - string.length(current_line)
      split_lines_impl(rest, offset + 1, "", [
        #(current_line, line_start),
        ..acc
      ])
    }

    // LF only
    Ok(#("\n", rest)) -> {
      let line_start = offset - string.length(current_line)
      split_lines_impl(rest, offset + 1, "", [
        #(current_line, line_start),
        ..acc
      ])
    }

    // Regular character - append to current line
    Ok(#(char, rest)) -> {
      split_lines_impl(
        rest,
        offset + string.length(char),
        current_line <> char,
        acc,
      )
    }
  }
}
