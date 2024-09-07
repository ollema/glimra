//// A module that defines and manages themes for syntax highlighting.
//// 
//// The module provides types and functions for creating and applying color
//// styles to different elements of code, such as variables, functions, keywords,
//// and more. The `Theme` type encapsulates all the styles necessary for
//// syntax highlighting, and the module includes functions to convert these
//// themes into CSS.

import gleam/int

// FFI -------------------------------------------------------------------------

@external(erlang, "libglimra", "is_windows")
fn is_windows() -> Bool

fn newline() -> String {
  case is_windows() {
    True -> "\r\n"
    False -> "\n"
  }
}

// TYPES -----------------------------------------------------------------------

/// Represents an RGB color.
pub type Color {
  Color(r: Int, g: Int, b: Int)
}

/// Represents a style for syntax highlighting.
pub type Style {
  Style(color: Color, italic: Bool)
}

/// Represents a syntax highlighting theme. This includes styles for
/// various code elements such as variables, functions, types, keywords,
/// punctuation, comments, and tags.
pub type Theme {
  Theme(
    background_color: Color,
    padding_x: Float,
    padding_y: Float,
    border_radius: Float,
    // identifiers
    variable: Style,
    variable_builtin: Style,
    variable_member: Style,
    variable_parameter_builtin: Style,
    variable_parameter: Style,
    constant: Style,
    constant_builtin: Style,
    constant_macro: Style,
    module: Style,
    label: Style,
    // literals
    string: Style,
    string_documentation: Style,
    string_escape: Style,
    string_regexp: Style,
    string_special: Style,
    character: Style,
    character_special: Style,
    boolean: Style,
    number: Style,
    // types
    type_: Style,
    type_builtin: Style,
    type_definition: Style,
    attribute: Style,
    attribute_builtin: Style,
    property: Style,
    // functions
    function: Style,
    function_builtin: Style,
    function_call: Style,
    function_macro: Style,
    function_method: Style,
    function_method_call: Style,
    constructor: Style,
    operator: Style,
    // keywords
    keyword: Style,
    keyword_coroutine: Style,
    keyword_function: Style,
    keyword_operator: Style,
    keyword_import: Style,
    keyword_type: Style,
    keyword_modifier: Style,
    keyword_repeat: Style,
    keyword_return: Style,
    keyword_debug: Style,
    keyword_exception: Style,
    keyword_conditional: Style,
    keyword_conditional_ternary: Style,
    // punctuation
    punctuation: Style,
    punctuation_bracket: Style,
    punctuation_delimiter: Style,
    punctuation_special: Style,
    // comments
    comment: Style,
    comment_documentation: Style,
    // tags
    tag: Style,
    tag_builtin: Style,
    tag_attribute: Style,
    tag_delimiter: Style,
  )
}

// THEMES ----------------------------------------------------------------------

/// Returns the default theme for syntax highlighting.
/// 
/// This theme is based on Catppuccin Mocha, a community-driven pastel theme.
/// 
pub fn default_theme() -> Theme {
  Theme(
    background_color: Color(30, 30, 30),
    padding_x: 1.5,
    padding_y: 1.25,
    border_radius: 0.5,
    // identifiers
    variable: Style(color: Color(205, 214, 244), italic: False),
    variable_builtin: Style(color: Color(243, 139, 168), italic: False),
    variable_member: Style(color: Color(205, 214, 244), italic: False),
    variable_parameter_builtin: Style(
      color: Color(235, 160, 172),
      italic: False,
    ),
    variable_parameter: Style(color: Color(235, 160, 172), italic: False),
    constant: Style(color: Color(250, 179, 135), italic: False),
    constant_builtin: Style(color: Color(250, 179, 135), italic: False),
    constant_macro: Style(color: Color(203, 166, 247), italic: False),
    module: Style(color: Color(180, 190, 254), italic: True),
    label: Style(color: Color(116, 199, 236), italic: False),
    // literals
    string: Style(color: Color(166, 227, 161), italic: False),
    string_documentation: Style(color: Color(108, 112, 134), italic: False),
    string_escape: Style(color: Color(245, 194, 231), italic: False),
    string_regexp: Style(color: Color(250, 179, 135), italic: False),
    string_special: Style(color: Color(245, 194, 231), italic: False),
    character: Style(color: Color(148, 226, 213), italic: False),
    character_special: Style(color: Color(245, 194, 231), italic: False),
    boolean: Style(color: Color(250, 179, 135), italic: False),
    number: Style(color: Color(250, 179, 135), italic: False),
    // types
    type_: Style(color: Color(249, 226, 175), italic: False),
    type_builtin: Style(color: Color(249, 226, 175), italic: False),
    type_definition: Style(color: Color(249, 226, 175), italic: False),
    attribute: Style(color: Color(250, 179, 135), italic: False),
    attribute_builtin: Style(color: Color(250, 179, 135), italic: False),
    property: Style(color: Color(180, 190, 254), italic: False),
    // functions
    function: Style(color: Color(137, 180, 250), italic: False),
    function_builtin: Style(color: Color(250, 179, 135), italic: False),
    function_call: Style(color: Color(137, 180, 250), italic: False),
    function_macro: Style(color: Color(148, 226, 213), italic: False),
    function_method: Style(color: Color(137, 180, 250), italic: False),
    function_method_call: Style(color: Color(137, 180, 250), italic: False),
    constructor: Style(color: Color(116, 199, 236), italic: False),
    operator: Style(color: Color(137, 220, 235), italic: False),
    // keywords
    keyword: Style(color: Color(203, 166, 247), italic: False),
    keyword_coroutine: Style(color: Color(203, 166, 247), italic: False),
    keyword_function: Style(color: Color(203, 166, 247), italic: False),
    keyword_operator: Style(color: Color(203, 166, 247), italic: False),
    keyword_import: Style(color: Color(203, 166, 247), italic: False),
    keyword_type: Style(color: Color(203, 166, 247), italic: False),
    keyword_modifier: Style(color: Color(203, 166, 247), italic: False),
    keyword_repeat: Style(color: Color(203, 166, 247), italic: False),
    keyword_return: Style(color: Color(203, 166, 247), italic: False),
    keyword_debug: Style(color: Color(245, 194, 231), italic: False),
    keyword_exception: Style(color: Color(203, 166, 247), italic: False),
    keyword_conditional: Style(color: Color(203, 166, 247), italic: False),
    keyword_conditional_ternary: Style(
      color: Color(203, 166, 247),
      italic: False,
    ),
    // punctuation
    punctuation: Style(color: Color(147, 153, 178), italic: False),
    punctuation_bracket: Style(color: Color(147, 153, 178), italic: False),
    punctuation_delimiter: Style(color: Color(147, 153, 178), italic: False),
    punctuation_special: Style(color: Color(245, 194, 231), italic: False),
    // comments
    comment: Style(color: Color(108, 112, 134), italic: True),
    comment_documentation: Style(color: Color(108, 112, 134), italic: True),
    // tags
    tag: Style(color: Color(203, 166, 247), italic: False),
    tag_builtin: Style(color: Color(203, 166, 247), italic: False),
    tag_attribute: Style(color: Color(148, 226, 213), italic: True),
    tag_delimiter: Style(color: Color(137, 220, 235), italic: False),
  )
}

// HELPERS ---------------------------------------------------------------------

fn color_to_css(color: Color) -> String {
  "rgb("
  <> int.to_string(color.r)
  <> ", "
  <> int.to_string(color.g)
  <> ", "
  <> int.to_string(color.b)
  <> ")"
}

fn style_to_css(class_name: String, style: Style) -> String {
  "."
  <> class_name
  <> " { color: "
  <> color_to_css(style.color)
  <> "; }"
  <> newline()
}

/// Converts a `Theme` into a complete CSS stylesheet as a string.
///
/// This function generates CSS classes for all the different styles included
/// in the theme, ready to be applied to code elements in HTML.
///
pub fn to_css(theme t: Theme) -> String {
  let newline = newline()

  newline
  <> "/* identifiers */"
  <> newline
  <> style_to_css("variable", t.variable)
  <> style_to_css("variable.builtin", t.variable_builtin)
  <> style_to_css("variable.member", t.variable_member)
  <> style_to_css("variable.parameter.builtin", t.variable_parameter_builtin)
  <> style_to_css("variable.parameter", t.variable_parameter)
  <> style_to_css("constant", t.constant)
  <> style_to_css("constant.builtin", t.constant_builtin)
  <> style_to_css("constant.macro", t.constant_macro)
  <> style_to_css("module", t.module)
  <> style_to_css("label", t.label)
  <> newline
  <> "/* literals */"
  <> newline
  <> style_to_css("string", t.string)
  <> style_to_css("string.documentation", t.string_documentation)
  <> style_to_css("string.escape", t.string_escape)
  <> style_to_css("string.regexp", t.string_regexp)
  <> style_to_css("string.special", t.string_special)
  <> style_to_css("character", t.character)
  <> style_to_css("character.special", t.character_special)
  <> style_to_css("boolean", t.boolean)
  <> style_to_css("number", t.number)
  <> newline
  <> "/* types */"
  <> newline
  <> style_to_css("type", t.type_)
  <> style_to_css("type.builtin", t.type_builtin)
  <> style_to_css("type.definition", t.type_definition)
  <> style_to_css("attribute", t.attribute)
  <> style_to_css("attribute.builtin", t.attribute_builtin)
  <> style_to_css("property", t.property)
  <> newline
  <> "/* functions */"
  <> newline
  <> style_to_css("function", t.function)
  <> style_to_css("function.builtin", t.function_builtin)
  <> style_to_css("function.call", t.function_call)
  <> style_to_css("function.macro", t.function_macro)
  <> style_to_css("function.method", t.function_method)
  <> style_to_css("function.method.call", t.function_method_call)
  <> style_to_css("constructor", t.constructor)
  <> style_to_css("operator", t.operator)
  <> newline
  <> "/* keywords */"
  <> newline
  <> style_to_css("keyword", t.keyword)
  <> style_to_css("keyword.coroutine", t.keyword_coroutine)
  <> style_to_css("keyword.function", t.keyword_function)
  <> style_to_css("keyword.operator", t.keyword_operator)
  <> style_to_css("keyword.import", t.keyword_import)
  <> style_to_css("keyword.type", t.keyword_type)
  <> style_to_css("keyword.modifier", t.keyword_modifier)
  <> style_to_css("keyword.repeat", t.keyword_repeat)
  <> style_to_css("keyword.return", t.keyword_return)
  <> style_to_css("keyword.debug", t.keyword_debug)
  <> style_to_css("keyword.exception", t.keyword_exception)
  <> style_to_css("keyword.conditional", t.keyword_conditional)
  <> style_to_css("keyword.conditional.ternary", t.keyword_conditional_ternary)
  <> newline
  <> "/* punctuation */"
  <> newline
  <> style_to_css("punctuation", t.punctuation)
  <> style_to_css("punctuation.bracket", t.punctuation_bracket)
  <> style_to_css("punctuation.delimiter", t.punctuation_delimiter)
  <> style_to_css("punctuation.special", t.punctuation_special)
  <> newline
  <> "/* comments */"
  <> newline
  <> style_to_css("comment", t.comment)
  <> style_to_css("comment.documentation", t.comment_documentation)
  <> newline
  <> "/* tags */"
  <> newline
  <> style_to_css("tag", t.tag)
  <> style_to_css("tag.builtin", t.tag_builtin)
  <> style_to_css("tag.attribute", t.tag_attribute)
  <> style_to_css("tag.delimiter", t.tag_delimiter)
}
