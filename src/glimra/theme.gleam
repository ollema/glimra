import gleam/int
import gleam/string_builder as sb

pub type Color {
  Color(r: Int, g: Int, b: Int)
}

pub type Style {
  Style(color: Color, italic: Bool)
}

pub type Theme {
  Theme(
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

pub fn default_theme() -> Theme {
  Theme(
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
  "." <> class_name <> " { color: " <> color_to_css(style.color) <> "; }\n"
}

pub fn to_css(theme theme: Theme) -> String {
  sb.new()
  |> sb.append("\n/* identifiers */\n")
  |> sb.append(style_to_css("variable", theme.variable))
  |> sb.append(style_to_css("variable.builtin", theme.variable_builtin))
  |> sb.append(style_to_css("variable.member", theme.variable_member))
  |> sb.append(style_to_css(
    "variable.parameter.builtin",
    theme.variable_parameter_builtin,
  ))
  |> sb.append(style_to_css("variable.parameter", theme.variable_parameter))
  |> sb.append(style_to_css("constant", theme.constant))
  |> sb.append(style_to_css("constant.builtin", theme.constant_builtin))
  |> sb.append(style_to_css("constant.macro", theme.constant_macro))
  |> sb.append(style_to_css("module", theme.module))
  |> sb.append(style_to_css("label", theme.label))
  |> sb.append("\n/* literals */\n")
  |> sb.append(style_to_css("string", theme.string))
  |> sb.append(style_to_css("string.documentation", theme.string_documentation))
  |> sb.append(style_to_css("string.escape", theme.string_escape))
  |> sb.append(style_to_css("string.regexp", theme.string_regexp))
  |> sb.append(style_to_css("string.special", theme.string_special))
  |> sb.append(style_to_css("character", theme.character))
  |> sb.append(style_to_css("character.special", theme.character_special))
  |> sb.append(style_to_css("boolean", theme.boolean))
  |> sb.append(style_to_css("number", theme.number))
  |> sb.append("\n/* types */\n")
  |> sb.append(style_to_css("type", theme.type_))
  |> sb.append(style_to_css("type.builtin", theme.type_builtin))
  |> sb.append(style_to_css("type.definition", theme.type_definition))
  |> sb.append(style_to_css("attribute", theme.attribute))
  |> sb.append(style_to_css("attribute.builtin", theme.attribute_builtin))
  |> sb.append(style_to_css("property", theme.property))
  |> sb.append("\n/* functions */\n")
  |> sb.append(style_to_css("function", theme.function))
  |> sb.append(style_to_css("function.builtin", theme.function_builtin))
  |> sb.append(style_to_css("function.call", theme.function_call))
  |> sb.append(style_to_css("function.macro", theme.function_macro))
  |> sb.append(style_to_css("function.method", theme.function_method))
  |> sb.append(style_to_css("function.method.call", theme.function_method_call))
  |> sb.append(style_to_css("constructor", theme.constructor))
  |> sb.append(style_to_css("operator", theme.operator))
  |> sb.append("\n/* keywords */\n")
  |> sb.append(style_to_css("keyword", theme.keyword))
  |> sb.append(style_to_css("keyword.coroutine", theme.keyword_coroutine))
  |> sb.append(style_to_css("keyword.function", theme.keyword_function))
  |> sb.append(style_to_css("keyword.operator", theme.keyword_operator))
  |> sb.append(style_to_css("keyword.import", theme.keyword_import))
  |> sb.append(style_to_css("keyword.type", theme.keyword_type))
  |> sb.append(style_to_css("keyword.modifier", theme.keyword_modifier))
  |> sb.append(style_to_css("keyword.repeat", theme.keyword_repeat))
  |> sb.append(style_to_css("keyword.return", theme.keyword_return))
  |> sb.append(style_to_css("keyword.debug", theme.keyword_debug))
  |> sb.append(style_to_css("keyword.exception", theme.keyword_exception))
  |> sb.append(style_to_css("keyword.conditional", theme.keyword_conditional))
  |> sb.append(style_to_css(
    "keyword.conditional.ternary",
    theme.keyword_conditional_ternary,
  ))
  |> sb.append("\n/* punctuation */\n")
  |> sb.append(style_to_css("punctuation", theme.punctuation))
  |> sb.append(style_to_css("punctuation.bracket", theme.punctuation_bracket))
  |> sb.append(style_to_css(
    "punctuation.delimiter",
    theme.punctuation_delimiter,
  ))
  |> sb.append(style_to_css("punctuation.special", theme.punctuation_special))
  |> sb.append("\n/* comments */\n")
  |> sb.append(style_to_css("comment", theme.comment))
  |> sb.append(style_to_css(
    "comment.documentation",
    theme.comment_documentation,
  ))
  |> sb.append("\n/* tags */\n")
  |> sb.append(style_to_css("tag", theme.tag))
  |> sb.append(style_to_css("tag.builtin", theme.tag_builtin))
  |> sb.append(style_to_css("tag.attribute", theme.tag_attribute))
  |> sb.append(style_to_css("tag.delimiter", theme.tag_delimiter))
  |> sb.to_string()
}
