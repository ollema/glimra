//// Unicode property map for Oniguruma regex patterns.
//// Contains all Unicode properties supported by Oniguruma 6.9.10.

import gleam/dict.{type Dict}
import gleam/list
import gleam/string

/// Type alias for the Unicode property map
pub type UnicodePropertyMap =
  Dict(String, String)

/// POSIX class names
const posix_class_names: List(String) = [
  "alnum", "alpha", "ascii", "blank", "cntrl", "digit", "graph", "lower",
  "print", "punct", "space", "upper", "word", "xdigit",
]

/// Check if a name is a POSIX class name
pub fn is_posix_class_name(name: String) -> Bool {
  list.contains(posix_class_names, name)
}

/// Generate a Unicode property lookup name: lowercase, without spaces, hyphens, or underscores
pub fn slug(name: String) -> String {
  name
  |> string.replace("-", "")
  |> string.replace("_", "")
  |> string.replace(" ", "")
  |> string.lowercase
}

/// Normalize a Unicode property name (best-effort formatting to official values)
pub fn normalize_property_name(name: String) -> String {
  name
  |> string.trim
  |> string.replace(each: "-", with: "_")
  |> string.replace(each: " ", with: "_")
  |> normalize_case
}

/// Normalize the case of a property name
fn normalize_case(name: String) -> String {
  // Simple normalization: capitalize first letter of each segment
  name
  |> string.split("_")
  |> list.map(capitalize_segment)
  |> string.join("_")
}

fn capitalize_segment(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> string.lowercase(rest)
    Error(_) -> s
  }
}

/// JavaScript-compatible Unicode property map.
/// Maps slugified property names to their original property names as used by JavaScript.
/// This matches the JsUnicodePropertyMap from oniguruma-to-es.
/// Each property name maps to itself (preserving original casing).
pub fn js_unicode_property_map() -> UnicodePropertyMap {
  // Build the map from property names
  // Each name maps from its slugified form to its original form
  js_property_names()
  |> list.map(fn(name) { #(slug(name), name) })
  |> dict.from_list
}

/// All property names in the JS unicode property map.
/// Each name maps to itself (slug(name) -> name).
fn js_property_names() -> List(String) {
  [
    // General categories
    "C",
    "Other",
    "Cc",
    "Control",
    "cntrl",
    "Cf",
    "Format",
    "Cn",
    "Unassigned",
    "Co",
    "Private_Use",
    "Cs",
    "Surrogate",
    "L",
    "Letter",
    "LC",
    "Cased_Letter",
    "Ll",
    "Lowercase_Letter",
    "Lm",
    "Modifier_Letter",
    "Lo",
    "Other_Letter",
    "Lt",
    "Titlecase_Letter",
    "Lu",
    "Uppercase_Letter",
    "M",
    "Mark",
    "Combining_Mark",
    "Mc",
    "Spacing_Mark",
    "Me",
    "Enclosing_Mark",
    "Mn",
    "Nonspacing_Mark",
    "N",
    "Number",
    "Nd",
    "Decimal_Number",
    "digit",
    "Nl",
    "Letter_Number",
    "No",
    "Other_Number",
    "P",
    "Punctuation",
    "punct",
    "Pc",
    "Connector_Punctuation",
    "Pd",
    "Dash_Punctuation",
    "Pe",
    "Close_Punctuation",
    "Pf",
    "Final_Punctuation",
    "Pi",
    "Initial_Punctuation",
    "Po",
    "Other_Punctuation",
    "Ps",
    "Open_Punctuation",
    "S",
    "Symbol",
    "Sc",
    "Currency_Symbol",
    "Sk",
    "Modifier_Symbol",
    "Sm",
    "Math_Symbol",
    "So",
    "Other_Symbol",
    "Z",
    "Separator",
    "Zl",
    "Line_Separator",
    "Zp",
    "Paragraph_Separator",
    "Zs",
    "Space_Separator",
    // Binary properties
    "ASCII",
    "ASCII_Hex_Digit",
    "AHex",
    "Alphabetic",
    "Alpha",
    "Any",
    "Assigned",
    "Bidi_Control",
    "Bidi_C",
    "Bidi_Mirrored",
    "Bidi_M",
    "Case_Ignorable",
    "CI",
    "Cased",
    "Changes_When_Casefolded",
    "CWCF",
    "Changes_When_Casemapped",
    "CWCM",
    "Changes_When_Lowercased",
    "CWL",
    "Changes_When_NFKC_Casefolded",
    "CWKCF",
    "Changes_When_Titlecased",
    "CWT",
    "Changes_When_Uppercased",
    "CWU",
    "Dash",
    "Default_Ignorable_Code_Point",
    "DI",
    "Deprecated",
    "Dep",
    "Diacritic",
    "Dia",
    "Emoji",
    "Emoji_Component",
    "EComp",
    "Emoji_Modifier",
    "EMod",
    "Emoji_Modifier_Base",
    "EBase",
    "Emoji_Presentation",
    "EPres",
    "Extended_Pictographic",
    "ExtPict",
    "Extender",
    "Ext",
    "Grapheme_Base",
    "Gr_Base",
    "Grapheme_Extend",
    "Gr_Ext",
    "Hex_Digit",
    "Hex",
    "IDS_Binary_Operator",
    "IDSB",
    "IDS_Trinary_Operator",
    "IDST",
    "ID_Continue",
    "IDC",
    "ID_Start",
    "IDS",
    "Ideographic",
    "Ideo",
    "Join_Control",
    "Join_C",
    "Logical_Order_Exception",
    "LOE",
    "Lowercase",
    "Lower",
    "Math",
    "Noncharacter_Code_Point",
    "NChar",
    "Pattern_Syntax",
    "Pat_Syn",
    "Pattern_White_Space",
    "Pat_WS",
    "Quotation_Mark",
    "QMark",
    "Radical",
    "Regional_Indicator",
    "RI",
    "Sentence_Terminal",
    "STerm",
    "Soft_Dotted",
    "SD",
    "Terminal_Punctuation",
    "Term",
    "Unified_Ideograph",
    "UIdeo",
    "Uppercase",
    "Upper",
    "Variation_Selector",
    "VS",
    "White_Space",
    "space",
    "XID_Continue",
    "XIDC",
    "XID_Start",
    "XIDS",
  ]
}
