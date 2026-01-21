//// Token types for syntax highlighting output

/// Font style flags for themed tokens
/// These match the vscode-textmate FontStyle values
pub type FontStyle {
  /// No font style (value: 0)
  FontStyleNone
  /// Italic text (value: 1)
  FontStyleItalic
  /// Bold text (value: 2)
  FontStyleBold
  /// Underlined text (value: 4)
  FontStyleUnderline
  /// Strikethrough text (value: 8)
  FontStyleStrikethrough
}

/// Raw theme setting from vscode-textmate
/// Used in explanation feature to show theme matching rules
pub type RawThemeSetting {
  RawThemeSetting(name: String, scope: String, foreground: String)
}

/// Explanation of a single scope in a themed token
pub type ThemedTokenScopeExplanation {
  ThemedTokenScopeExplanation(
    scope_name: String,
    /// Only present when using full explanation (not scopeName-only mode)
    theme_matches: List(RawThemeSetting),
  )
}

/// Explanation of a themed token, showing content and matching scopes
pub type ThemedTokenExplanation {
  ThemedTokenExplanation(
    content: String,
    scopes: List(ThemedTokenScopeExplanation),
  )
}

/// A single themed token with color and optional explanation
pub type ThemedToken {
  ThemedToken(
    /// The text content of the token
    content: String,
    /// The start offset of the token, relative to the input code (0-indexed)
    offset: Int,
    /// 6 or 8 digit hex code representation of the token's color
    color: String,
    /// Font style of token (None/Italic/Bold/Underline/Strikethrough)
    font_style: Int,
    /// Explanation of token scopes and theme matching (when includeExplanation is true)
    explanation: List(ThemedTokenExplanation),
  )
}

/// Create a simple themed token without explanation
pub fn simple_token(content: String, offset: Int) -> ThemedToken {
  ThemedToken(
    content: content,
    offset: offset,
    color: "",
    font_style: 0,
    explanation: [],
  )
}
