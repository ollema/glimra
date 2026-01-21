/// Token types for the Oniguruma regex tokenizer.
/// These types mirror the JS oniguruma-parser library's tokenizer output.
import gleam/option.{type Option}
import glimra/oniguruma_parser/parser/ast_types.{
  type FlagGroupModifiers, type NamedCalloutKind,
}

// ============================================================================
// Token Kind Enums
// ============================================================================

/// Character set kinds as recognized by the tokenizer
pub type TokenCharacterSetKind {
  TokAny
  TokDigit
  TokDot
  TokHex
  TokNewline
  TokPosix
  TokProperty
  TokSpace
  TokTextSegment
  TokWord
}

/// Directive kinds
pub type TokenDirectiveKind {
  TokKeep
  TokFlags
}

/// Group opening kinds
pub type TokenGroupOpenKind {
  AbsenceRepeater
  Atomic
  Capturing
  GroupKind
  LookaheadKind
  LookbehindKind
}

/// Quantifier kinds
pub type TokenQuantifierKind {
  TokGreedy
  TokLazy
  TokPossessive
}

// ============================================================================
// Token Types
// ============================================================================

/// The main token type - a union of all token variants
pub type Token {
  AlternatorToken(raw: String)
  AssertionToken(kind: String, raw: String)
  BackreferenceToken(raw: String)
  CharacterToken(value: Int, raw: String)
  CharacterClassCloseToken(raw: String)
  CharacterClassHyphenToken(raw: String)
  CharacterClassIntersectorToken(raw: String)
  CharacterClassOpenToken(negate: Bool, raw: String)
  CharacterSetToken(
    kind: TokenCharacterSetKind,
    value: Option(String),
    negate: Option(Bool),
    raw: String,
  )
  DirectiveToken(
    kind: TokenDirectiveKind,
    flags: Option(FlagGroupModifiers),
    raw: String,
  )
  GroupCloseToken(raw: String)
  GroupOpenToken(
    kind: TokenGroupOpenKind,
    flags: Option(FlagGroupModifiers),
    name: Option(String),
    number: Option(Int),
    negate: Option(Bool),
    raw: String,
  )
  NamedCalloutToken(
    kind: NamedCalloutKind,
    tag: Option(String),
    arguments: Option(List(String)),
    raw: String,
  )
  QuantifierToken(kind: TokenQuantifierKind, min: Int, max: Int, raw: String)
  SubroutineToken(raw: String)
}

/// Intermediate token for escaped numbers (becomes backref or character)
pub type EscapedNumberToken {
  EscapedNumberToken(in_char_class: Bool, raw: String)
}

/// Either a regular token or an intermediate escaped number token
pub type TokenOrIntermediate {
  RegularToken(Token)
  IntermediateToken(EscapedNumberToken)
}

// ============================================================================
// Flag Properties
// ============================================================================

/// Properties extracted from pattern flags
pub type FlagProperties {
  FlagProperties(
    ignore_case: Bool,
    dot_all: Bool,
    extended: Bool,
    digit_is_ascii: Bool,
    posix_is_ascii: Bool,
    space_is_ascii: Bool,
    word_is_ascii: Bool,
    text_segment_mode: Option(TextSegmentMode),
  )
}

/// Text segment mode for y{g} or y{w} flags
pub type TextSegmentMode {
  GraphemeMode
  WordSegmentMode
}

// ============================================================================
// Tokenizer Result
// ============================================================================

/// Result of tokenizing a pattern
pub type TokenizeResult {
  TokenizeResult(tokens: List(Token), flags: FlagProperties)
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Get the raw string from a token
pub fn token_raw(token: Token) -> String {
  case token {
    AlternatorToken(raw: raw) -> raw
    AssertionToken(raw: raw, ..) -> raw
    BackreferenceToken(raw: raw) -> raw
    CharacterToken(raw: raw, ..) -> raw
    CharacterClassCloseToken(raw: raw) -> raw
    CharacterClassHyphenToken(raw: raw) -> raw
    CharacterClassIntersectorToken(raw: raw) -> raw
    CharacterClassOpenToken(raw: raw, ..) -> raw
    CharacterSetToken(raw: raw, ..) -> raw
    DirectiveToken(raw: raw, ..) -> raw
    GroupCloseToken(raw: raw) -> raw
    GroupOpenToken(raw: raw, ..) -> raw
    NamedCalloutToken(raw: raw, ..) -> raw
    QuantifierToken(raw: raw, ..) -> raw
    SubroutineToken(raw: raw) -> raw
  }
}

/// Create a default flag properties with all flags disabled
pub fn default_flag_properties() -> FlagProperties {
  FlagProperties(
    ignore_case: False,
    dot_all: False,
    extended: False,
    digit_is_ascii: False,
    posix_is_ascii: False,
    space_is_ascii: False,
    word_is_ascii: False,
    text_segment_mode: option.None,
  )
}

/// Convert token character set kind to AST character set kind
pub fn token_cs_kind_to_ast(
  kind: TokenCharacterSetKind,
) -> ast_types.CharacterSetKind {
  case kind {
    TokAny -> ast_types.Any
    TokDigit -> ast_types.Digit
    TokDot -> ast_types.Dot
    TokHex -> ast_types.Hex
    TokNewline -> ast_types.Newline
    TokPosix -> ast_types.Posix
    TokProperty -> ast_types.Property
    TokSpace -> ast_types.Space
    TokTextSegment -> ast_types.TextSegment
    TokWord -> ast_types.Word
  }
}

/// Convert token quantifier kind to AST quantifier kind
pub fn token_q_kind_to_ast(
  kind: TokenQuantifierKind,
) -> ast_types.QuantifierKind {
  case kind {
    TokGreedy -> ast_types.Greedy
    TokLazy -> ast_types.Lazy
    TokPossessive -> ast_types.Possessive
  }
}
