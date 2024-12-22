use std::fmt;

use crate::span::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
  // Keywords.
  IntKw,
  VoidKw,
  ReturnKw,

  // Operators.
  BitwiseNot,
  Decrement,
  Negate,

  // Punctuation.
  ParenOpen,
  ParenClose,
  BraceOpen,
  BraceClose,
  Semicolon,

  // Non-terminals.
  Constant,
  Identifier,
  Newline,
  Whitespace,

  // Other.
  Invalid,
  Eof,
}

impl TokenKind {
  pub fn description(&self) -> &str {
    match self {
      // Keywords.
      | TokenKind::IntKw => "the 'int' keyword",
      | TokenKind::VoidKw => "the 'void' keyword",
      | TokenKind::ReturnKw => "the 'return' keyword",

      // Operators.
      | TokenKind::BitwiseNot => "a bitwise not operator",
      | TokenKind::Decrement => "a decrement operator",
      | TokenKind::Negate => "a negate operator",

      // Punctuation.
      | TokenKind::ParenOpen => "a '('",
      | TokenKind::ParenClose => "a ')'",
      | TokenKind::BraceOpen => "a '{'",
      | TokenKind::BraceClose => "a '}'",
      | TokenKind::Semicolon => "a ';'",

      // Non-terminals.
      | TokenKind::Constant => "a constant",
      | TokenKind::Identifier => "an identifier",
      | TokenKind::Newline => "a newline",
      | TokenKind::Whitespace => "whitespace",

      // Other.
      | TokenKind::Invalid => "an invalid token",
      | TokenKind::Eof => "the end of input",
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
  pub kind: TokenKind,
  pub value: String,
  pub span: Span,
}

impl Token {
  pub fn new(kind: TokenKind, value: String, span: Span) -> Self {
    Self { kind, value, span }
  }

  /// Returns a token signalling unexpected input. The token contains the invalid character(s).
  pub fn invalid(value: String, span: Span) -> Self {
    Self::new(TokenKind::Invalid, value, span)
  }

  /// Returns a token that signals the end of the input stream. Using it so we don't need to
  /// wrap/unwrap every token in [Option].
  pub fn eof(span: Span) -> Self {
    Self::new(TokenKind::Eof, String::new(), span)
  }

  /// Returns `true` if the token is the end of input token.
  pub fn is_eof(&self) -> bool {
    self.kind == TokenKind::Eof
  }

  /// Returns `true` if the token is a keyword.
  pub fn is_keyword(&self) -> bool {
    matches!(
      self.kind,
      TokenKind::IntKw | TokenKind::VoidKw | TokenKind::ReturnKw
    )
  }
}

impl Default for Token {
  fn default() -> Self {
    Self::eof(Span::default())
  }
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Token({:?}", self.kind)?;

    if matches!(self.kind, TokenKind::Constant | TokenKind::Identifier) {
      write!(f, ", \"{}\"", self.value)?;
    }

    if matches!(self.kind, TokenKind::Whitespace) {
      write!(f, ", {}", self.value.len())?;
    }

    write!(f, ")")
  }
}
