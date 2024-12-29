use std::fmt;

use crate::span::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
  // Keywords.
  IntKw,
  ReturnKw,
  VoidKw,

  // Operators.
  Add,
  BitAnd,
  BitNot,
  BitOr,
  BitShl,
  BitShr,
  BitXor,
  Dec,
  Div,
  Inc,
  Mod,
  Mul,
  Sub,

  // Punctuation.
  BraceClose,
  BraceOpen,
  ParenClose,
  ParenOpen,
  Semi,

  // Non-terminals.
  Int,
  Ident,
  Newline,
  Whitespace,

  // Other.
  Eof,
  Invalid,
}

impl TokenKind {
  pub fn description(&self) -> &str {
    match self {
      // Keywords.
      | TokenKind::IntKw => "the 'int' keyword",
      | TokenKind::ReturnKw => "the 'return' keyword",
      | TokenKind::VoidKw => "the 'void' keyword",

      // Operators.
      | TokenKind::Add => "a '+'",
      | TokenKind::BitAnd => "a '&'",
      | TokenKind::BitNot => "a '~'",
      | TokenKind::BitOr => "a '|'",
      | TokenKind::BitShl => "a '<<'",
      | TokenKind::BitShr => "a '>>'",
      | TokenKind::BitXor => "a '^'",
      | TokenKind::Dec => "a '--'",
      | TokenKind::Div => "a '/'",
      | TokenKind::Inc => "a '++'",
      | TokenKind::Mod => "a '%'",
      | TokenKind::Mul => "a '*'",
      | TokenKind::Sub => "a '-'",

      // Punctuation.
      | TokenKind::BraceClose => "a '}'",
      | TokenKind::BraceOpen => "a '{'",
      | TokenKind::ParenClose => "a ')'",
      | TokenKind::ParenOpen => "a '('",
      | TokenKind::Semi => "a ';'",

      // Non-terminals.
      | TokenKind::Int => "an integer",
      | TokenKind::Ident => "an identifier",
      | TokenKind::Newline => "a newline",
      | TokenKind::Whitespace => "whitespace",

      // Other.
      | TokenKind::Eof => "the end of input",
      | TokenKind::Invalid => "an invalid token",
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

  /// Returns `true` if the token is a binary operator.
  pub fn is_binary_operator(&self) -> bool {
    matches!(
      self.kind,
      TokenKind::Add | TokenKind::Sub | TokenKind::Mul | TokenKind::Div | TokenKind::Mod
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

    if matches!(self.kind, TokenKind::Int | TokenKind::Ident) {
      write!(f, ", \"{}\"", self.value)?;
    }

    if matches!(self.kind, TokenKind::Whitespace) {
      write!(f, ", {}", self.value.len())?;
    }

    write!(f, ")")
  }
}
