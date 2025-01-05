use std::fmt;

use cyan_reporting::Location;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
  // Keywords.
  IntKw,
  ReturnKw,
  VoidKw,
  // Arithmetic operators.
  Add,
  Dec,
  Div,
  Inc,
  Mod,
  Mul,
  Sub,
  // Bitwise operators.
  BitAnd,
  BitNot,
  BitOr,
  BitShl,
  BitShr,
  BitXor,
  // Logical operators.
  And,
  Equal,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  NotEqual,
  Or,
  // Other operators.
  Assign,
  Bang,
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
      // Arithmetic operators.
      | TokenKind::Add => "a '+'",
      | TokenKind::Dec => "a '--'",
      | TokenKind::Div => "a '/'",
      | TokenKind::Inc => "a '++'",
      | TokenKind::Mod => "a '%'",
      | TokenKind::Mul => "a '*'",
      | TokenKind::Sub => "a '-'",
      // Bitwise operators.
      | TokenKind::BitAnd => "a '&'",
      | TokenKind::BitNot => "a '~'",
      | TokenKind::BitOr => "a '|'",
      | TokenKind::BitShl => "a '<<'",
      | TokenKind::BitShr => "a '>>'",
      | TokenKind::BitXor => "a '^'",
      // Logical operators.
      | TokenKind::And => "a '&&'",
      | TokenKind::Equal => "a '=='",
      | TokenKind::Greater => "a '>'",
      | TokenKind::GreaterEqual => "a '>='",
      | TokenKind::Less => "a '<'",
      | TokenKind::LessEqual => "a '<='",
      | TokenKind::NotEqual => "a '!='",
      | TokenKind::Or => "a '||'",
      // Other operators.
      | TokenKind::Bang => "a '!'",
      | TokenKind::Assign => "a '='",
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
  pub location: Location,
}

impl Token {
  pub fn new(kind: TokenKind, value: String, location: Location) -> Self {
    Self {
      kind,
      value,
      location,
    }
  }

  /// Returns a token signalling unexpected input. The token contains the invalid character(s).
  pub fn invalid(value: String, location: Location) -> Self {
    Self::new(TokenKind::Invalid, value, location)
  }

  /// Returns a token that signals the end of the input stream. Using it so we don't need to
  /// wrap/unwrap every token in [Option].
  pub fn eof(location: Location) -> Self {
    Self::new(TokenKind::Eof, String::new(), location)
  }

  /// Returns the same token with the given location.
  pub fn with_location(self, location: Location) -> Self {
    Self { location, ..self }
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

  /// Returns `true` if the token is a unary operator.
  pub fn is_unary_operator(&self) -> bool {
    matches!(
      self.kind,
      TokenKind::BitNot | TokenKind::Sub | TokenKind::Bang
    )
  }

  /// Returns `true` if the token is a binary operator.
  pub fn is_binary_operator(&self) -> bool {
    matches!(
      self.kind,
      // Arithmetic operators.
      TokenKind::Add
        | TokenKind::Sub
        | TokenKind::Mul
        | TokenKind::Div
        | TokenKind::Mod
        // Bitwise operators.
        | TokenKind::BitAnd
        | TokenKind::BitOr
        | TokenKind::BitShl
        | TokenKind::BitShr
        | TokenKind::BitXor
        // Logical operators.
        | TokenKind::And
        | TokenKind::Equal
        | TokenKind::Greater
        | TokenKind::GreaterEqual
        | TokenKind::Less
        | TokenKind::LessEqual
        | TokenKind::NotEqual
        | TokenKind::Or
        // Assignment operator.
        | TokenKind::Assign
    )
  }
}

impl Default for Token {
  fn default() -> Self {
    Self::eof(Location::default())
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
