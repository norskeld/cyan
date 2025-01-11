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
  Div,
  Mod,
  Mul,
  Sub,
  // Arithmetic operators (compound).
  AddAssign,
  DivAssign,
  ModAssign,
  MulAssign,
  SubAssign,
  // Bitwise operators.
  BitAnd,
  BitNot,
  BitOr,
  BitShl,
  BitShr,
  BitXor,
  // Bitwise operators (compound).
  BitAndAssign,
  BitOrAssign,
  BitShlAssign,
  BitShrAssign,
  BitXorAssign,
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
  Dec,
  Inc,
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
  /// Returns `true` if the token kind is the end of input token.
  pub fn is_eof(&self) -> bool {
    matches!(self, Self::Eof)
  }

  /// Returns `true` if the token kind is a keyword.
  pub fn is_keyword(&self) -> bool {
    matches!(self, Self::IntKw | Self::VoidKw | Self::ReturnKw)
  }

  /// Returns `true` if the token kind is a postfix operator.
  pub fn is_postfix_op(&self) -> bool {
    matches!(self, Self::Inc | Self::Dec)
  }

  /// Returns `true` if the token kind is a unary operator.
  pub fn is_unary_op(&self) -> bool {
    matches!(
      self,
      Self::BitNot | Self::Sub | Self::Bang | Self::Dec | Self::Inc
    )
  }

  /// Returns `true` if the token kind is a binary operator.
  pub fn is_binary_op(&self) -> bool {
    match self {
      // Arithmetic operators.
      | Self::Add
      | Self::Sub
      | Self::Mul
      | Self::Div
      | Self::Mod
      // Bitwise operators.
      | Self::BitAnd
      | Self::BitOr
      | Self::BitShl
      | Self::BitShr
      | Self::BitXor
      // Logical operators.
      | Self::And
      | Self::Equal
      | Self::Greater
      | Self::GreaterEqual
      | Self::Less
      | Self::LessEqual
      | Self::NotEqual
      | Self::Or => true,
      // Assignment operators.
      | op if op.is_assignment_op() => true,
      | _ => false,
    }
  }

  pub fn is_assignment_op(&self) -> bool {
    match self {
      // Assignment operator.
      | Self::Assign
      // Arithmetic operators (compound).
      | Self::AddAssign
      | Self::SubAssign
      | Self::MulAssign
      | Self::DivAssign
      | Self::ModAssign
      // Bitwise operators (compound).
      | Self::BitAndAssign
      | Self::BitOrAssign
      | Self::BitShlAssign
      | Self::BitShrAssign
      | Self::BitXorAssign => true,
      | _ => false,
    }
  }

  /// Returns token kind description.
  pub fn description(&self) -> &str {
    match self {
      // Keywords.
      | Self::IntKw => "the 'int' keyword",
      | Self::ReturnKw => "the 'return' keyword",
      | Self::VoidKw => "the 'void' keyword",
      // Arithmetic operators.
      | Self::Add => "a '+'",
      | Self::Div => "a '/'",
      | Self::Mod => "a '%'",
      | Self::Mul => "a '*'",
      | Self::Sub => "a '-'",
      // Arithmetic operators (compound).
      | Self::AddAssign => "a '+='",
      | Self::DivAssign => "a '/='",
      | Self::ModAssign => "a '%='",
      | Self::MulAssign => "a '*='",
      | Self::SubAssign => "a '-='",
      // Bitwise operators.
      | Self::BitAnd => "a '&'",
      | Self::BitNot => "a '~'",
      | Self::BitOr => "a '|'",
      | Self::BitShl => "a '<<'",
      | Self::BitShr => "a '>>'",
      | Self::BitXor => "a '^'",
      // Bitwise operators (compound).
      | Self::BitAndAssign => "a '&='",
      | Self::BitOrAssign => "a '|='",
      | Self::BitShlAssign => "a '<<='",
      | Self::BitShrAssign => "a '>>='",
      | Self::BitXorAssign => "a '^='",
      // Logical operators.
      | Self::And => "a '&&'",
      | Self::Equal => "a '=='",
      | Self::Greater => "a '>'",
      | Self::GreaterEqual => "a '>='",
      | Self::Less => "a '<'",
      | Self::LessEqual => "a '<='",
      | Self::NotEqual => "a '!='",
      | Self::Or => "a '||'",
      // Other operators.
      | Self::Assign => "a '='",
      | Self::Bang => "a '!'",
      | Self::Dec => "a '--'",
      | Self::Inc => "a '++'",
      // Punctuation.
      | Self::BraceClose => "a '}'",
      | Self::BraceOpen => "a '{'",
      | Self::ParenClose => "a ')'",
      | Self::ParenOpen => "a '('",
      | Self::Semi => "a ';'",
      // Non-terminals.
      | Self::Int => "an integer",
      | Self::Ident => "an identifier",
      | Self::Newline => "a newline",
      | Self::Whitespace => "whitespace",
      // Other.
      | Self::Eof => "the end of input",
      | Self::Invalid => "an invalid token",
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
    self.kind.is_eof()
  }

  /// Returns `true` if the token is a keyword.
  pub fn is_keyword(&self) -> bool {
    self.kind.is_keyword()
  }

  /// Returns `true` if the token is a postfix operator.
  pub fn is_postfix_op(&self) -> bool {
    self.kind.is_postfix_op()
  }

  /// Returns `true` if the token is a unary operator.
  pub fn is_unary_op(&self) -> bool {
    self.kind.is_unary_op()
  }

  /// Returns `true` if the token is a binary operator.
  pub fn is_binary_op(&self) -> bool {
    self.kind.is_binary_op()
  }

  /// Returns `true` if the token is an assignment operator.
  pub fn is_assignment_op(&self) -> bool {
    self.kind.is_assignment_op()
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
