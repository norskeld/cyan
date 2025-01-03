use cyan_reporting::{Location, Span};

use super::ascii::*;
use super::token::{Token, TokenKind};

pub struct Lexer<'i> {
  /// The stream of bytes to process.
  input: &'i [u8],
  /// The current position in the input stream.
  position: usize,
  /// Input length, i.e. the maximum position in the input stream.
  length: usize,
  /// The current line number.
  line: usize,
  /// The current column number.
  column: usize,
}

impl<'i> Lexer<'i> {
  /// Creates a new lexer for the given input.
  pub fn new(input: &'i [u8]) -> Self {
    let length = input.len();

    Self {
      input,
      position: 0,
      length,
      line: 1,
      column: 1,
    }
  }

  /// Lexes the input and returns a vector of tokens.
  pub fn lex(&mut self) -> Vec<Token> {
    let mut tokens = Vec::new();

    loop {
      let token = self.next();
      let kind = token.kind;

      tokens.push(token);

      if kind == TokenKind::Eof {
        break;
      }
    }

    tokens
  }

  /// Returns `true` if there is another byte to process.
  #[inline]
  fn has_next(&self) -> bool {
    self.position < self.length
  }

  /// Returns the byte at the given offset from the current position.
  fn peek(&self, offset: usize) -> u8 {
    let index = self.position + offset;

    if index < self.length {
      self.input[index]
    } else {
      0
    }
  }

  /// Returns the current byte.
  fn current_byte(&self) -> u8 {
    if self.has_next() {
      self.input[self.position]
    } else {
      0
    }
  }

  /// Advances the column by the length of the given string.
  #[inline]
  fn advance_column(&mut self, value: &str) {
    self.column += value.len();
  }

  /// Advances the column and the current position by one.
  #[inline]
  fn advance_char(&mut self) {
    self.column += 1;
    self.position += 1;
  }

  /// Returns a string slice of the given range.
  fn slice(&mut self, start: usize, stop: usize) -> String {
    String::from_utf8_lossy(&self.input[start..stop]).into_owned()
  }

  /// Returns a location for the given range.
  fn location(&self, start_offset: usize, start_line: usize, start_column: usize) -> Location {
    Location::new(
      Span::new(start_offset, self.position + 1),
      Span::new(start_line, self.line),
      Span::new(start_column, self.column),
    )
  }

  /// Returns a token with the given kind, value, and location.
  fn token_with_column(
    &mut self,
    kind: TokenKind,
    start: usize,
    line: usize,
    column: usize,
  ) -> Token {
    let value = self.slice(start, self.position);

    self.advance_column(&value);

    let location = self.location(start, line, column);

    Token::new(kind, value, location)
  }

  /// Returns a token with the given kind, offset and line.
  fn token(&mut self, kind: TokenKind, start: usize, line: usize) -> Token {
    self.token_with_column(kind, start, line, self.column)
  }

  /// Returns a token with the given kind and the current position.
  fn token_single(&mut self, kind: TokenKind) -> Token {
    let start = self.position;
    let line = self.line;

    self.position += 1;

    self.token(kind, start, line)
  }
}

impl Lexer<'_> {
  /// Returns the next token.
  pub fn next(&mut self) -> Token {
    match self.current_byte() {
      | ZERO..=NINE => self.int(),
      | TILDE => self.tilde(),
      | PIPE => self.pipe(),
      | CARET => self.caret(),
      | AMPERSAND => self.ampersand(),
      | LESS => self.less(),
      | GREATER => self.greater(),
      | BANG => self.bang(),
      | EQUAL => self.equal(),
      | HYPHEN => self.hyphen(),
      | PLUS => self.plus(),
      | SLASH => self.slash(),
      | STAR => self.star(),
      | PERCENT => self.percent(),
      | SEMICOLON => self.semicolon(),
      | BRACE_OPEN => self.brace_open(),
      | BRACE_CLOSE => self.brace_close(),
      | PAREN_OPEN => self.paren_open(),
      | PAREN_CLOSE => self.paren_close(),
      | UNDERSCORE => self.underscore(),
      | SPACE | TAB => self.whitespace(),
      | NEWLINE | CARRIAGE_RETURN => self.newline(),
      | LOWER_A..=LOWER_Z | UPPER_A..=UPPER_Z => self.identifier(self.position),
      | _ => {
        if self.has_next() {
          self.invalid(self.position, self.position + 1)
        } else {
          self.eof()
        }
      },
    }
  }

  /// Returns a token signalling invalid input.
  fn invalid(&mut self, start: usize, stop: usize) -> Token {
    let column = self.column;
    let value = self.slice(start, stop);

    self.advance_column(&value);

    let location = self.location(start, self.line, column);

    self.position = self.length;

    Token::invalid(value, location)
  }

  /// Returns a token that signals the end of the input stream.
  fn eof(&self) -> Token {
    let offsets = Span::new(self.position, self.position);
    let lines = Span::new(self.line, self.line);

    let location = if self.column == 1 {
      Location::new(offsets, lines, Span::new(1, 1))
    } else {
      Location::new(offsets, lines, Span::new(self.column, self.column))
    };

    Token::eof(location)
  }

  /// Returns a token for a `;`.
  fn semicolon(&mut self) -> Token {
    self.token_single(TokenKind::Semi)
  }

  /// Returns a token for a `{`.
  fn brace_open(&mut self) -> Token {
    self.token_single(TokenKind::BraceOpen)
  }

  /// Returns a token for a `}`.
  fn brace_close(&mut self) -> Token {
    self.token_single(TokenKind::BraceClose)
  }

  /// Returns a token for a `(`.
  fn paren_open(&mut self) -> Token {
    self.token_single(TokenKind::ParenOpen)
  }

  /// Returns a token for a `)`.
  fn paren_close(&mut self) -> Token {
    self.token_single(TokenKind::ParenClose)
  }

  /// Returns a token for a `~` (bitwise not).
  fn tilde(&mut self) -> Token {
    self.token_single(TokenKind::BitNot)
  }

  /// Returns a token for a `|` (bitwise or).
  fn pipe(&mut self) -> Token {
    let start = self.position;
    let line = self.line;

    let kind = match self.peek(1) {
      | PIPE => {
        self.position += 2;
        TokenKind::Or
      },
      | _ => {
        self.position += 1;
        TokenKind::BitOr
      },
    };

    self.token(kind, start, line)
  }

  /// Returns a token for a `^` (bitwise xor).
  fn caret(&mut self) -> Token {
    self.token_single(TokenKind::BitXor)
  }

  /// Returns a token for a `&` (bitwise and).
  fn ampersand(&mut self) -> Token {
    let start = self.position;
    let line = self.line;

    let kind = match self.peek(1) {
      | AMPERSAND => {
        self.position += 2;
        TokenKind::And
      },
      | _ => {
        self.position += 1;
        TokenKind::BitAnd
      },
    };

    self.token(kind, start, line)
  }

  /// Returns a token for one of: a `<<` (bitwise shift left), a `<=`, or a `<`.
  fn less(&mut self) -> Token {
    let start = self.position;
    let line = self.line;

    let kind = match self.peek(1) {
      | LESS => {
        self.position += 2;
        TokenKind::BitShl
      },
      | EQUAL => {
        self.position += 2;
        TokenKind::LessEqual
      },
      | _ => {
        self.position += 1;
        TokenKind::Less
      },
    };

    self.token(kind, start, line)
  }

  /// Returns a token for one of: a `>>` (bitwise shift right), a `>=`, or a `>`.
  fn greater(&mut self) -> Token {
    let start = self.position;
    let line = self.line;

    let kind = match self.peek(1) {
      | GREATER => {
        self.position += 2;
        TokenKind::BitShr
      },
      | EQUAL => {
        self.position += 2;
        TokenKind::GreaterEqual
      },
      | _ => {
        self.position += 1;
        TokenKind::Greater
      },
    };

    self.token(kind, start, line)
  }

  /// Returns a token for either a `!` or a `!=`.
  fn bang(&mut self) -> Token {
    let start = self.position;
    let line = self.line;

    let kind = match self.peek(1) {
      | EQUAL => {
        self.position += 2;
        TokenKind::NotEqual
      },
      | _ => {
        self.position += 1;
        TokenKind::Bang
      },
    };

    self.token(kind, start, line)
  }

  /// Returns a token for either a `=` or a `==`.
  fn equal(&mut self) -> Token {
    let start = self.position;
    let line = self.line;

    if self.peek(1) == EQUAL {
      self.position += 2;

      return self.token(TokenKind::Equal, start, line);
    }

    self.position += 1;

    self.token(TokenKind::Assign, start, line)
  }

  /// Returns a token for a `/`.
  fn slash(&mut self) -> Token {
    self.token_single(TokenKind::Div)
  }

  /// Returns a token for a `*`.
  fn star(&mut self) -> Token {
    self.token_single(TokenKind::Mul)
  }

  /// Returns a token for a `%`.
  fn percent(&mut self) -> Token {
    self.token_single(TokenKind::Mod)
  }

  /// Returns a token for either a `+` or a `++` (increment).
  fn plus(&mut self) -> Token {
    let start = self.position;
    let line = self.line;

    // We look ahead to see if the next character is a plus. If we have two consecutive pluses,
    // we return token for an increment operator.
    if self.peek(1) == PLUS {
      self.position += 2;

      return self.token(TokenKind::Inc, start, line);
    }

    // Otherwise, we return an add operator.
    self.position += 1;

    self.token(TokenKind::Add, start, line)
  }

  /// Returns a token for a `-` or `--` (decrement).
  fn hyphen(&mut self) -> Token {
    let start = self.position;
    let line = self.line;

    // We look ahead to see if the next character is a hyphen. If we have two consecutive hyphens,
    // we return token for a decrement operator.
    if self.peek(1) == HYPHEN {
      self.position += 2;

      return self.token(TokenKind::Dec, start, line);
    }

    // Otherwise, we return a negate operator.
    self.position += 1;

    self.token(TokenKind::Sub, start, line)
  }

  /// Returns a token for a `_`.
  fn underscore(&mut self) -> Token {
    let start = self.position;

    while self.current_byte() == UNDERSCORE {
      self.position += 1;
    }

    self.identifier(start)
  }

  /// Returns a token for an integer.
  fn int(&mut self) -> Token {
    let start = self.position;
    let line = self.line;

    while let ZERO..=NINE | UNDERSCORE = self.current_byte() {
      self.position += 1;
    }

    self.token(TokenKind::Int, start, line)
  }

  /// Returns a token for an identifier.
  fn identifier(&mut self, offset: usize) -> Token {
    let start = self.position;
    let column = self.column;

    while let ZERO..=NINE | LOWER_A..=LOWER_Z | UPPER_A..=UPPER_Z | UNDERSCORE = self.current_byte()
    {
      self.position += 1
    }

    let slice = self.slice(offset, self.position);
    let length = slice.len();
    let value = slice.as_str();

    let kind = match length {
      | 3 => {
        match value {
          | "int" => TokenKind::IntKw,
          | _ => TokenKind::Ident,
        }
      },
      | 4 => {
        match value {
          | "void" => TokenKind::VoidKw,
          | _ => TokenKind::Ident,
        }
      },
      | 6 => {
        match value {
          | "return" => TokenKind::ReturnKw,
          | _ => TokenKind::Ident,
        }
      },
      | _ => TokenKind::Ident,
    };

    self.advance_column(value);

    Token::new(kind, slice, self.location(start, self.line, column))
  }

  /// Returns a token for a newline.
  fn newline(&mut self) -> Token {
    let start = self.position;
    let line = self.line;
    let column = self.column;

    let current = self.current_byte();

    if current == CARRIAGE_RETURN {
      if self.peek(1) == NEWLINE {
        self.advance_char();
        self.advance_char();
      } else {
        return self.whitespace();
      }
    } else {
      self.advance_char();
    }

    let value = self.slice(start, self.position);
    let location = self.location(start, line, column);

    self.column = 1;
    self.line += 1;

    Token::new(TokenKind::Newline, value, location)
  }

  /// Returns a token for whitespace.
  fn whitespace(&mut self) -> Token {
    let start = self.position;
    let line = self.line;
    let column = self.column;

    while self.has_next() {
      match self.current_byte() {
        | SPACE | TAB | CARRIAGE_RETURN => self.advance_char(),
        | _ => break,
      }
    }

    let value = self.slice(start, self.position);
    let location = self.location(start, line, column);

    Token::new(TokenKind::Whitespace, value, location)
  }
}

#[cfg(test)]
mod tests {
  use std::ops::Range;

  use cyan_reporting::Span;

  use super::TokenKind::*;
  use super::*;

  fn lexer(input: &str) -> Lexer {
    Lexer::new(input.as_bytes())
  }

  fn lex(input: &str) -> Vec<Token> {
    let mut lexer = lexer(input);
    lexer.lex()
  }

  fn token(kind: TokenKind, value: &str, lines: Range<usize>, cols: Range<usize>) -> Token {
    Token::new(
      kind,
      value.to_string(),
      Location::new(Span::default(), lines.into(), cols.into()),
    )
  }

  fn without_offsets(token: Token) -> Token {
    Token::new(
      token.kind,
      token.value,
      Location::new(Span::default(), token.location.lines, token.location.cols),
    )
  }

  fn assert_kinds(tokens: Vec<Token>, expected: Vec<TokenKind>) {
    assert_eq!(tokens.len(), expected.len());

    for (token, kind) in tokens.iter().zip(expected) {
      assert_eq!(token.kind, kind);
    }
  }

  macro_rules! assert_token {
    (
      $input: expr,
      $kind: expr,
      $value: expr,
      $lines: expr,
      $cols: expr
    ) => {{
      let mut lexer = lexer($input);
      let next = lexer.next();

      let actual = without_offsets(next);
      let expected = token($kind, $value, $lines, $cols);

      assert_eq!(actual, expected);
    }};
  }

  macro_rules! assert_tokens {
    (
      $input: expr,
      $($token: expr),+
    ) => {{
      let mut tokens = Vec::new();

      let mut lexer = lexer($input);
      let mut next = without_offsets(lexer.next());

      while next.kind != TokenKind::Eof {
        tokens.push(next);

        next = without_offsets(lexer.next());
      }

      assert_eq!(tokens, vec![$( $token, )+]);
    }};
  }

  #[test]
  fn lex_identifier() {
    assert_token!("foo", Ident, "foo", 1..1, 1..4);
    assert_token!("fooBar", Ident, "fooBar", 1..1, 1..7);
    assert_token!("foo_bar", Ident, "foo_bar", 1..1, 1..8);
    assert_token!("foo_123", Ident, "foo_123", 1..1, 1..8);
    assert_token!("_foo", Ident, "_foo", 1..1, 1..5);
    assert_token!("_", Ident, "_", 1..1, 1..2);
    assert_token!("_0", Ident, "_0", 1..1, 1..3);
  }

  #[test]
  fn lex_keyword() {
    assert_token!("int", IntKw, "int", 1..1, 1..4);
    assert_token!("void", VoidKw, "void", 1..1, 1..5);
    assert_token!("return", ReturnKw, "return", 1..1, 1..7);

    assert_token!("int123", Ident, "int123", 1..1, 1..7);
    assert_token!("void123", Ident, "void123", 1..1, 1..8);
    assert_token!("return123", Ident, "return123", 1..1, 1..10);
  }

  #[test]
  fn lex_constant() {
    assert_token!("0", Int, "0", 1..1, 1..2);
    assert_token!("123", Int, "123", 1..1, 1..4);
    assert_token!("123_456", Int, "123_456", 1..1, 1..8);
  }

  #[test]
  fn lex_whitespace() {
    assert_kinds(lex(" \t\n"), vec![Whitespace, Newline, Eof]);
  }

  #[test]
  fn lex_newline() {
    assert_kinds(lex("\n\n\n"), vec![Newline, Newline, Newline, Eof]);
  }

  #[test]
  fn lex_unary_operators() {
    assert_token!("!", Bang, "!", 1..1, 1..2);
    assert_token!("~", BitNot, "~", 1..1, 1..2);
    assert_token!("--", Dec, "--", 1..1, 1..3);
    assert_token!("++", Inc, "++", 1..1, 1..3);
  }

  #[test]
  fn lex_arithmetic_binary_operators() {
    assert_token!("+", Add, "+", 1..1, 1..2);
    assert_token!("-", Sub, "-", 1..1, 1..2);
    assert_token!("*", Mul, "*", 1..1, 1..2);
    assert_token!("/", Div, "/", 1..1, 1..2);
    assert_token!("%", Mod, "%", 1..1, 1..2);
  }

  #[test]
  fn lex_logical_binary_operators() {
    assert_token!("&&", And, "&&", 1..1, 1..3);
    assert_token!("||", Or, "||", 1..1, 1..3);
    assert_token!("<", Less, "<", 1..1, 1..2);
    assert_token!("<=", LessEqual, "<=", 1..1, 1..3);
    assert_token!(">", Greater, ">", 1..1, 1..2);
    assert_token!(">=", GreaterEqual, ">=", 1..1, 1..3);
    assert_token!("==", Equal, "==", 1..1, 1..3);
    assert_token!("!=", NotEqual, "!=", 1..1, 1..3);
  }

  #[test]
  fn lex_bitwise_binary_operators() {
    assert_token!("|", BitOr, "|", 1..1, 1..2);
    assert_token!("^", BitXor, "^", 1..1, 1..2);
    assert_token!("&", BitAnd, "&", 1..1, 1..2);
    assert_token!("<<", BitShl, "<<", 1..1, 1..3);
    assert_token!(">>", BitShr, ">>", 1..1, 1..3);
  }

  #[test]
  fn lex_binary_operators_sequence() {
    assert_tokens!(
      "+-a",
      token(Add, "+", 1..1, 1..2),
      token(Sub, "-", 1..1, 2..3),
      token(Ident, "a", 1..1, 3..4)
    );
  }

  #[test]
  fn lex_program() {
    let input = indoc::indoc! {"
      int main(void) {
        return 42;
      }
    "};

    assert_tokens!(
      input.trim_end(),
      token(IntKw, "int", 1..1, 1..4),
      token(Whitespace, " ", 1..1, 4..5),
      token(Ident, "main", 1..1, 5..9),
      token(ParenOpen, "(", 1..1, 9..10),
      token(VoidKw, "void", 1..1, 10..14),
      token(ParenClose, ")", 1..1, 14..15),
      token(Whitespace, " ", 1..1, 15..16),
      token(BraceOpen, "{", 1..1, 16..17),
      token(Newline, "\n", 1..1, 17..18),
      token(Whitespace, "  ", 2..2, 1..3),
      token(ReturnKw, "return", 2..2, 3..9),
      token(Whitespace, " ", 2..2, 9..10),
      token(Int, "42", 2..2, 10..12),
      token(Semi, ";", 2..2, 12..13),
      token(Newline, "\n", 2..2, 13..14),
      token(BraceClose, "}", 3..3, 1..2)
    );
  }
}
