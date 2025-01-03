use thiserror::Error;

use crate::ir::ast;
use crate::lexer::{Token, TokenKind};
use crate::location::{Located, Location};

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Error, PartialEq, Eq)]
#[error("parse error {location}: {message}")]
pub struct ParseError {
  /// The error message.
  pub message: String,
  /// The location of the error.
  pub location: Location,
}

impl ParseError {
  pub fn new(message: impl AsRef<str> + Into<String>, location: Location) -> Self {
    Self {
      message: message.into(),
      location,
    }
  }
}

pub struct Parser {
  /// The iterator over tokens.
  tokens: Box<dyn Iterator<Item = Token>>,
  /// The last peeked token.
  peeked: Option<Token>,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    let tokens = Box::new(tokens.into_iter());

    Self {
      tokens,
      peeked: None,
    }
  }

  /// Parses the input and returns the AST.
  pub fn parse(&mut self) -> Result<ast::Program> {
    let init_location = Location::default();
    let function = self.function()?;

    let location = Location::merge(&init_location, &function.location);

    Ok(ast::Program { function, location })
  }

  /// Returns the next token, skipping whitespace and comments.
  fn next(&mut self) -> Token {
    loop {
      let token = self.peeked.take().unwrap_or_else(|| {
        self
          .tokens
          .next()
          .unwrap_or_else(|| Token::eof(Location::default()))
      });

      match token.kind {
        | TokenKind::Whitespace | TokenKind::Newline => continue,
        | _ => return token,
      }
    }
  }

  /// Peeks at the next token advancing to the next token if needed.
  fn peek(&mut self) -> &Token {
    if self.peeked.is_none() {
      self.peeked = Some(self.next());
    }

    self.peeked.as_ref().expect("should have peeked token")
  }

  /// Consumes the next token if it matches the given kind.
  fn expect(&mut self, kind: TokenKind) -> Result<Token> {
    let token = self.next();
    self.check_token_kind(&token, kind)?;

    Ok(token)
  }

  /// Advances to the next token and consumes it if it is valid.
  fn consume(&mut self) -> Result<Token> {
    let token = self.next();
    self.check_token(&token)?;

    Ok(token)
  }

  /// Checks if the next token is valid, i.e. not [TokenKind::Invalid] and not [TokenKind::Eof].
  fn check_token(&self, token: &Token) -> Result<()> {
    match token.kind {
      | TokenKind::Invalid => {
        Err(ParseError::new(
          format!("a '{}' is not allowed", token.value),
          token.location,
        ))
      },
      | TokenKind::Eof => {
        Err(ParseError::new(
          "the end of the input is reached, but more is expected",
          token.location,
        ))
      },
      | _ => Ok(()),
    }
  }

  /// Checks if the next token is of the given kind. Also validates the token.
  fn check_token_kind(&self, token: &Token, kind: TokenKind) -> Result<()> {
    self.check_token(token)?;

    if token.kind != kind {
      return Err(ParseError::new(
        format!(
          "expected {}, found '{}' instead",
          kind.description(),
          token.value
        ),
        token.location,
      ));
    }

    Ok(())
  }
}

impl Parser {
  /// Parses a function definition.
  fn function(&mut self) -> Result<ast::Function> {
    let start = self.expect(TokenKind::IntKw)?;

    let name = self.identifier()?;

    self.expect(TokenKind::ParenOpen)?;
    self.expect(TokenKind::VoidKw)?;
    self.expect(TokenKind::ParenClose)?;
    self.expect(TokenKind::BraceOpen)?;

    let body = self.statement()?;
    let close = self.expect(TokenKind::BraceClose)?;
    let location = Location::merge(&start.location, &close.location);

    Ok(ast::Function {
      name,
      body,
      location,
    })
  }

  /// Parses a statement.
  fn statement(&mut self) -> Result<ast::Statement> {
    let token = self.peek();

    match token.kind {
      | TokenKind::ReturnKw => self.return_statement(),
      | _ => {
        Err(ParseError::new(
          format!("expected statement, found '{}'", token.value),
          token.location,
        ))
      },
    }
  }

  /// Parses a return statement.
  fn return_statement(&mut self) -> Result<ast::Statement> {
    self.consume()?;

    let expression = self.expression(0)?;

    self.expect(TokenKind::Semi)?;

    Ok(ast::Statement::Return(expression))
  }

  /// Parses an expression.
  fn expression(&mut self, min_precedence: usize) -> Result<ast::Expression> {
    let mut left = self.factor()?;
    let mut next = self.peek();

    while next.is_binary_operator() {
      match Self::precedence(next) {
        | Some(precedence) if precedence >= min_precedence => {
          let op = self.binary()?;
          let right = self.expression(precedence + 1)?;

          let left_location = *left.location();
          let right_location = *right.location();

          left = ast::Expression::Binary(ast::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            location: Location::merge(&left_location, &right_location),
          });

          next = self.peek();
        },
        | _ => break,
      }
    }

    Ok(left)
  }

  /// Parses a "factor" expression, i.e. and indirection arm to allow for precedence parsing.
  fn factor(&mut self) -> Result<ast::Expression> {
    let token = self.peek();

    match token.kind {
      | TokenKind::Int => self.constant(),
      | TokenKind::ParenOpen => self.group(),
      | TokenKind::BitNot | TokenKind::Sub | TokenKind::Bang => self.unary(),
      | _ => {
        Err(ParseError::new(
          format!("expected expression, found '{}'", token.value),
          token.location,
        ))
      },
    }
  }

  /// Parses an expression group.
  fn group(&mut self) -> Result<ast::Expression> {
    self.consume()?;

    let expression = self.expression(0)?;

    self.expect(TokenKind::ParenClose)?;

    Ok(expression)
  }

  /// Parses a constant.
  fn constant(&mut self) -> Result<ast::Expression> {
    let token = self.consume()?;

    let value = token.value.parse().map_err(|_| {
      ParseError::new(
        format!("expected a constant, found '{}'", token.value),
        token.location,
      )
    })?;

    Ok(ast::Expression::Constant(ast::Int {
      value,
      location: token.location,
    }))
  }

  /// Parses an unary expression.
  fn unary(&mut self) -> Result<ast::Expression> {
    let token = self.consume()?;

    let op = match token.kind {
      | TokenKind::BitNot => ast::UnaryOp::BitNot,
      | TokenKind::Sub => ast::UnaryOp::Negate,
      | TokenKind::Bang => ast::UnaryOp::Not,
      | _ => {
        return Err(ParseError::new(
          format!("expected unary operator, found '{}'", token.value),
          token.location,
        ))
      },
    };

    let expression = self.factor()?;
    let location = Location::merge(&token.location, expression.location());

    Ok(ast::Expression::Unary(ast::Unary {
      op,
      expression: Box::new(expression),
      location,
    }))
  }

  /// Parses a binary operator.
  fn binary(&mut self) -> Result<ast::BinaryOp> {
    let token = self.consume()?;

    let op = match token.kind {
      // Arithmetic operators.
      | TokenKind::Add => ast::BinaryOp::Add,
      | TokenKind::Div => ast::BinaryOp::Div,
      | TokenKind::Mod => ast::BinaryOp::Mod,
      | TokenKind::Mul => ast::BinaryOp::Mul,
      | TokenKind::Sub => ast::BinaryOp::Sub,
      // Bitwise operators.
      | TokenKind::BitAnd => ast::BinaryOp::BitAnd,
      | TokenKind::BitOr => ast::BinaryOp::BitOr,
      | TokenKind::BitShl => ast::BinaryOp::BitShl,
      | TokenKind::BitShr => ast::BinaryOp::BitShr,
      | TokenKind::BitXor => ast::BinaryOp::BitXor,
      // Logical operators.
      | TokenKind::And => ast::BinaryOp::And,
      | TokenKind::Equal => ast::BinaryOp::Equal,
      | TokenKind::Greater => ast::BinaryOp::Greater,
      | TokenKind::GreaterEqual => ast::BinaryOp::GreaterEqual,
      | TokenKind::Less => ast::BinaryOp::Less,
      | TokenKind::LessEqual => ast::BinaryOp::LessEqual,
      | TokenKind::NotEqual => ast::BinaryOp::NotEqual,
      | TokenKind::Or => ast::BinaryOp::Or,
      // Otherwise we got an unexpected token or an unary operator.
      | _ => {
        return Err(ParseError::new(
          format!("expected binary operator, found '{}'", token.value),
          token.location,
        ))
      },
    };

    Ok(op)
  }

  /// Parses an identifier.
  fn identifier(&mut self) -> Result<ast::Ident> {
    let token = self.consume()?;

    if token.kind != TokenKind::Ident {
      return Err(ParseError::new(
        format!("expected identifier, found '{}'", token.value),
        token.location,
      ));
    }

    Ok(ast::Ident {
      value: token.value.into(),
      location: token.location,
    })
  }

  /// Returns the precedence of the given token. The higher the precedence number, the higher the
  /// binding power.
  ///
  /// Returns `None` if the token is not a binary operator.
  fn precedence(token: &Token) -> Option<usize> {
    match token.kind {
      | TokenKind::Mul | TokenKind::Div | TokenKind::Mod => Some(50),
      | TokenKind::Add | TokenKind::Sub => Some(45),
      | TokenKind::BitShl | TokenKind::BitShr => Some(40),
      | TokenKind::Less | TokenKind::LessEqual | TokenKind::Greater | TokenKind::GreaterEqual => {
        Some(35)
      },
      | TokenKind::Equal | TokenKind::NotEqual => Some(30),
      | TokenKind::BitAnd => Some(25),
      | TokenKind::BitXor => Some(20),
      | TokenKind::BitOr => Some(15),
      | TokenKind::And => Some(10),
      | TokenKind::Or => Some(5),
      | _ => None,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ir::ast;
  use crate::lexer::Token;
  use crate::location::Location;

  fn token(kind: TokenKind, value: &str) -> Token {
    Token::new(kind, value.to_string(), Location::default())
  }

  /// Tries to parse the whole program from the given tokens.
  fn parse(tokens: Vec<Token>) -> Result<ast::Program> {
    let mut parser = Parser::new(tokens);
    parser.parse()
  }

  #[test]
  fn parse_program() {
    let mut parser = Parser::new(vec![
      token(TokenKind::IntKw, "int"),
      token(TokenKind::Ident, "main"),
      token(TokenKind::ParenOpen, "("),
      token(TokenKind::VoidKw, "void"),
      token(TokenKind::ParenClose, ")"),
      token(TokenKind::BraceOpen, "{"),
      token(TokenKind::ReturnKw, "return"),
      token(TokenKind::Int, "42"),
      token(TokenKind::Semi, ";"),
      token(TokenKind::BraceClose, "}"),
    ]);

    let actual = parser.parse();

    let expected = ast::Program {
      function: ast::Function {
        name: ast::Ident {
          value: "main".to_string().into(),
          location: Location::default(),
        },
        body: ast::Statement::Return(ast::Expression::Constant(ast::Int {
          value: 42,
          location: Location::default(),
        })),
        location: Location::default(),
      },
      location: Location::default(),
    };

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_statement() {
    let mut parser = Parser::new(vec![
      token(TokenKind::ReturnKw, "return"),
      token(TokenKind::Int, "42"),
      token(TokenKind::Semi, ";"),
    ]);

    let actual = parser.statement();

    let expected = ast::Statement::Return(ast::Expression::Constant(ast::Int {
      value: 42,
      location: Location::default(),
    }));

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_constant() {
    let mut parser = Parser::new(vec![token(TokenKind::Int, "42")]);

    let actual = parser.constant();

    let expected = ast::Expression::Constant(ast::Int {
      value: 42,
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_unary() {
    let mut parser = Parser::new(vec![
      token(TokenKind::Sub, "-"),
      token(TokenKind::Int, "42"),
    ]);

    let actual = parser.unary();

    let expected = ast::Expression::Unary(ast::Unary {
      op: ast::UnaryOp::Negate,
      expression: Box::new(ast::Expression::Constant(ast::Int {
        value: 42,
        location: Location::default(),
      })),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_unary_nested() {
    // -(~(-42))
    let mut parser = Parser::new(vec![
      token(TokenKind::Sub, "-"),
      token(TokenKind::ParenOpen, "("),
      token(TokenKind::BitNot, "~"),
      token(TokenKind::ParenOpen, "("),
      token(TokenKind::Int, "-42"),
      token(TokenKind::ParenClose, ")"),
      token(TokenKind::ParenClose, ")"),
    ]);

    let actual = parser.expression(0);

    let expected = ast::Expression::Unary(ast::Unary {
      op: ast::UnaryOp::Negate,
      expression: Box::new(ast::Expression::Unary(ast::Unary {
        op: ast::UnaryOp::BitNot,
        expression: Box::new(ast::Expression::Constant(ast::Int {
          value: -42,
          location: Location::default(),
        })),
        location: Location::default(),
      })),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_unary_not() {
    let mut parser = Parser::new(vec![
      token(TokenKind::ReturnKw, "return"),
      token(TokenKind::Bang, "!"),
      token(TokenKind::Int, "42"),
      token(TokenKind::Semi, ";"),
    ]);

    let actual = parser.statement();

    let expected = ast::Statement::Return(ast::Expression::Unary(ast::Unary {
      op: ast::UnaryOp::Not,
      expression: Box::new(ast::Expression::Constant(ast::Int {
        value: 42,
        location: Location::default(),
      })),
      location: Location::default(),
    }));

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_binary() {
    let mut parser = Parser::new(vec![
      token(TokenKind::Int, "1"),
      token(TokenKind::Mul, "*"),
      token(TokenKind::Int, "2"),
    ]);

    let actual = parser.expression(0);

    let expected = ast::Expression::Binary(ast::Binary {
      op: ast::BinaryOp::Mul,
      left: Box::new(ast::Expression::Constant(ast::Int {
        value: 1,
        location: Location::default(),
      })),
      right: Box::new(ast::Expression::Constant(ast::Int {
        value: 2,
        location: Location::default(),
      })),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_precedence() {
    //  1 * 2  -  3 * (4 + 5)
    // (1 * 2) - (3 * (4 + 5))
    //         ^ root
    // ```
    //      -
    //     / \
    //    /   \
    //   *     *
    //  / \   / \
    // 1  2  3   +
    //          / \
    //         4   5
    // ```
    let mut parser = Parser::new(vec![
      token(TokenKind::Int, "1"),
      token(TokenKind::Mul, "*"),
      token(TokenKind::Int, "2"),
      token(TokenKind::Sub, "-"),
      token(TokenKind::Int, "3"),
      token(TokenKind::Mul, "*"),
      token(TokenKind::ParenOpen, "("),
      token(TokenKind::Int, "4"),
      token(TokenKind::Add, "+"),
      token(TokenKind::Int, "5"),
      token(TokenKind::ParenClose, ")"),
    ]);

    let actual = parser.expression(0);

    let expected = ast::Expression::Binary(ast::Binary {
      op: ast::BinaryOp::Sub,
      left: Box::new(ast::Expression::Binary(ast::Binary {
        op: ast::BinaryOp::Mul,
        left: Box::new(ast::Expression::Constant(ast::Int {
          value: 1,
          location: Location::default(),
        })),
        right: Box::new(ast::Expression::Constant(ast::Int {
          value: 2,
          location: Location::default(),
        })),
        location: Location::default(),
      })),
      right: Box::new(ast::Expression::Binary(ast::Binary {
        op: ast::BinaryOp::Mul,
        left: Box::new(ast::Expression::Constant(ast::Int {
          value: 3,
          location: Location::default(),
        })),
        right: Box::new(ast::Expression::Binary(ast::Binary {
          op: ast::BinaryOp::Add,
          left: Box::new(ast::Expression::Constant(ast::Int {
            value: 4,
            location: Location::default(),
          })),
          right: Box::new(ast::Expression::Constant(ast::Int {
            value: 5,
            location: Location::default(),
          })),
          location: Location::default(),
        })),
        location: Location::default(),
      })),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_empty_program() {
    let result = parse(vec![]);

    assert!(result.is_err());

    assert_eq!(
      result.unwrap_err().message,
      "the end of the input is reached, but more is expected"
    );
  }

  #[test]
  fn parse_function_definition_missing_brace() {
    let result = parse(vec![
      token(TokenKind::IntKw, "int"),
      token(TokenKind::Ident, "main"),
      token(TokenKind::ParenOpen, "("),
      token(TokenKind::VoidKw, "void"),
      token(TokenKind::ParenClose, ")"),
      // Missing opening brace
      token(TokenKind::ReturnKw, "return"),
      token(TokenKind::Int, "5"),
      token(TokenKind::Semi, ";"),
      token(TokenKind::BraceClose, "}"),
    ]);

    assert!(result.is_err());

    assert_eq!(
      result.unwrap_err().message,
      "expected a '{', found 'return' instead"
    );
  }

  #[test]
  fn parse_invalid_token() {
    let result = parse(vec![token(TokenKind::Invalid, "@")]);

    assert!(result.is_err());

    assert_eq!(result.unwrap_err().message, "a '@' is not allowed");
  }

  #[test]
  fn parse_unexpected_end_of_input() {
    let result = parse(vec![
      token(TokenKind::IntKw, "int"),
      token(TokenKind::Ident, "main"),
      token(TokenKind::ParenOpen, "("),
    ]);

    assert!(result.is_err());

    assert_eq!(
      result.unwrap_err().message,
      "the end of the input is reached, but more is expected"
    );
  }
}
