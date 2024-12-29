use thiserror::Error;

use crate::ir::ast;
use crate::lexer::{Token, TokenKind};
use crate::span::{Span, Spanned};

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Error)]
#[error("parse error [{span}]: {message}")]
pub struct ParseError {
  /// The error message.
  pub message: String,
  /// The span of the error.
  pub span: Span,
}

impl ParseError {
  pub fn new(message: impl AsRef<str> + Into<String>, span: Span) -> Self {
    Self {
      message: message.into(),
      span,
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
    let init_span = Span::default();
    let function = self.function()?;

    let span = Span::merge(&init_span, &function.span);

    Ok(ast::Program { function, span })
  }

  /// Returns the next token, skipping whitespace and comments.
  fn next(&mut self) -> Token {
    loop {
      let token = self.peeked.take().unwrap_or_else(|| {
        self
          .tokens
          .next()
          .unwrap_or_else(|| Token::eof(Span::default()))
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
    let span = token.span.clone();

    match token.kind {
      | TokenKind::Invalid => {
        Err(ParseError::new(
          format!("a '{}' is not allowed", token.value),
          span,
        ))
      },
      | TokenKind::Eof => {
        Err(ParseError::new(
          "the end of the input is reached, but more is expected",
          span,
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
        token.span.clone(),
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

    let span = Span::merge(&start.span, &close.span);

    Ok(ast::Function { name, body, span })
  }

  /// Parses a statement.
  fn statement(&mut self) -> Result<ast::Statement> {
    let token = self.peek();

    match token.kind {
      | TokenKind::ReturnKw => self.return_statement(),
      | _ => {
        Err(ParseError::new(
          format!("expected statement, found '{}'", token.value),
          token.span.clone(),
        ))
      },
    }
  }

  /// Parses a return statement.
  fn return_statement(&mut self) -> Result<ast::Statement> {
    self.consume()?;

    let expression = self.expression()?;

    self.expect(TokenKind::Semicolon)?;

    Ok(ast::Statement::Return(expression))
  }

  /// Parses an expression.
  fn expression(&mut self) -> Result<ast::Expression> {
    let mut left = self.factor()?;
    let mut next = self.peek();

    while next.is_binary_operator() {
      let operator = self.binary()?;
      let right = self.factor()?;

      let left_span = left.span().clone();
      let right_span = right.span().clone();

      left = ast::Expression::Binary(ast::Binary {
        operator,
        left: Box::new(left),
        right: Box::new(right),
        span: Span::merge(&left_span, &right_span),
      });

      next = self.peek();
    }

    Ok(left)
  }

  /// Parses a "factor" expression, i.e. and indirection arm to allow for precedence parsing.
  fn factor(&mut self) -> Result<ast::Expression> {
    let token = self.peek();

    match token.kind {
      | TokenKind::Constant => self.constant(),
      | TokenKind::BitwiseNot | TokenKind::Negate => self.unary(),
      | TokenKind::ParenOpen => self.expression_group(),
      | _ => {
        Err(ParseError::new(
          format!("expected expression, found '{}'", token.value),
          token.span.clone(),
        ))
      },
    }
  }

  /// Parses an expression group.
  fn expression_group(&mut self) -> Result<ast::Expression> {
    self.consume()?;

    let expression = self.expression()?;

    self.expect(TokenKind::ParenClose)?;

    Ok(expression)
  }

  /// Parses a constant.
  fn constant(&mut self) -> Result<ast::Expression> {
    let token = self.consume()?;

    let value = token.value.parse().map_err(|_| {
      ParseError::new(
        format!("expected a constant, found '{}'", token.value),
        token.span.clone(),
      )
    })?;

    Ok(ast::Expression::Constant(ast::Int {
      value,
      span: token.span.clone(),
    }))
  }

  /// Parses an unary expression.
  fn unary(&mut self) -> Result<ast::Expression> {
    let token = self.consume()?;

    let operator = match token.kind {
      | TokenKind::BitwiseNot => ast::UnaryOp::BitwiseNot,
      | TokenKind::Negate => ast::UnaryOp::Negate,
      | _ => {
        return Err(ParseError::new(
          format!("expected unary operator, found '{}'", token.value),
          token.span.clone(),
        ))
      },
    };

    let expression = self.factor()?;
    let span = Span::merge(&token.span, expression.span());

    Ok(ast::Expression::Unary(ast::Unary {
      operator,
      expression: Box::new(expression),
      span,
    }))
  }

  /// Parses a binary operator.
  fn binary(&mut self) -> Result<ast::BinaryOp> {
    let token = self.consume()?;

    let operator = match token.kind {
      | TokenKind::Plus => ast::BinaryOp::Add,
      | TokenKind::Negate => ast::BinaryOp::Subtract,
      | TokenKind::Multiply => ast::BinaryOp::Multiply,
      | TokenKind::Divide => ast::BinaryOp::Divide,
      | TokenKind::Percent => ast::BinaryOp::Remainder,
      | _ => {
        return Err(ParseError::new(
          format!("expected binary operator, found '{}'", token.value),
          token.span.clone(),
        ))
      },
    };

    Ok(operator)
  }

  /// Parses an identifier.
  fn identifier(&mut self) -> Result<ast::Identifier> {
    let token = self.consume()?;

    if token.kind != TokenKind::Identifier {
      return Err(ParseError::new(
        format!("expected identifier, found '{}'", token.value),
        token.span.clone(),
      ));
    }

    Ok(ast::Identifier {
      value: token.value.into(),
      span: token.span,
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ir::ast;
  use crate::lexer::Token;
  use crate::span::Span;

  fn token(kind: TokenKind, value: &str, span: Span) -> Token {
    Token::new(kind, value.to_string(), span)
  }

  fn parse(tokens: Vec<Token>) -> Result<ast::Program> {
    let mut parser = Parser::new(tokens);
    parser.parse()
  }

  #[test]
  fn test_function_definition_parsing() {
    let actual = parse(vec![
      token(TokenKind::IntKw, "int", Span::default()),
      token(TokenKind::Identifier, "main", Span::default()),
      token(TokenKind::ParenOpen, "(", Span::default()),
      token(TokenKind::VoidKw, "void", Span::default()),
      token(TokenKind::ParenClose, ")", Span::default()),
      token(TokenKind::BraceOpen, "{", Span::default()),
      token(TokenKind::ReturnKw, "return", Span::default()),
      token(TokenKind::Constant, "5", Span::default()),
      token(TokenKind::Semicolon, ";", Span::default()),
      token(TokenKind::BraceClose, "}", Span::default()),
    ])
    .expect("should parse function definition");

    let expected = ast::Program {
      function: ast::Function {
        name: ast::Identifier {
          value: "main".to_string().into(),
          span: Span::default(),
        },
        body: ast::Statement::Return(ast::Expression::Constant(ast::Int {
          value: 5,
          span: Span::default(),
        })),
        span: Span::default(),
      },
      span: Span::default(),
    };

    assert_eq!(actual, expected);
  }

  #[test]
  fn test_empty_program() {
    let result = parse(vec![]);

    assert!(result.is_err());
    assert_eq!(
      result.unwrap_err().message,
      "the end of the input is reached, but more is expected"
    );
  }

  #[test]
  fn test_function_definition_missing_brace() {
    let result = parse(vec![
      token(TokenKind::IntKw, "int", Span::default()),
      token(TokenKind::Identifier, "main", Span::default()),
      token(TokenKind::ParenOpen, "(", Span::default()),
      token(TokenKind::VoidKw, "void", Span::default()),
      token(TokenKind::ParenClose, ")", Span::default()),
      // Missing opening brace
      token(TokenKind::ReturnKw, "return", Span::default()),
      token(TokenKind::Constant, "5", Span::default()),
      token(TokenKind::Semicolon, ";", Span::default()),
      token(TokenKind::BraceClose, "}", Span::default()),
    ]);

    assert!(result.is_err());
    assert_eq!(
      result.unwrap_err().message,
      "expected a '{', found 'return' instead"
    );
  }

  #[test]
  fn test_invalid_token() {
    let result = parse(vec![token(TokenKind::Invalid, "@", Span::default())]);

    assert!(result.is_err());
    assert_eq!(result.unwrap_err().message, "a '@' is not allowed");
  }

  #[test]
  fn test_unexpected_end_of_input() {
    let result = parse(vec![
      token(TokenKind::IntKw, "int", Span::default()),
      token(TokenKind::Identifier, "main", Span::default()),
      token(TokenKind::ParenOpen, "(", Span::default()),
    ]);

    assert!(result.is_err());
    assert_eq!(
      result.unwrap_err().message,
      "the end of the input is reached, but more is expected"
    );
  }
}
