use cyan_reporting::{Located, Location};
use thiserror::Error;

use crate::ir::ast;
use crate::lexer::{Token, TokenKind};

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
    self.program()
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
  /// Parses a whole program.
  fn program(&mut self) -> Result<ast::Program> {
    let init_location = Location::default();
    let function = self.function()?;

    let location = Location::merge(&init_location, &function.location);

    Ok(ast::Program { function, location })
  }

  /// Parses a function definition.
  fn function(&mut self) -> Result<ast::Function> {
    let start = self.expect(TokenKind::IntKw)?;

    let name = self.identifier()?;

    self.expect(TokenKind::ParenOpen)?;
    self.expect(TokenKind::VoidKw)?;
    self.expect(TokenKind::ParenClose)?;
    self.expect(TokenKind::BraceOpen)?;

    let mut body = Vec::new();

    while self.peek().kind != TokenKind::BraceClose {
      body.push(self.block_item()?);
    }

    let close = self.expect(TokenKind::BraceClose)?;
    let location = Location::merge(&start.location, &close.location);

    Ok(ast::Function {
      name,
      body,
      location,
    })
  }

  /// Parses a block item.
  fn block_item(&mut self) -> Result<ast::BlockItem> {
    let token = self.peek();

    match token.kind {
      | TokenKind::IntKw => Ok(ast::BlockItem::Declaration(self.declaration()?)),
      | _ => Ok(ast::BlockItem::Statement(self.statement()?)),
    }
  }

  /// Parses a declaration.
  fn declaration(&mut self) -> Result<ast::Declaration> {
    let start = self.expect(TokenKind::IntKw)?;
    let name = self.identifier()?;

    let next = self.peek();

    match next.kind {
      | TokenKind::Assign => {
        self.consume()?;

        let right = self.expression(0)?;
        let end = self.expect(TokenKind::Semi)?;
        let location = Location::merge(&start.location, &end.location);

        Ok(ast::Declaration {
          name,
          initializer: Some(right),
          location,
        })
      },
      | TokenKind::Semi => {
        let end = self.expect(TokenKind::Semi)?;
        let location = Location::merge(&start.location, &end.location);

        Ok(ast::Declaration {
          name,
          initializer: None,
          location,
        })
      },
      | _ => {
        Err(ParseError::new(
          format!(
            "expected ';' or '=' after declaration, found '{}'",
            next.value
          ),
          next.location,
        ))
      },
    }
  }

  /// Parses a statement.
  fn statement(&mut self) -> Result<ast::Statement> {
    let token = self.peek();

    match token.kind {
      | TokenKind::ReturnKw => self.statement_return(),
      | TokenKind::Semi => self.statement_null(),
      | _ => self.statement_expression(),
    }
  }

  /// Parses a return statement.
  fn statement_return(&mut self) -> Result<ast::Statement> {
    self.consume()?;

    let expression = self.expression(0)?;

    self.expect(TokenKind::Semi)?;

    Ok(ast::Statement::Return(expression))
  }

  /// Parses an expression statement.
  fn statement_expression(&mut self) -> Result<ast::Statement> {
    let expression = self.expression(0)?;

    self.expect(TokenKind::Semi)?;

    Ok(ast::Statement::Expression(expression))
  }

  /// Parses a 'null statement', i.e. semicolon.
  fn statement_null(&mut self) -> Result<ast::Statement> {
    let token = self.expect(TokenKind::Semi)?;

    Ok(ast::Statement::Null {
      location: token.location,
    })
  }

  /// Parses an expression.
  fn expression(&mut self, min_precedence: usize) -> Result<ast::Expression> {
    let mut left = self.factor()?;
    let mut next = self.peek();

    while next.is_binary_operator() {
      match Self::precedence(next) {
        | Some(precedence) if precedence >= min_precedence => {
          // Handle assignment differently, because unlike other binary operators it is
          // right-associative, i.e. we need to parse the following:
          //
          // a = b = c = d
          //
          // as:
          //
          // a = (b = (c = d))
          //
          // instead of:
          //
          // ((a = b) = c) = d
          //
          // This is not ideal, because if we ever need other right-associative operators, e.g.
          // other assignment operations like += or *=, we'll need to slightly alter this logic. But
          // for now, it's fine.
          if next.kind == TokenKind::Assign {
            self.consume()?;

            let right = self.expression(precedence)?;

            let left_location = *left.location();
            let right_location = *right.location();

            left = ast::Expression::Assignment(ast::Assignment {
              left: Box::new(left),
              right: Box::new(right),
              location: Location::merge(&left_location, &right_location),
            });
          }
          // Otherwise, we just parse the (left-associative) binary operators.
          else {
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
          }

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

    if token.is_unary_operator() {
      return self.unary();
    }

    match token.kind {
      | TokenKind::Int => self.constant(),
      | TokenKind::Ident => self.variable(),
      | TokenKind::ParenOpen => self.group(),
      | _ => {
        Err(ParseError::new(
          format!("expected a factor, found '{}'", token.value),
          token.location,
        ))
      },
    }
  }

  /// Parses a variable.
  fn variable(&mut self) -> Result<ast::Expression> {
    let ident = self.identifier()?;

    Ok(ast::Expression::Var(ident))
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
      | TokenKind::Assign => Some(1),
      | _ => None,
    }
  }
}

#[cfg(test)]
mod tests {
  use cyan_reporting::Location;
  use indoc::indoc;

  use super::*;
  use crate::ir::ast;
  use crate::lexer::Lexer;

  /// Creates a new parser from the given input.
  fn parser(input: impl AsRef<str> + Into<String>) -> Parser {
    let mut lexer = Lexer::new(input.as_ref().trim().as_bytes());
    let tokens = lexer.lex_locationless();

    Parser::new(tokens)
  }

  #[test]
  fn parse_program() {
    let actual = parser(indoc! {"
      int main(void) {
        int a = 21;
        int b = 42;
        int c;

        b = b / 2;
        c = a + b;

        return c;
      }
    "})
    .parse();

    let expected = ast::Program {
      function: ast::Function {
        name: ast::Ident {
          value: "main".to_string().into(),
          location: Location::default(),
        },
        body: vec![
          ast::BlockItem::Declaration(ast::Declaration {
            name: ast::Ident {
              value: "a".to_string().into(),
              location: Location::default(),
            },
            initializer: Some(ast::Expression::Constant(ast::Int {
              value: 21,
              location: Location::default(),
            })),
            location: Location::default(),
          }),
          ast::BlockItem::Declaration(ast::Declaration {
            name: ast::Ident {
              value: "b".to_string().into(),
              location: Location::default(),
            },
            initializer: Some(ast::Expression::Constant(ast::Int {
              value: 42,
              location: Location::default(),
            })),
            location: Location::default(),
          }),
          ast::BlockItem::Declaration(ast::Declaration {
            name: ast::Ident {
              value: "c".to_string().into(),
              location: Location::default(),
            },
            initializer: None,
            location: Location::default(),
          }),
          ast::BlockItem::Statement(ast::Statement::Expression(ast::Expression::Assignment(
            ast::Assignment {
              left: Box::new(ast::Expression::Var(ast::Ident {
                value: "b".to_string().into(),
                location: Location::default(),
              })),
              right: Box::new(ast::Expression::Binary(ast::Binary {
                op: ast::BinaryOp::Div,
                left: Box::new(ast::Expression::Var(ast::Ident {
                  value: "b".to_string().into(),
                  location: Location::default(),
                })),
                right: Box::new(ast::Expression::Constant(ast::Int {
                  value: 2,
                  location: Location::default(),
                })),
                location: Location::default(),
              })),
              location: Location::default(),
            },
          ))),
          ast::BlockItem::Statement(ast::Statement::Expression(ast::Expression::Assignment(
            ast::Assignment {
              left: Box::new(ast::Expression::Var(ast::Ident {
                value: "c".to_string().into(),
                location: Location::default(),
              })),
              right: Box::new(ast::Expression::Binary(ast::Binary {
                op: ast::BinaryOp::Add,
                left: Box::new(ast::Expression::Var(ast::Ident {
                  value: "a".to_string().into(),
                  location: Location::default(),
                })),
                right: Box::new(ast::Expression::Var(ast::Ident {
                  value: "b".to_string().into(),
                  location: Location::default(),
                })),
                location: Location::default(),
              })),
              location: Location::default(),
            },
          ))),
          ast::BlockItem::Statement(ast::Statement::Return(ast::Expression::Var(ast::Ident {
            value: "c".to_string().into(),
            location: Location::default(),
          }))),
        ],
        location: Location::default(),
      },
      location: Location::default(),
    };

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_declaration_with_initializer() {
    let actual = parser("int meaning = 42;").declaration();

    let expected = ast::Declaration {
      name: ast::Ident {
        value: "meaning".to_string().into(),
        location: Location::default(),
      },
      initializer: Some(ast::Expression::Constant(ast::Int {
        value: 42,
        location: Location::default(),
      })),
      location: Location::default(),
    };

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_declaration_without_initializer() {
    let actual = parser("int meaning;").declaration();

    let expected = ast::Declaration {
      name: ast::Ident {
        value: "meaning".to_string().into(),
        location: Location::default(),
      },
      initializer: None,
      location: Location::default(),
    };

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_declaration_with_multiple_assignments() {
    let actual = parser("int a = b = c = 42;").block_item();

    let expected = ast::BlockItem::Declaration(ast::Declaration {
      name: ast::Ident {
        value: "a".to_string().into(),
        location: Location::default(),
      },
      initializer: Some(ast::Expression::Assignment(ast::Assignment {
        left: Box::new(ast::Expression::Var(ast::Ident {
          value: "b".to_string().into(),
          location: Location::default(),
        })),
        right: Box::new(ast::Expression::Assignment(ast::Assignment {
          left: Box::new(ast::Expression::Var(ast::Ident {
            value: "c".to_string().into(),
            location: Location::default(),
          })),
          right: Box::new(ast::Expression::Constant(ast::Int {
            value: 42,
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
  fn parse_assignment_statement() {
    let actual = parser("meaning = 42;").block_item();

    let expected = ast::BlockItem::Statement(ast::Statement::Expression(
      ast::Expression::Assignment(ast::Assignment {
        left: Box::new(ast::Expression::Var(ast::Ident {
          value: "meaning".to_string().into(),
          location: Location::default(),
        })),
        right: Box::new(ast::Expression::Constant(ast::Int {
          value: 42,
          location: Location::default(),
        })),
        location: Location::default(),
      }),
    ));

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_return_statement() {
    let actual = parser("return 42;").statement();

    let expected = ast::Statement::Return(ast::Expression::Constant(ast::Int {
      value: 42,
      location: Location::default(),
    }));

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_constant() {
    let actual = parser("42").constant();

    let expected = ast::Expression::Constant(ast::Int {
      value: 42,
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_unary() {
    let actual = parser("-42").unary();

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
    let actual = parser("-(~(-42))").expression(0);

    let expected = ast::Expression::Unary(ast::Unary {
      op: ast::UnaryOp::Negate,
      expression: Box::new(ast::Expression::Unary(ast::Unary {
        op: ast::UnaryOp::BitNot,
        expression: Box::new(ast::Expression::Unary(ast::Unary {
          op: ast::UnaryOp::Negate,
          expression: Box::new(ast::Expression::Constant(ast::Int {
            value: 42,
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
  fn parse_unary_not() {
    let actual = parser("!42;").statement();

    let expected = ast::Statement::Expression(ast::Expression::Unary(ast::Unary {
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
    let actual = parser("1 * 2").expression(0);

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

    let actual = parser("1 * 2 - 3 * (4 + 5)").expression(0);

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
  fn parse_empty_main() {
    let actual = parser("int main(void) {}").function();

    let expected = ast::Function {
      name: ast::Ident {
        value: "main".to_string().into(),
        location: Location::default(),
      },
      body: vec![],
      location: Location::default(),
    };

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_empty_program() {
    let actual = parser("").parse();

    assert!(actual.is_err());

    assert_eq!(
      actual.unwrap_err().message,
      "the end of the input is reached, but more is expected"
    );
  }

  #[test]
  fn parse_function_definition_missing_brace() {
    let actual = parser(indoc! {"
      int main(void)
        return 5;
      }
    "})
    .parse();

    assert!(actual.is_err());

    assert_eq!(
      actual.unwrap_err().message,
      "expected a '{', found 'return' instead"
    );
  }

  #[test]
  fn parse_invalid_token() {
    let actual = parser("@").parse();

    assert!(actual.is_err());
    assert_eq!(actual.unwrap_err().message, "a '@' is not allowed");
  }

  #[test]
  fn parse_unexpected_end_of_input() {
    let actual = parser("int main(").parse();

    assert!(actual.is_err());

    assert_eq!(
      actual.unwrap_err().message,
      "the end of the input is reached, but more is expected"
    );
  }
}
