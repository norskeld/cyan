use std::mem;

use cyan_reporting::{Located, Location};
use thiserror::Error;

use crate::ir::ast::*;
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
  /// The tokens to parse.
  tokens: Vec<Token>,
  /// Current position in token stream.
  pos: usize,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Self { tokens, pos: 0 }
  }

  /// Parses the input and returns the AST.
  pub fn parse(&mut self) -> Result<Program> {
    self.program()
  }

  /// Returns the next token, skipping whitespace and comments.
  fn next(&mut self) -> Token {
    loop {
      if self.pos >= self.tokens.len() {
        return Token::default();
      }

      // Move the token out of the vector, replacing it with EOF token.
      let token = mem::take(&mut self.tokens[self.pos]);

      self.pos += 1;

      match token.kind {
        | TokenKind::Whitespace | TokenKind::Newline => continue,
        | _ => return token,
      }
    }
  }

  /// Peeks at the `n`th token relative the current position, skipping whitespace and newlines.
  fn peek(&self, mut n: usize) -> &Token {
    let mut pos = self.pos;

    while pos < self.tokens.len() {
      // Here `n` acts as the logical offset, decrementing only when a non-whitespace or
      // newline token is encountered.
      match self.tokens[pos].kind {
        | TokenKind::Whitespace | TokenKind::Newline => pos += 1,
        | _ if n <= 1 => return &self.tokens[pos],
        | _ => {
          n -= 1;
          pos += 1;
        },
      }
    }

    &self.tokens[self.tokens.len().saturating_sub(1)]
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
  fn program(&mut self) -> Result<Program> {
    let init_location = Location::default();
    let function = self.function()?;

    let location = Location::merge(&init_location, &function.location);

    Ok(Program { function, location })
  }

  /// Parses a function definition.
  fn function(&mut self) -> Result<Function> {
    let start = self.expect(TokenKind::IntKw)?;

    let name = self.identifier()?;

    self.expect(TokenKind::ParenOpen)?;
    self.expect(TokenKind::VoidKw)?;
    self.expect(TokenKind::ParenClose)?;

    let body = self.block()?;
    let location = Location::merge(&start.location, &body.location);

    Ok(Function {
      name,
      body,
      location,
    })
  }

  /// Parses a block.
  fn block(&mut self) -> Result<Block> {
    let open = self.expect(TokenKind::BraceOpen)?;

    let mut body = Vec::new();

    while self.peek(1).kind != TokenKind::BraceClose {
      body.push(self.block_item()?);
    }

    let close = self.expect(TokenKind::BraceClose)?;
    let location = Location::merge(&open.location, &close.location);

    Ok(Block { body, location })
  }

  /// Parses a block item.
  fn block_item(&mut self) -> Result<BlockItem> {
    let token = self.peek(1);

    match token.kind {
      | TokenKind::IntKw => Ok(BlockItem::Declaration(self.declaration()?)),
      | _ => Ok(BlockItem::Statement(self.statement()?)),
    }
  }

  /// Parses a declaration.
  fn declaration(&mut self) -> Result<Declaration> {
    let start = self.expect(TokenKind::IntKw)?;
    let name = self.identifier()?;

    let next = self.peek(1);

    match next.kind {
      | TokenKind::Assign => {
        self.consume()?;

        let right = self.expression(0)?;
        let end = self.expect(TokenKind::Semi)?;
        let location = Location::merge(&start.location, &end.location);

        Ok(Declaration {
          name,
          initializer: Some(right),
          location,
        })
      },
      | TokenKind::Semi => {
        let end = self.expect(TokenKind::Semi)?;
        let location = Location::merge(&start.location, &end.location);

        Ok(Declaration {
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
  fn statement(&mut self) -> Result<Statement> {
    let token = self.peek(1);

    match token.kind {
      | TokenKind::Ident if self.peek(2).kind == TokenKind::Colon => self.statement_labeled(),
      | TokenKind::IfKw => self.statement_if(),
      | TokenKind::BreakKw => self.statement_break(),
      | TokenKind::ContinueKw => self.statement_continue(),
      | TokenKind::DoKw => self.statement_do_while(),
      | TokenKind::WhileKw => self.statement_while(),
      | TokenKind::ForKw => self.statement_for(),
      | TokenKind::GotoKw => self.statement_goto(),
      | TokenKind::ReturnKw => self.statement_return(),
      | TokenKind::BraceOpen => self.statement_block(),
      | TokenKind::Semi => self.statement_null(),
      | _ => self.statement_expression(),
    }
  }

  /// Parses a break statement.
  fn statement_break(&mut self) -> Result<Statement> {
    let token = self.consume()?;

    self.expect(TokenKind::Semi)?;

    Ok(Statement::Break(Break {
      label: None,
      location: token.location,
    }))
  }

  /// Parses a continue statement.
  fn statement_continue(&mut self) -> Result<Statement> {
    let token = self.consume()?;

    self.expect(TokenKind::Semi)?;

    Ok(Statement::Continue(Continue {
      label: None,
      location: token.location,
    }))
  }

  /// Parses a while statement.
  fn statement_while(&mut self) -> Result<Statement> {
    let token = self.consume()?;

    self.expect(TokenKind::ParenOpen)?;

    let condition = self.expression(0)?;

    self.expect(TokenKind::ParenClose)?;

    let body = self.statement().map(Box::new)?;

    let location = Location::merge(&token.location, body.location());

    Ok(Statement::While(While {
      condition,
      body,
      label: None,
      location,
    }))
  }

  /// Parses a do-while statement.
  fn statement_do_while(&mut self) -> Result<Statement> {
    let token = self.consume()?;
    let body = self.statement().map(Box::new)?;

    self.expect(TokenKind::WhileKw)?;
    self.expect(TokenKind::ParenOpen)?;

    let condition = self.expression(0)?;

    self.expect(TokenKind::ParenClose)?;
    self.expect(TokenKind::Semi)?;

    let location = Location::merge(&token.location, body.location());

    Ok(Statement::DoWhile(DoWhile {
      condition,
      body,
      label: None,
      location,
    }))
  }

  /// Parses a for statement.
  fn statement_for(&mut self) -> Result<Statement> {
    let token = self.consume()?;

    self.expect(TokenKind::ParenOpen)?;

    let initializer = self.statement_for_initializer()?;
    let (condition, _) = self.optional_expression(TokenKind::Semi)?;
    let (postcondition, _) = self.optional_expression(TokenKind::ParenClose)?;
    let body = self.statement().map(Box::new)?;

    let location = Location::merge(&token.location, body.location());

    Ok(Statement::For(For {
      initializer,
      condition,
      postcondition,
      body,
      label: None,
      location,
    }))
  }

  /// Parses an initializer for a for statement.
  fn statement_for_initializer(&mut self) -> Result<Initializer> {
    let token = self.peek(1);

    match token.kind {
      | TokenKind::IntKw => self.declaration().map(Initializer::Declaration),
      | _ => {
        let (expression, location) = self.optional_expression(TokenKind::Semi)?;

        match expression {
          | Some(expression) => Ok(Initializer::Expression(expression)),
          | None => Ok(Initializer::None { location }),
        }
      },
    }
  }

  /// Parses a goto statement.
  fn statement_goto(&mut self) -> Result<Statement> {
    let goto = self.consume()?;
    let label = self.identifier()?;

    self.expect(TokenKind::Semi)?;

    let location = Location::merge(&goto.location, label.location());

    Ok(Statement::Goto(Goto { label, location }))
  }

  /// Parses a labeled statement.
  fn statement_labeled(&mut self) -> Result<Statement> {
    let label = self.identifier()?;

    self.expect(TokenKind::Colon)?;

    let statement = self.statement().map(Box::new)?;
    let location = Location::merge(label.location(), statement.location());

    Ok(Statement::Labeled(Labeled {
      label,
      statement,
      location,
    }))
  }

  /// Parses an if statement.
  fn statement_if(&mut self) -> Result<Statement> {
    // Consume the `if` keyword.
    let if_token = self.consume()?;

    // Parse the condition.
    self.expect(TokenKind::ParenOpen)?;
    let condition = self.expression(0).map(Box::new)?;
    self.expect(TokenKind::ParenClose)?;

    // Parse the `then` branch.
    let then = self.statement().map(Box::new)?;

    // Parse the optional `else` branch.
    let otherwise = match self.peek(1).kind {
      | TokenKind::ElseKw => {
        self.consume()?;
        let otherwise = self.statement().map(Box::new)?;

        Some(otherwise)
      },
      | _ => None,
    };

    let location = Location::merge(
      &if_token.location,
      // If we have an else branch, then merge with its location.
      otherwise
        .as_ref()
        .map_or_else(|| then.location(), |otherwise| otherwise.location()),
    );

    Ok(Statement::If(If {
      condition,
      then,
      otherwise,
      location,
    }))
  }

  /// Parses a return statement.
  fn statement_return(&mut self) -> Result<Statement> {
    self.consume()?;

    let expression = self.expression(0)?;

    self.expect(TokenKind::Semi)?;

    Ok(Statement::Return(expression))
  }

  /// Parses an expression statement.
  fn statement_expression(&mut self) -> Result<Statement> {
    let expression = self.expression(0)?;

    self.expect(TokenKind::Semi)?;

    Ok(Statement::Expression(expression))
  }

  /// Parses a block statement.
  fn statement_block(&mut self) -> Result<Statement> {
    let block = self.block()?;

    Ok(Statement::Block(block))
  }

  /// Parses a 'null statement', i.e. semicolon.
  fn statement_null(&mut self) -> Result<Statement> {
    let token = self.expect(TokenKind::Semi)?;

    Ok(Statement::Null {
      location: token.location,
    })
  }

  /// Parses an optional expression up to the given delimiter, and returns `Option<Expression>` with
  /// the location of either the expression or the delimiter.
  fn optional_expression(
    &mut self,
    delimiter: TokenKind,
  ) -> Result<(Option<Expression>, Location)> {
    if self.peek(1).kind == delimiter {
      let token = self.consume()?;

      Ok((None, token.location))
    } else {
      let expression = self.expression(0)?;
      let token = self.expect(delimiter)?;

      let location = Location::merge(expression.location(), &token.location);

      Ok((Some(expression), location))
    }
  }

  /// Parses an expression.
  fn expression(&mut self, min_precedence: usize) -> Result<Expression> {
    let mut left = self.factor()?;
    let mut next = self.peek(1);

    while let Some(precedence) = Self::precedence(next) {
      if precedence >= min_precedence {
        // Handle assignments differently, because unlike other binary operators they are
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
        if next.is_assignment_op() {
          let token = self.consume()?;
          let right = self.expression(precedence)?;
          let operator = Self::resolve_compound_op(&token)?;
          let location = Location::merge(left.location(), right.location());

          left = match operator {
            | Some(op) => {
              Expression::CompoundAssignment(CompoundAssignment {
                op,
                left: Box::new(left),
                right: Box::new(right),
                location,
              })
            },
            | None => {
              Expression::Assignment(Assignment {
                left: Box::new(left),
                right: Box::new(right),
                location,
              })
            },
          }
        }
        // We special-case ternary operators too.
        //
        // This time we perform a trick: we parse the ternary operator almost like as a binary
        // operator, where the operator in the middle is `"?" <expression> ":"`, i.e. that
        // operator is delimited by `?` and `:` tokens.
        //
        // This way we can assign a precedence relative to other binary operators.
        else if next.is_ternary_op() {
          let middle = self.ternary_middle()?;
          let right = self.expression(precedence)?;

          let location = Location::merge(left.location(), right.location());

          left = Expression::Ternary(Ternary {
            condition: Box::new(left),
            then: Box::new(middle),
            otherwise: Box::new(right),
            location,
          })
        }
        // Otherwise, we just parse the (left-associative) binary operators.
        else {
          let op = self.binary()?;
          let right = self.expression(precedence + 1)?;
          let location = Location::merge(left.location(), right.location());

          left = Expression::Binary(Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            location,
          });
        }

        next = self.peek(1);
      } else {
        break;
      }
    }

    Ok(left)
  }

  /// Parses a factor expression.
  fn factor(&mut self) -> Result<Expression> {
    if self.peek(1).is_unary_op() {
      self.unary()
    } else {
      self.postfix()
    }
  }

  /// Parses a postfix expression.
  fn postfix(&mut self) -> Result<Expression> {
    let mut primary = self.primary()?;

    while self.peek(1).is_postfix_op() {
      let token = self.consume()?;

      let op = Self::resolve_postfix_op(&token)?;
      let location = Location::merge(primary.location(), &token.location);

      primary = Expression::Postfix(Postfix {
        op,
        operand: Box::new(primary),
        location,
      })
    }

    Ok(primary)
  }

  /// Parses the middle (then branch) of a ternary operator.
  fn ternary_middle(&mut self) -> Result<Expression> {
    self.consume()?;
    let condition = self.expression(0)?;
    self.expect(TokenKind::Colon)?;

    Ok(condition)
  }

  /// Parses a primary expression.
  fn primary(&mut self) -> Result<Expression> {
    let token = self.peek(1);

    match token.kind {
      | TokenKind::Int => self.constant(),
      | TokenKind::Ident => self.variable(),
      | TokenKind::ParenOpen => self.group(),
      | _ => {
        Err(ParseError::new(
          format!("expected a primary expression, found '{}'", token.value),
          token.location,
        ))
      },
    }
  }

  /// Parses a variable.
  fn variable(&mut self) -> Result<Expression> {
    let ident = self.identifier()?;

    Ok(Expression::Var(ident))
  }

  /// Parses an expression group.
  fn group(&mut self) -> Result<Expression> {
    self.consume()?;

    let expression = self.expression(0)?;

    self.expect(TokenKind::ParenClose)?;

    Ok(expression)
  }

  /// Parses a constant.
  fn constant(&mut self) -> Result<Expression> {
    let token = self.consume()?;

    let value = token.value.parse().map_err(|_| {
      ParseError::new(
        format!("expected a constant, found '{}'", token.value),
        token.location,
      )
    })?;

    Ok(Expression::Constant(Int {
      value,
      location: token.location,
    }))
  }

  /// Parses an unary expression.
  fn unary(&mut self) -> Result<Expression> {
    let token = self.consume()?;

    let op = match token.kind {
      | TokenKind::BitNot => UnaryOp::BitNot,
      | TokenKind::Bang => UnaryOp::Not,
      | TokenKind::Dec => UnaryOp::Dec,
      | TokenKind::Inc => UnaryOp::Inc,
      | TokenKind::Sub => UnaryOp::Negate,
      | _ => {
        return Err(ParseError::new(
          format!("expected unary operator, found '{}'", token.value),
          token.location,
        ))
      },
    };

    let expression = self.factor()?;
    let location = Location::merge(&token.location, expression.location());

    Ok(Expression::Unary(Unary {
      op,
      expression: Box::new(expression),
      location,
    }))
  }

  /// Parses a binary operator.
  fn binary(&mut self) -> Result<BinaryOp> {
    let token = self.consume()?;

    let op = match token.kind {
      // Arithmetic operators.
      | TokenKind::Add => BinaryOp::Add,
      | TokenKind::Div => BinaryOp::Div,
      | TokenKind::Mod => BinaryOp::Mod,
      | TokenKind::Mul => BinaryOp::Mul,
      | TokenKind::Sub => BinaryOp::Sub,
      // Bitwise operators.
      | TokenKind::BitAnd => BinaryOp::BitAnd,
      | TokenKind::BitOr => BinaryOp::BitOr,
      | TokenKind::BitShl => BinaryOp::BitShl,
      | TokenKind::BitShr => BinaryOp::BitShr,
      | TokenKind::BitXor => BinaryOp::BitXor,
      // Logical operators.
      | TokenKind::And => BinaryOp::And,
      | TokenKind::Equal => BinaryOp::Equal,
      | TokenKind::Greater => BinaryOp::Greater,
      | TokenKind::GreaterEqual => BinaryOp::GreaterEqual,
      | TokenKind::Less => BinaryOp::Less,
      | TokenKind::LessEqual => BinaryOp::LessEqual,
      | TokenKind::NotEqual => BinaryOp::NotEqual,
      | TokenKind::Or => BinaryOp::Or,
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
  fn identifier(&mut self) -> Result<Ident> {
    let token = self.consume()?;

    if token.kind != TokenKind::Ident {
      return Err(ParseError::new(
        format!("expected identifier, found '{}'", token.value),
        token.location,
      ));
    }

    Ok(Ident {
      value: token.value.into(),
      location: token.location,
    })
  }

  /// Returns the precedence of the given token. The higher the precedence number, the higher the
  /// binding power.
  ///
  /// Returns `None` if the token is not a binary, assignment, or ternary operator.
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
      | TokenKind::Question => Some(3),
      | kind if kind.is_assignment_op() => Some(1),
      | _ => None,
    }
  }

  /// Returns the matching binary operator for the given compound assignment token.
  fn resolve_compound_op(token: &Token) -> Result<Option<BinaryOp>> {
    match token.kind {
      // Assignment operator.
      | TokenKind::Assign => Ok(None),
      // Arithmetic operators.
      | TokenKind::AddAssign => Ok(Some(BinaryOp::Add)),
      | TokenKind::SubAssign => Ok(Some(BinaryOp::Sub)),
      | TokenKind::MulAssign => Ok(Some(BinaryOp::Mul)),
      | TokenKind::DivAssign => Ok(Some(BinaryOp::Div)),
      | TokenKind::ModAssign => Ok(Some(BinaryOp::Mod)),
      // Bitwise operators.
      | TokenKind::BitAndAssign => Ok(Some(BinaryOp::BitAnd)),
      | TokenKind::BitOrAssign => Ok(Some(BinaryOp::BitOr)),
      | TokenKind::BitShlAssign => Ok(Some(BinaryOp::BitShl)),
      | TokenKind::BitShrAssign => Ok(Some(BinaryOp::BitShr)),
      | TokenKind::BitXorAssign => Ok(Some(BinaryOp::BitXor)),
      | _ => {
        Err(ParseError::new(
          format!(
            "expected compound assignment operator, found '{}'",
            token.value
          ),
          token.location,
        ))
      },
    }
  }

  /// Returns the matching postfix operator for the given token.
  fn resolve_postfix_op(token: &Token) -> Result<PostfixOp> {
    match token.kind {
      | TokenKind::Inc => Ok(PostfixOp::Inc),
      | TokenKind::Dec => Ok(PostfixOp::Dec),
      | _ => {
        Err(ParseError::new(
          format!("expected postfix operator, found '{}'", token.value),
          token.location,
        ))
      },
    }
  }
}

#[cfg(test)]
mod tests {
  use cyan_reporting::Location;
  use indoc::indoc;

  use super::*;
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

    let expected = Program {
      function: Function {
        name: Ident {
          value: "main".to_string().into(),
          location: Location::default(),
        },
        body: Block {
          body: vec![
            BlockItem::Declaration(Declaration {
              name: Ident {
                value: "a".to_string().into(),
                location: Location::default(),
              },
              initializer: Some(Expression::Constant(Int {
                value: 21,
                location: Location::default(),
              })),
              location: Location::default(),
            }),
            BlockItem::Declaration(Declaration {
              name: Ident {
                value: "b".to_string().into(),
                location: Location::default(),
              },
              initializer: Some(Expression::Constant(Int {
                value: 42,
                location: Location::default(),
              })),
              location: Location::default(),
            }),
            BlockItem::Declaration(Declaration {
              name: Ident {
                value: "c".to_string().into(),
                location: Location::default(),
              },
              initializer: None,
              location: Location::default(),
            }),
            BlockItem::Statement(Statement::Expression(Expression::Assignment(Assignment {
              left: Box::new(Expression::Var(Ident {
                value: "b".to_string().into(),
                location: Location::default(),
              })),
              right: Box::new(Expression::Binary(Binary {
                op: BinaryOp::Div,
                left: Box::new(Expression::Var(Ident {
                  value: "b".to_string().into(),
                  location: Location::default(),
                })),
                right: Box::new(Expression::Constant(Int {
                  value: 2,
                  location: Location::default(),
                })),
                location: Location::default(),
              })),
              location: Location::default(),
            }))),
            BlockItem::Statement(Statement::Expression(Expression::Assignment(Assignment {
              left: Box::new(Expression::Var(Ident {
                value: "c".to_string().into(),
                location: Location::default(),
              })),
              right: Box::new(Expression::Binary(Binary {
                op: BinaryOp::Add,
                left: Box::new(Expression::Var(Ident {
                  value: "a".to_string().into(),
                  location: Location::default(),
                })),
                right: Box::new(Expression::Var(Ident {
                  value: "b".to_string().into(),
                  location: Location::default(),
                })),
                location: Location::default(),
              })),
              location: Location::default(),
            }))),
            BlockItem::Statement(Statement::Return(Expression::Var(Ident {
              value: "c".to_string().into(),
              location: Location::default(),
            }))),
          ],
          location: Location::default(),
        },
        location: Location::default(),
      },
      location: Location::default(),
    };

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_declaration_with_initializer() {
    let actual = parser("int meaning = 42;").declaration();

    let expected = Declaration {
      name: Ident {
        value: "meaning".to_string().into(),
        location: Location::default(),
      },
      initializer: Some(Expression::Constant(Int {
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

    let expected = Declaration {
      name: Ident {
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

    let expected = BlockItem::Declaration(Declaration {
      name: Ident {
        value: "a".to_string().into(),
        location: Location::default(),
      },
      initializer: Some(Expression::Assignment(Assignment {
        left: Box::new(Expression::Var(Ident {
          value: "b".to_string().into(),
          location: Location::default(),
        })),
        right: Box::new(Expression::Assignment(Assignment {
          left: Box::new(Expression::Var(Ident {
            value: "c".to_string().into(),
            location: Location::default(),
          })),
          right: Box::new(Expression::Constant(Int {
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

    let expected =
      BlockItem::Statement(Statement::Expression(Expression::Assignment(Assignment {
        left: Box::new(Expression::Var(Ident {
          value: "meaning".to_string().into(),
          location: Location::default(),
        })),
        right: Box::new(Expression::Constant(Int {
          value: 42,
          location: Location::default(),
        })),
        location: Location::default(),
      })));

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_assignment_statement_to_constant() {
    let actual = parser("2 = a * 42;").block_item();

    let expected =
      BlockItem::Statement(Statement::Expression(Expression::Assignment(Assignment {
        left: Box::new(Expression::Constant(Int {
          value: 2,
          location: Location::default(),
        })),
        right: Box::new(Expression::Binary(Binary {
          op: BinaryOp::Mul,
          left: Box::new(Expression::Var(Ident {
            value: "a".to_string().into(),
            location: Location::default(),
          })),
          right: Box::new(Expression::Constant(Int {
            value: 42,
            location: Location::default(),
          })),
          location: Location::default(),
        })),
        location: Location::default(),
      })));

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_if_statement() {
    let actual = parser("if (a) return 42;").statement();

    let expected = Statement::If(If {
      condition: Box::new(Expression::Var(Ident {
        value: "a".to_string().into(),
        location: Location::default(),
      })),
      then: Box::new(Statement::Return(Expression::Constant(Int {
        value: 42,
        location: Location::default(),
      }))),
      otherwise: None,
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_if_else_statement_block() {
    let actual = parser(indoc! {"
      if (a > 100)
        return 0;
      else if (a > 50)
        return 1;
      else
        return 2;
    "})
    .statement();

    let expected = Statement::If(If {
      condition: Box::new(Expression::Binary(Binary {
        op: BinaryOp::Greater,
        left: Box::new(Expression::Var(Ident {
          value: "a".to_string().into(),
          location: Location::default(),
        })),
        right: Box::new(Expression::Constant(Int {
          value: 100,
          location: Location::default(),
        })),
        location: Location::default(),
      })),
      then: Box::new(Statement::Return(Expression::Constant(Int {
        value: 0,
        location: Location::default(),
      }))),
      otherwise: Some(Box::new(Statement::If(If {
        condition: Box::new(Expression::Binary(Binary {
          op: BinaryOp::Greater,
          left: Box::new(Expression::Var(Ident {
            value: "a".to_string().into(),
            location: Location::default(),
          })),
          right: Box::new(Expression::Constant(Int {
            value: 50,
            location: Location::default(),
          })),
          location: Location::default(),
        })),
        then: Box::new(Statement::Return(Expression::Constant(Int {
          value: 1,
          location: Location::default(),
        }))),
        otherwise: Some(Box::new(Statement::Return(Expression::Constant(Int {
          value: 2,
          location: Location::default(),
        })))),
        location: Location::default(),
      }))),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_if_else_statement_dangling_else() {
    // By C standard, we parse the program below as the following:
    //
    // ```c
    // if (a) {
    //   if (a > 10)
    //     return a;
    //   else
    //     return 10 - a;
    // }
    // ```
    let actual = parser(indoc! {"
      if (a)
        if (a > 10)
          return a;
        else
          return 10 - a;
    "})
    .statement();

    let expected = Statement::If(If {
      condition: Box::new(Expression::Var(Ident {
        value: "a".to_string().into(),
        location: Location::default(),
      })),
      then: Box::new(Statement::If(If {
        condition: Box::new(Expression::Binary(Binary {
          op: BinaryOp::Greater,
          left: Box::new(Expression::Var(Ident {
            value: "a".to_string().into(),
            location: Location::default(),
          })),
          right: Box::new(Expression::Constant(Int {
            value: 10,
            location: Location::default(),
          })),
          location: Location::default(),
        })),
        then: Box::new(Statement::Return(Expression::Var(Ident {
          value: "a".to_string().into(),
          location: Location::default(),
        }))),
        otherwise: Some(Box::new(Statement::Return(Expression::Binary(Binary {
          op: BinaryOp::Sub,
          left: Box::new(Expression::Constant(Int {
            value: 10,
            location: Location::default(),
          })),
          right: Box::new(Expression::Var(Ident {
            value: "a".to_string().into(),
            location: Location::default(),
          })),
          location: Location::default(),
        })))),
        location: Location::default(),
      })),
      otherwise: None,
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_ternary_operator() {
    let actual = parser("a ? b : c").expression(0);

    let expected = Expression::Ternary(Ternary {
      condition: Box::new(Expression::Var(Ident {
        value: "a".to_string().into(),
        location: Location::default(),
      })),
      then: Box::new(Expression::Var(Ident {
        value: "b".to_string().into(),
        location: Location::default(),
      })),
      otherwise: Box::new(Expression::Var(Ident {
        value: "c".to_string().into(),
        location: Location::default(),
      })),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_ternary_operator_associativity() {
    // The program below is equivalent to the following:
    //
    // ```c
    // a ? 1 : (b ? 2 : 3)
    // ```
    let actual = parser("a ? 1 : b ? 2 : 3").expression(0);

    let expected = Expression::Ternary(Ternary {
      // a ? 1 : ...
      condition: Box::new(Expression::Var(Ident {
        value: "a".to_string().into(),
        location: Location::default(),
      })),
      then: Box::new(Expression::Constant(Int {
        value: 1,
        location: Location::default(),
      })),
      // b ? 2 : 3
      otherwise: Box::new(Expression::Ternary(Ternary {
        condition: Box::new(Expression::Var(Ident {
          value: "b".to_string().into(),
          location: Location::default(),
        })),
        then: Box::new(Expression::Constant(Int {
          value: 2,
          location: Location::default(),
        })),
        otherwise: Box::new(Expression::Constant(Int {
          value: 3,
          location: Location::default(),
        })),
        location: Location::default(),
      })),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_ternary_operator_assignment() {
    // The program below is equivalent to the following:
    //
    // ```c
    // a = (1 ? 2 : 3)
    // ```
    let actual = parser("a = 1 ? 2 : 3;").statement();

    let expected = Statement::Expression(Expression::Assignment(Assignment {
      left: Box::new(Expression::Var(Ident {
        value: "a".to_string().into(),
        location: Location::default(),
      })),
      right: Box::new(Expression::Ternary(Ternary {
        condition: Box::new(Expression::Constant(Int {
          value: 1,
          location: Location::default(),
        })),
        then: Box::new(Expression::Constant(Int {
          value: 2,
          location: Location::default(),
        })),
        otherwise: Box::new(Expression::Constant(Int {
          value: 3,
          location: Location::default(),
        })),
        location: Location::default(),
      })),
      location: Location::default(),
    }));

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_ternary_operator_logical_or() {
    // The program below is equivalent to the following:
    //
    // ```c
    // (a || b) ? 1 : 0
    // ```
    let actual = parser("a || b ? 1 : 0").expression(0);

    let expected = Expression::Ternary(Ternary {
      condition: Box::new(Expression::Binary(Binary {
        op: BinaryOp::Or,
        left: Box::new(Expression::Var(Ident {
          value: "a".to_string().into(),
          location: Location::default(),
        })),
        right: Box::new(Expression::Var(Ident {
          value: "b".to_string().into(),
          location: Location::default(),
        })),
        location: Location::default(),
      })),
      then: Box::new(Expression::Constant(Int {
        value: 1,
        location: Location::default(),
      })),
      otherwise: Box::new(Expression::Constant(Int {
        value: 0,
        location: Location::default(),
      })),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_goto_statement() {
    let actual = parser("goto foo;").statement();

    let expected = Statement::Goto(Goto {
      label: Ident {
        value: "foo".to_string().into(),
        location: Location::default(),
      },
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_labeled_statement() {
    let actual = parser("foo: return 42;").statement();

    let expected = Statement::Labeled(Labeled {
      label: Ident {
        value: "foo".to_string().into(),
        location: Location::default(),
      },
      statement: Box::new(Statement::Return(Expression::Constant(Int {
        value: 42,
        location: Location::default(),
      }))),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_labeled_statement_program() {
    let actual = parser(indoc! {"
      int main(void) {
        int x = 0;
      foo:
        x = 1;
        return x;
      }
    "})
    .parse();

    let expected = Program {
      function: Function {
        name: Ident {
          value: "main".to_string().into(),
          location: Location::default(),
        },
        body: Block {
          body: vec![
            BlockItem::Declaration(Declaration {
              name: Ident {
                value: "x".to_string().into(),
                location: Location::default(),
              },
              initializer: Some(Expression::Constant(Int {
                value: 0,
                location: Location::default(),
              })),
              location: Location::default(),
            }),
            BlockItem::Statement(Statement::Labeled(Labeled {
              label: Ident {
                value: "foo".to_string().into(),
                location: Location::default(),
              },
              statement: Box::new(Statement::Expression(Expression::Assignment(Assignment {
                left: Box::new(Expression::Var(Ident {
                  value: "x".to_string().into(),
                  location: Location::default(),
                })),
                right: Box::new(Expression::Constant(Int {
                  value: 1,
                  location: Location::default(),
                })),
                location: Location::default(),
              }))),
              location: Location::default(),
            })),
            BlockItem::Statement(Statement::Return(Expression::Var(Ident {
              value: "x".to_string().into(),
              location: Location::default(),
            }))),
          ],
          location: Location::default(),
        },
        location: Location::default(),
      },
      location: Location::default(),
    };

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_labeled_statement_goto() {
    let actual = parser("foo: goto bar;").statement();

    let expected = Statement::Labeled(Labeled {
      label: Ident {
        value: "foo".to_string().into(),
        location: Location::default(),
      },
      statement: Box::new(Statement::Goto(Goto {
        label: Ident {
          value: "bar".to_string().into(),
          location: Location::default(),
        },
        location: Location::default(),
      })),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_return_statement() {
    let actual = parser("return 42;").statement();

    let expected = Statement::Return(Expression::Constant(Int {
      value: 42,
      location: Location::default(),
    }));

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_break_statement() {
    let actual = parser("break;").statement();

    let expected = Statement::Break(Break {
      label: None,
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_continue_statement() {
    let actual = parser("continue;").statement();

    let expected = Statement::Continue(Continue {
      label: None,
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_while_loop() {
    let actual = parser(indoc! {"
      while (1) {
        break;
      }
    "})
    .statement();

    let expected = Statement::While(While {
      condition: Expression::Constant(Int {
        value: 1,
        location: Location::default(),
      }),
      body: Box::new(Statement::Block(Block {
        body: vec![BlockItem::Statement(Statement::Break(Break {
          label: None,
          location: Location::default(),
        }))],
        location: Location::default(),
      })),
      label: None,
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_do_while_loop() {
    let actual = parser(indoc! {"
      do {
        break;
      } while (1);
    "})
    .statement();

    let expected = Statement::DoWhile(DoWhile {
      condition: Expression::Constant(Int {
        value: 1,
        location: Location::default(),
      }),
      body: Box::new(Statement::Block(Block {
        body: vec![BlockItem::Statement(Statement::Break(Break {
          label: None,
          location: Location::default(),
        }))],
        location: Location::default(),
      })),
      label: None,
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_infinite_for_loop() {
    let actual = parser(indoc! {"
      for (;;) {
        break;
      }
    "})
    .statement();

    let expected = Statement::For(For {
      initializer: Initializer::None {
        location: Location::default(),
      },
      condition: None,
      postcondition: None,
      body: Box::new(Statement::Block(Block {
        body: vec![BlockItem::Statement(Statement::Break(Break {
          label: None,
          location: Location::default(),
        }))],
        location: Location::default(),
      })),
      label: None,
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_for_loop() {
    let actual = parser(indoc! {"
      for (int i = 0; i < 10; i++) {
        break;
      }
    "})
    .statement();

    let expected = Statement::For(For {
      initializer: Initializer::Declaration(Declaration {
        name: Ident {
          value: "i".to_string().into(),
          location: Location::default(),
        },
        initializer: Some(Expression::Constant(Int {
          value: 0,
          location: Location::default(),
        })),
        location: Location::default(),
      }),
      condition: Some(Expression::Binary(Binary {
        op: BinaryOp::Less,
        left: Box::new(Expression::Var(Ident {
          value: "i".to_string().into(),
          location: Location::default(),
        })),
        right: Box::new(Expression::Constant(Int {
          value: 10,
          location: Location::default(),
        })),
        location: Location::default(),
      })),
      postcondition: Some(Expression::Postfix(Postfix {
        op: PostfixOp::Inc,
        operand: Box::new(Expression::Var(Ident {
          value: "i".to_string().into(),
          location: Location::default(),
        })),
        location: Location::default(),
      })),
      body: Box::new(Statement::Block(Block {
        body: vec![BlockItem::Statement(Statement::Break(Break {
          label: None,
          location: Location::default(),
        }))],
        location: Location::default(),
      })),
      label: None,
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_constant() {
    let actual = parser("42").constant();

    let expected = Expression::Constant(Int {
      value: 42,
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_unary() {
    let actual = parser("-42").unary();

    let expected = Expression::Unary(Unary {
      op: UnaryOp::Negate,
      expression: Box::new(Expression::Constant(Int {
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

    let expected = Expression::Unary(Unary {
      op: UnaryOp::Negate,
      expression: Box::new(Expression::Unary(Unary {
        op: UnaryOp::BitNot,
        expression: Box::new(Expression::Unary(Unary {
          op: UnaryOp::Negate,
          expression: Box::new(Expression::Constant(Int {
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
    let actual = parser("!42;").expression(0);

    let expected = Expression::Unary(Unary {
      op: UnaryOp::Not,
      expression: Box::new(Expression::Constant(Int {
        value: 42,
        location: Location::default(),
      })),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_unary_postfix_dec() {
    let actual = parser("a--").expression(0);

    let expected = Expression::Postfix(Postfix {
      op: PostfixOp::Dec,
      operand: Box::new(Expression::Var(Ident {
        value: "a".to_string().into(),
        location: Location::default(),
      })),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_unary_postfix_inc() {
    let actual = parser("a++").expression(0);

    let expected = Expression::Postfix(Postfix {
      op: PostfixOp::Inc,
      operand: Box::new(Expression::Var(Ident {
        value: "a".to_string().into(),
        location: Location::default(),
      })),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_unary_postfix_sequence() {
    let actual = parser("(a--)++").expression(0);

    let expected = Expression::Postfix(Postfix {
      op: PostfixOp::Inc,
      operand: Box::new(Expression::Postfix(Postfix {
        op: PostfixOp::Dec,
        operand: Box::new(Expression::Var(Ident {
          value: "a".to_string().into(),
          location: Location::default(),
        })),
        location: Location::default(),
      })),
      location: Location::default(),
    });

    assert_eq!(actual, Ok(expected));
  }

  #[test]
  fn parse_binary() {
    let actual = parser("1 * 2").expression(0);

    let expected = Expression::Binary(Binary {
      op: BinaryOp::Mul,
      left: Box::new(Expression::Constant(Int {
        value: 1,
        location: Location::default(),
      })),
      right: Box::new(Expression::Constant(Int {
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

    let expected = Expression::Binary(Binary {
      op: BinaryOp::Sub,
      left: Box::new(Expression::Binary(Binary {
        op: BinaryOp::Mul,
        left: Box::new(Expression::Constant(Int {
          value: 1,
          location: Location::default(),
        })),
        right: Box::new(Expression::Constant(Int {
          value: 2,
          location: Location::default(),
        })),
        location: Location::default(),
      })),
      right: Box::new(Expression::Binary(Binary {
        op: BinaryOp::Mul,
        left: Box::new(Expression::Constant(Int {
          value: 3,
          location: Location::default(),
        })),
        right: Box::new(Expression::Binary(Binary {
          op: BinaryOp::Add,
          left: Box::new(Expression::Constant(Int {
            value: 4,
            location: Location::default(),
          })),
          right: Box::new(Expression::Constant(Int {
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

    let expected = Function {
      name: Ident {
        value: "main".to_string().into(),
        location: Location::default(),
      },
      body: Block {
        body: vec![],
        location: Location::default(),
      },
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
