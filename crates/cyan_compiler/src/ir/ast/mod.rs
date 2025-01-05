//! AST definition.

use cyan_reporting::{Located, Location};
use internment::Intern;

#[derive(Debug, Located, PartialEq, Eq)]
pub struct Program {
  pub function: Function,
  pub location: Location,
}

#[derive(Debug, Located, PartialEq, Eq)]
pub struct Function {
  pub name: Ident,
  pub body: Vec<BlockItem>,
  pub location: Location,
}

#[derive(Debug, Located, PartialEq, Eq)]
pub enum BlockItem {
  Declaration(Declaration),
  Statement(Statement),
}

#[derive(Debug, Located, PartialEq, Eq)]
pub struct Declaration {
  pub name: Ident,
  pub initializer: Option<Expression>,
  pub location: Location,
}

#[derive(Debug, Located, PartialEq, Eq)]
pub enum Statement {
  Return(Expression),
  Expression(Expression),
  Null { location: Location },
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub enum Expression {
  Constant(Int),
  Var(Ident),
  Unary(Unary),
  Binary(Binary),
  Assignment(Assignment),
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct Unary {
  pub op: UnaryOp,
  pub expression: Box<Expression>,
  pub location: Location,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
  // Arithmetic operators.
  Negate,
  // Bitwise operators.
  BitNot,
  // Logical operators.
  Not,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct Binary {
  pub op: BinaryOp,
  pub left: Box<Expression>,
  pub right: Box<Expression>,
  pub location: Location,
}

impl Binary {
  pub fn is_and(&self) -> bool {
    matches!(self.op, BinaryOp::And)
  }

  pub fn is_or(&self) -> bool {
    matches!(self.op, BinaryOp::Or)
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
  // Arithmetic operators.
  Add,
  Div,
  Mod,
  Mul,
  Sub,
  // Bitwise operators.
  BitAnd,
  BitOr,
  BitShl,
  BitShr,
  BitXor,
  // Logical and relational operators.
  And,
  Equal,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  NotEqual,
  Or,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct Assignment {
  pub left: Box<Expression>,
  pub right: Box<Expression>,
  pub location: Location,
}

#[derive(Clone, Copy, Debug, Located, PartialEq, Eq)]
pub struct Int {
  pub value: isize,
  pub location: Location,
}

#[derive(Clone, Copy, Debug, Located, PartialEq, Eq)]
pub struct Ident {
  pub value: Intern<String>,
  pub location: Location,
}
