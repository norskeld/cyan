//! Three Address Code (TAC) definition.

mod passes;

use std::fmt;

use internment::Intern;
pub use passes::*;

use crate::span::Span;

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
  pub function: Function,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
  pub name: Intern<String>,
  pub instructions: Vec<Instruction>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instruction {
  Return {
    value: Value,
  },
  Unary {
    op: UnaryOp,
    src: Value,
    dst: Value,
  },
  Binary {
    op: BinaryOp,
    left: Value,
    right: Value,
    dst: Value,
  },
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Value {
  Constant(isize),
  Var(Intern<String>),
}

impl fmt::Debug for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      | Value::Constant(int) => write!(f, "Constant({int})"),
      | Value::Var(identifier) => write!(f, "Var({identifier})"),
    }
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
  BitNot,
  Negate,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
}
