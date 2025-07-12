//! Three Address Code (TAC) definition.

mod passes;

use std::fmt;

pub use passes::*;

use crate::symbol::Symbol;

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
  pub definitions: Vec<Function>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
  pub name: Symbol,
  pub params: Vec<Symbol>,
  pub instructions: Vec<Instruction>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
  Return(Value),
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
  Copy {
    src: Value,
    dst: Value,
  },
  Jump(Symbol),
  JumpIfZero {
    condition: Value,
    label: Symbol,
  },
  JumpIfNotZero {
    condition: Value,
    label: Symbol,
  },
  JumpIfEqual {
    left: Value,
    right: Value,
    label: Symbol,
  },
  Label(Symbol),
  FuncCall {
    name: Symbol,
    args: Vec<Value>,
    dst: Value,
  },
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Value {
  Constant(isize),
  Var(Symbol),
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
  Not,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
  // Arithmetics operators.
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
  // Relational operators.
  Equal,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  NotEqual,
}

impl fmt::Display for BinaryOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      | BinaryOp::Add => write!(f, "+"),
      | BinaryOp::Div => write!(f, "/"),
      | BinaryOp::Mod => write!(f, "%"),
      | BinaryOp::Mul => write!(f, "*"),
      | BinaryOp::Sub => write!(f, "-"),
      | BinaryOp::BitAnd => write!(f, "&"),
      | BinaryOp::BitOr => write!(f, "|"),
      | BinaryOp::BitShl => write!(f, "<<"),
      | BinaryOp::BitShr => write!(f, ">>"),
      | BinaryOp::BitXor => write!(f, "^"),
      | BinaryOp::Equal => write!(f, "=="),
      | BinaryOp::Greater => write!(f, ">"),
      | BinaryOp::GreaterEqual => write!(f, ">="),
      | BinaryOp::Less => write!(f, "<"),
      | BinaryOp::LessEqual => write!(f, "<="),
      | BinaryOp::NotEqual => write!(f, "!="),
    }
  }
}
