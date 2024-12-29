//! Assembly AST (AAST) definition.

mod passes;

use std::fmt;

use internment::Intern;
pub use passes::*;

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
  pub function: Function,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function {
  pub name: Intern<String>,
  pub instructions: Vec<Instruction>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instruction {
  Mov {
    src: Operand,
    dst: Operand,
  },
  Unary {
    operator: UnaryOp,
    operand: Operand,
  },
  Binary {
    operator: BinaryOp,
    src: Operand,
    dst: Operand,
  },
  Idiv(Operand),
  AllocateStack(isize),
  Cdq,
  Ret,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
  Neg,
  Not,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
  Add,
  Sub,
  Mult,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Operand {
  Imm(isize),
  Reg(Reg),
  Pseudo(Intern<String>),
  Stack(isize),
}

impl fmt::Debug for Operand {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      | Operand::Imm(int) => write!(f, "Imm({int})"),
      | Operand::Reg(reg) => write!(f, "{reg:?}"),
      | Operand::Stack(offset) => write!(f, "Stack({offset})"),
      | Operand::Pseudo(identifier) => write!(f, "Pseudo({identifier})"),
    }
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Reg {
  AX,
  DX,
  R10,
  R11,
}
