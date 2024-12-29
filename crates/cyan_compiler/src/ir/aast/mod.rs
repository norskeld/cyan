//! Assembly AST (AAST).
//!
//! ```zephyr
//! program = Program(function)
//!
//! function =
//!   | Function(identifier name, instruction* instructions)
//!
//! instruction =
//!   | Mov(operand src, operand dst)
//!   | Unary(unary_op, operand)
//!   | AllocateStack(int)
//!   | Ret
//!
//! unary_op =
//!   | Neg
//!   | Not
//!
//! operand =
//!   | Imm(int)
//!   | Reg(reg)
//!   | Pseudo(identifier)
//!   | Stack(int)
//!
//! reg =
//!   | AX
//!   | R10
//! ```

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
  Mov { src: Operand, dst: Operand },
  Unary { operator: UnaryOp, operand: Operand },
  AllocateStack(isize),
  Ret,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
  Neg,
  Not,
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
  R10,
}
