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

use internment::Intern;
use thiserror::Error;

use super::tac;
use crate::span::Span;

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
  pub function: Function,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function {
  pub name: Intern<String>,
  pub instructions: Vec<Instruction>,
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operand {
  Imm(isize),
  Reg(Reg),
  Pseudo(Intern<String>),
  Stack(isize),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Reg {
  AX,
  R10,
}

#[derive(Debug, Error)]
#[error("TAC lowering error [{span}]: {message}")]
pub struct LoweringError {
  /// The error message.
  pub message: String,
  /// The span of the error.
  pub span: Span,
}

impl LoweringError {
  pub fn new(message: impl AsRef<str> + Into<String>, span: Span) -> Self {
    Self {
      message: message.into(),
      span,
    }
  }
}

pub struct Lowerer;

impl Lowerer {
  pub fn new() -> Self {
    Self
  }

  pub fn lower(&self, program: &tac::Program) -> Result<Program, LoweringError> {
    self.lower_program(program)
  }

  fn lower_program(&self, program: &tac::Program) -> Result<Program, LoweringError> {
    let function = self.lower_function(&program.function)?;

    Ok(Program { function })
  }

  fn lower_function(&self, function: &tac::Function) -> Result<Function, LoweringError> {
    let name = function.name;
    let instructions = self.lower_instructions(&function.instructions)?;

    Ok(Function { name, instructions })
  }

  fn lower_instructions(
    &self,
    tac_instructions: &[tac::Instruction],
  ) -> Result<Vec<Instruction>, LoweringError> {
    let mut instructions = Vec::new();

    for instruction in tac_instructions {
      match instruction {
        | tac::Instruction::Return { value } => {
          let src = self.lower_value(value);
          let dst = Operand::Reg(Reg::AX);

          instructions.push(Instruction::Mov { src, dst });
          instructions.push(Instruction::Ret);
        },
        | tac::Instruction::Unary { operator, src, dst } => {
          let operator = self.lower_operator(operator);
          let src = self.lower_value(src);
          let dst = self.lower_value(dst);

          instructions.push(Instruction::Mov { src, dst });
          instructions.push(Instruction::Unary {
            operator,
            operand: dst,
          });
        },
      }
    }

    Ok(instructions)
  }

  fn lower_value(&self, value: &tac::Value) -> Operand {
    match value {
      | tac::Value::Constant(int) => Operand::Imm(*int),
      | tac::Value::Var(identifier) => Operand::Pseudo(*identifier),
    }
  }

  fn lower_operator(&self, operator: &tac::UnaryOp) -> UnaryOp {
    match operator {
      | tac::UnaryOp::BitwiseNot => UnaryOp::Not,
      | tac::UnaryOp::Negate => UnaryOp::Neg,
    }
  }
}
