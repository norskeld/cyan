//! Assembly AST (AAST) definition using Zephyr ASDL:
//!
//! ```zephyr
//! program     = Program(function)
//! function    = Function(identifier name, instruction* instructions)
//! instruction = Mov(operand src, operand dst) | Ret
//! operand     = Imm(int) | Register
//! ```

use internment::Intern;
use thiserror::Error;

use super::ast;
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
  Ret,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operand {
  Imm(isize),
  Register,
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

pub struct Lowerer {
  /// The program to lower.
  program: ast::Program,
}

impl Lowerer {
  pub fn new(program: ast::Program) -> Self {
    Self { program }
  }

  pub fn lower(&self) -> Result<Program, LoweringError> {
    self.lower_program(&self.program)
  }

  fn lower_program(&self, program: &ast::Program) -> Result<Program, LoweringError> {
    let function = self.lower_function(&program.function)?;

    Ok(Program { function })
  }

  fn lower_function(&self, function: &ast::Function) -> Result<Function, LoweringError> {
    let name = function.name.value;
    let instructions = self.lower_statement(&function.body)?;

    Ok(Function { name, instructions })
  }

  fn lower_statement(&self, statement: &ast::Statement) -> Result<Vec<Instruction>, LoweringError> {
    match statement {
      | ast::Statement::Return(expression) => self.lower_expression(expression),
    }
  }

  fn lower_expression(
    &self,
    expression: &ast::Expression,
  ) -> Result<Vec<Instruction>, LoweringError> {
    match expression {
      | ast::Expression::Constant(int) => self.lower_int(int),
      | ast::Expression::Unary(unary) => unimplemented!("unary: {unary:?}"),
    }
  }

  fn lower_int(&self, int: &ast::Int) -> Result<Vec<Instruction>, LoweringError> {
    Ok(vec![
      Instruction::Mov {
        src: Operand::Imm(int.value),
        dst: Operand::Register,
      },
      Instruction::Ret,
    ])
  }
}
