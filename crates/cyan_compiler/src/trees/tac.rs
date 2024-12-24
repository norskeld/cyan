//! Three Address Code (TAC) definition.
//!
//! ```zephyr
//! program = Program(function)
//!
//! function =
//!   | Function(identifier, instruction* body)
//!
//! instruction =
//!   | Return(value)
//!   | Unary(unary_op, value src, value dst)
//!
//! value =
//!   | Constant(int)
//!   | Var(identifier)
//!
//! unary_op =
//!   | BitwiseNot
//!   | Negate
//! ```

use internment::Intern;
use thiserror::Error;

use super::ast;
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
  Return {
    value: Value,
  },
  Unary {
    operator: UnaryOp,
    src: Value,
    dst: Value,
  },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Value {
  Constant(isize),
  Var(Intern<String>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
  BitwiseNot,
  Negate,
}

#[derive(Debug, Error)]
#[error("AST lowering error [{span}]: {message}")]
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

/// AST to TAC lowerer.
pub struct Lowerer {
  /// The global counter for temporary variables.
  var_counter: usize,
  /// The prefix for temporary variables, equals to function's name.
  var_prefix: Intern<String>,
}

impl Lowerer {
  pub fn new() -> Self {
    Self {
      var_counter: 0,
      var_prefix: "global".to_string().into(),
    }
  }

  pub fn lower(&mut self, program: &ast::Program) -> Result<Program, LoweringError> {
    self.lower_program(program)
  }

  fn lower_program(&mut self, program: &ast::Program) -> Result<Program, LoweringError> {
    let function = self.lower_function(&program.function)?;

    Ok(Program { function })
  }

  fn lower_function(&mut self, function: &ast::Function) -> Result<Function, LoweringError> {
    // Set the prefix for temporary variables to current function's name so that emitted
    // instructions' temporary variables names are "scoped".
    self.var_prefix = function.name.value;

    let name = function.name.value;
    let mut instructions = Vec::new();

    self.emit_statement_instructions(&function.body, &mut instructions)?;

    Ok(Function { name, instructions })
  }
}

impl Lowerer {
  fn emit_statement_instructions(
    &mut self,
    statement: &ast::Statement,
    instructions: &mut Vec<Instruction>,
  ) -> Result<(), LoweringError> {
    match statement {
      | ast::Statement::Return(expression) => {
        let value = self.emit_tac(expression, instructions);
        instructions.push(Instruction::Return { value });

        Ok(())
      },
    }
  }

  fn emit_tac(
    &mut self,
    expression: &ast::Expression,
    instructions: &mut Vec<Instruction>,
  ) -> Value {
    match expression {
      | ast::Expression::Constant(int) => Value::Constant(int.value),
      | ast::Expression::Unary(unary) => {
        let operator = self.make_unary_op(&unary.operator);
        let src = self.emit_tac(&unary.expression, instructions);
        let dst = Value::Var(self.make_temporary().into());

        // `dst` contains interned value, so we don't need to `clone` it, it'll be copied.
        instructions.push(Instruction::Unary { operator, src, dst });

        dst
      },
    }
  }

  fn make_temporary(&mut self) -> String {
    let name = format!("{}.{}", self.var_prefix, self.var_counter);
    self.var_counter += 1;

    name
  }

  fn make_unary_op(&self, operator: &ast::UnaryOp) -> UnaryOp {
    match operator {
      | ast::UnaryOp::BitwiseNot => UnaryOp::BitwiseNot,
      | ast::UnaryOp::Negate => UnaryOp::Negate,
    }
  }
}
