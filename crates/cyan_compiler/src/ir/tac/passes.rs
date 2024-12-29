use thiserror::Error;

use crate::ir::ast;
use crate::ir::tac::*;

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

/// Pass to transform (lower) AST to TAC.
///
/// This pass lowers AST to TAC by either transforming or compacting each AST node into TAC nodes.
pub struct LoweringPass {
  /// The global counter for temporary variables.
  var_counter: usize,
  /// The prefix for temporary variables, equals to function's name.
  var_prefix: Intern<String>,
}

impl LoweringPass {
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

    let mut instructions = Vec::new();

    self.emit_statement_instructions(&function.body, &mut instructions)?;

    Ok(Function {
      name: function.name.value,
      instructions,
    })
  }

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
    // `Value`s are Copy, so we don't need to `clone` them, they'll be copied.
    match expression {
      | ast::Expression::Constant(int) => Value::Constant(int.value),
      | ast::Expression::Unary(unary) => {
        let op = self.make_unary_op(&unary.op);
        let src = self.emit_tac(&unary.expression, instructions);
        let dst = Value::Var(self.make_temporary().into());

        instructions.push(Instruction::Unary { op, src, dst });

        dst
      },
      | ast::Expression::Binary(binary) => {
        let op = self.make_binary_op(&binary.op);
        let left = self.emit_tac(&binary.left, instructions);
        let right = self.emit_tac(&binary.right, instructions);
        let dst = Value::Var(self.make_temporary().into());

        instructions.push(Instruction::Binary {
          op,
          left,
          right,
          dst,
        });

        dst
      },
    }
  }

  fn make_temporary(&mut self) -> String {
    let name = format!("{}.{}", self.var_prefix, self.var_counter);
    self.var_counter += 1;

    name
  }

  fn make_unary_op(&self, op: &ast::UnaryOp) -> UnaryOp {
    match op {
      | ast::UnaryOp::BitNot => UnaryOp::BitNot,
      | ast::UnaryOp::Negate => UnaryOp::Negate,
    }
  }

  fn make_binary_op(&self, op: &ast::BinaryOp) -> BinaryOp {
    match op {
      | ast::BinaryOp::Add => BinaryOp::Add,
      | ast::BinaryOp::Sub => BinaryOp::Sub,
      | ast::BinaryOp::Mul => BinaryOp::Mul,
      | ast::BinaryOp::Div => BinaryOp::Div,
      | ast::BinaryOp::Mod => BinaryOp::Mod,
    }
  }
}
