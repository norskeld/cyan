use thiserror::Error;

use crate::ir::ast;
use crate::ir::tac::*;
use crate::location::Location;

#[derive(Debug, Error)]
#[error("AST lowering error {location}: {message}")]
pub struct LoweringError {
  /// The error message.
  pub message: String,
  /// The location of the error.
  pub location: Location,
}

impl LoweringError {
  pub fn new(message: impl AsRef<str> + Into<String>, location: Location) -> Self {
    Self {
      message: message.into(),
      location,
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

  /// Lowers AST to TAC.
  pub fn lower(&mut self, program: &ast::Program) -> Result<Program, LoweringError> {
    self.lower_program(program)
  }

  /// Lowers an AST program to a TAC equivalent.
  fn lower_program(&mut self, program: &ast::Program) -> Result<Program, LoweringError> {
    let function = self.lower_function(&program.function)?;

    Ok(Program { function })
  }

  /// Lowers a function by converting its body to a flat sequence of instructions.
  fn lower_function(&mut self, function: &ast::Function) -> Result<Function, LoweringError> {
    // Set the prefix for temporary variables to current function's name so that emitted
    // instructions' temporary variables names are "scoped".
    self.var_prefix = function.name.value;

    let mut instructions = Vec::new();

    self.emit_statement(&function.body, &mut instructions)?;

    Ok(Function {
      name: function.name.value,
      instructions,
    })
  }

  /// Emits instructions for statements.
  fn emit_statement(
    &mut self,
    statement: &ast::Statement,
    instructions: &mut Vec<Instruction>,
  ) -> Result<(), LoweringError> {
    match statement {
      | ast::Statement::Return(expression) => {
        let value = self.emit_expression(expression, instructions)?;
        instructions.push(Instruction::Return(value));

        Ok(())
      },
    }
  }

  /// Emits instructions for expressions.
  fn emit_expression(
    &mut self,
    expression: &ast::Expression,
    instructions: &mut Vec<Instruction>,
  ) -> Result<Value, LoweringError> {
    // `Value`s are Copy, so we don't need to `clone` them, they'll be copied.
    match expression {
      | ast::Expression::Constant(int) => Ok(Value::Constant(int.value)),
      | ast::Expression::Unary(unary) => self.emit_unary(unary, instructions),
      | ast::Expression::Binary(binary) if binary.is_and() => self.emit_and(binary, instructions),
      | ast::Expression::Binary(binary) if binary.is_or() => self.emit_or(binary, instructions),
      | ast::Expression::Binary(binary) => self.emit_binary(binary, instructions),
    }
  }

  /// Emits instructions for unary operators.
  fn emit_unary(
    &mut self,
    unary: &ast::Unary,
    instructions: &mut Vec<Instruction>,
  ) -> Result<Value, LoweringError> {
    let op = self.lower_unary_op(&unary.op);
    let src = self.emit_expression(&unary.expression, instructions)?;
    let dst = Value::Var(self.gen_temporary());

    instructions.push(Instruction::Unary { op, src, dst });

    Ok(dst)
  }

  /// Emits instructions for binary operators.
  fn emit_binary(
    &mut self,
    binary: &ast::Binary,
    instructions: &mut Vec<Instruction>,
  ) -> Result<Value, LoweringError> {
    let op = self.lower_binary_op(binary)?;
    let left = self.emit_expression(&binary.left, instructions)?;
    let right = self.emit_expression(&binary.right, instructions)?;
    let dst = Value::Var(self.gen_temporary());

    instructions.push(Instruction::Binary {
      op,
      left,
      right,
      dst,
    });

    Ok(dst)
  }

  /// Emits instructions for the `&&` operator.
  ///
  /// We start by evaluating left subexpression. If it’s 0, we short-circuit and set result to 0,
  /// without evaluating right subexpression. To accomplish this we use the JumpIfZero instruction;
  /// if left value is 0, we jump straight to false_label, then set result to 0 with the
  /// Copy(0, result) instruction.
  ///
  /// If right value isn’t 0, we still need to evaluate right expression. We handle the case where
  /// right value is 0 exactly like the case where left value is 0, by jumping to false_label with
  /// JumpIfZero. We reach the Copy(1, result) instruction only if we didn’t take either
  /// conditional jump. That means both left and right expressions are non-zero, so we set result
  /// to 1 (Copy(1, result)). Then, we jump over Copy(0, result) to the end_label to avoid
  /// overwriting result.
  ///
  /// Pseudocode:
  ///
  /// ```plaintext,ignore
  /// <left instructions>
  /// left_value = <left result>
  /// JumpIfZero(left_value, false_label)
  /// <right instructions>
  /// right_value = <right result>
  /// JumpIfZero(v2, false_label)
  /// Copy(1, result)
  /// Jump(end)
  /// Label(false_label)
  /// Copy(0, result)
  /// Label(end)
  /// ```
  fn emit_and(
    &mut self,
    binary: &ast::Binary,
    instructions: &mut Vec<Instruction>,
  ) -> Result<Value, LoweringError> {
    // We don't push instructions emitted for subexpressions straight to the main instructions
    // vector, because we need to interleave them with the jump instructions.
    let mut left_instructions = vec![];
    let mut right_instructions = vec![];

    let left = self.emit_expression(&binary.left, &mut left_instructions)?;
    let right = self.emit_expression(&binary.right, &mut right_instructions)?;

    // Generate jump labels.
    let false_label = self.gen_label("and_false");
    let end_label = self.gen_label("and_end");

    let dst = Value::Var(self.gen_temporary());

    // Emit instructions for left subexpression.
    instructions.extend(left_instructions);
    instructions.extend([Instruction::JumpIfZero {
      condition: left,
      label: false_label,
    }]);

    // Emit instructions for right subexpression.
    instructions.extend(right_instructions);
    instructions.extend([
      Instruction::JumpIfZero {
        condition: right,
        label: false_label,
      },
      Instruction::Copy {
        src: Value::Constant(1),
        dst,
      },
      Instruction::Jump(end_label),
      Instruction::Label(false_label),
      Instruction::Copy {
        src: Value::Constant(0),
        dst,
      },
      Instruction::Label(end_label),
    ]);

    Ok(dst)
  }

  /// Emits instructions for the `||` operator.
  ///
  /// It works basically the same as the `&&` operator, but we use the
  /// [JumpIfNotZero][Instruction::JumpIfNotZero] instruction instead of
  /// [JumpIfZero][Instruction::JumpIfZero] and invert conditions.
  fn emit_or(
    &mut self,
    binary: &ast::Binary,
    instructions: &mut Vec<Instruction>,
  ) -> Result<Value, LoweringError> {
    // We don't push instructions emitted for subexpressions straight to the main instructions
    // vector, because we need to interleave them with the jump instructions.
    let mut left_instructions = vec![];
    let mut right_instructions = vec![];

    let left = self.emit_expression(&binary.left, &mut left_instructions)?;
    let right = self.emit_expression(&binary.right, &mut right_instructions)?;

    // Generate jump labels.
    let true_label = self.gen_label("or_true");
    let end_label = self.gen_label("or_end");

    let dst = Value::Var(self.gen_temporary());

    // Emit instructions for left subexpression.
    instructions.extend(left_instructions);
    instructions.extend([Instruction::JumpIfNotZero {
      condition: left,
      label: true_label,
    }]);

    // Emit instructions for right subexpression.
    instructions.extend(right_instructions);
    instructions.extend([
      Instruction::JumpIfNotZero {
        condition: right,
        label: true_label,
      },
      Instruction::Copy {
        src: Value::Constant(0),
        dst,
      },
      Instruction::Jump(end_label),
      Instruction::Label(true_label),
      Instruction::Copy {
        src: Value::Constant(1),
        dst,
      },
      Instruction::Label(end_label),
    ]);

    Ok(dst)
  }

  /// Generates a label name using `label` as a prefix and increments the counter.
  fn gen_label(&mut self, label: &str) -> Intern<String> {
    let label = format!("{}.{}", self.var_prefix, label);
    self.var_counter += 1;

    label.into()
  }

  /// Generates a temporary variable name scoped to the current function and increments the counter.
  fn gen_temporary(&mut self) -> Intern<String> {
    let name = format!("{}.{}", self.var_prefix, self.var_counter);
    self.var_counter += 1;

    name.into()
  }

  /// Lowers an unary operator to its TAC equivalent.
  fn lower_unary_op(&self, op: &ast::UnaryOp) -> UnaryOp {
    match op {
      // Arithmetics operators.
      | ast::UnaryOp::Negate => UnaryOp::Negate,
      // Bitwise operators.
      | ast::UnaryOp::BitNot => UnaryOp::BitNot,
      // Logical operators.
      | ast::UnaryOp::Not => UnaryOp::Not,
    }
  }

  /// Lowers a binary operator to its TAC equivalent.
  fn lower_binary_op(&self, binary: &ast::Binary) -> Result<BinaryOp, LoweringError> {
    match binary.op {
      // Arithmetics operators.
      | ast::BinaryOp::Add => Ok(BinaryOp::Add),
      | ast::BinaryOp::Div => Ok(BinaryOp::Div),
      | ast::BinaryOp::Mod => Ok(BinaryOp::Mod),
      | ast::BinaryOp::Mul => Ok(BinaryOp::Mul),
      | ast::BinaryOp::Sub => Ok(BinaryOp::Sub),
      // Bitwise operators.
      | ast::BinaryOp::BitAnd => Ok(BinaryOp::BitAnd),
      | ast::BinaryOp::BitOr => Ok(BinaryOp::BitOr),
      | ast::BinaryOp::BitShl => Ok(BinaryOp::BitShl),
      | ast::BinaryOp::BitShr => Ok(BinaryOp::BitShr),
      | ast::BinaryOp::BitXor => Ok(BinaryOp::BitXor),
      // Logical operators.
      | ast::BinaryOp::Equal => Ok(BinaryOp::Equal),
      | ast::BinaryOp::Greater => Ok(BinaryOp::Greater),
      | ast::BinaryOp::GreaterEqual => Ok(BinaryOp::GreaterEqual),
      | ast::BinaryOp::Less => Ok(BinaryOp::Less),
      | ast::BinaryOp::LessEqual => Ok(BinaryOp::LessEqual),
      | ast::BinaryOp::NotEqual => Ok(BinaryOp::NotEqual),
      | ast::BinaryOp::And | ast::BinaryOp::Or => {
        Err(LoweringError::new(
          "|| and && cannot be directly lowered to TAC",
          binary.location.clone(),
        ))
      },
    }
  }
}
