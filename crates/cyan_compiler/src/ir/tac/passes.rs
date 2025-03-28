use cyan_reporting::{Located, Location};
use thiserror::Error;

use crate::context::Context;
use crate::ir::ast;
use crate::ir::tac::*;

pub type Result<T> = std::result::Result<T, LoweringError>;

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
pub struct LoweringPass<'ctx> {
  ctx: &'ctx mut Context,
}

impl<'ctx> LoweringPass<'ctx> {
  pub fn new(ctx: &'ctx mut Context) -> Self {
    Self { ctx }
  }

  /// Lowers AST to TAC.
  pub fn lower(&mut self, program: &ast::Program) -> Result<Program> {
    self.lower_program(program)
  }

  /// Lowers an AST program to a TAC equivalent.
  fn lower_program(&mut self, program: &ast::Program) -> Result<Program> {
    let function = self.lower_function(&program.function)?;

    Ok(Program { function })
  }

  /// Lowers a function by converting its body to a flat sequence of instructions.
  fn lower_function(&mut self, function: &ast::Function) -> Result<Function> {
    // Set the prefix for temporary variables to current function's name so that emitted
    // instructions' temporary variables names are "scoped".
    self.ctx.var_prefix = function.name.value;

    let mut instructions = Vec::new();

    self.emit_block(&function.body, &mut instructions)?;

    // Shove a return instruction at the end of the function.
    instructions.push(Instruction::Return(Value::Constant(0)));

    Ok(Function {
      name: function.name.value,
      instructions,
    })
  }

  /// Emits instructions for blocks.
  fn emit_block(&mut self, block: &ast::Block, instructions: &mut Vec<Instruction>) -> Result<()> {
    for block_item in &block.body {
      self.emit_block_item(block_item, instructions)?;
    }

    Ok(())
  }

  /// Emits instructions for block item.
  fn emit_block_item(
    &mut self,
    block_item: &ast::BlockItem,
    instructions: &mut Vec<Instruction>,
  ) -> Result<()> {
    match block_item {
      | ast::BlockItem::Declaration(declaration) => {
        self.emit_declaration(declaration, instructions)
      },
      | ast::BlockItem::Statement(statement) => self.emit_statement(statement, instructions),
    }
  }

  /// Emits instructions for declarations.
  fn emit_declaration(
    &mut self,
    declaration: &ast::Declaration,
    instructions: &mut Vec<Instruction>,
  ) -> Result<()> {
    // Treating declaration with initializer like an assignment expression. This is suboptimal, but
    // will do for now.
    if let Some(right) = &declaration.initializer {
      let left = Box::new(ast::Expression::Var(declaration.name));
      let right = Box::new(right.clone());

      let expression = ast::Expression::Assignment(ast::Assignment {
        left,
        right,
        location: declaration.location,
      });

      self.emit_expression(&expression, instructions)?;
    }

    Ok(())
  }

  /// Emits instructions for statements.
  fn emit_statement(
    &mut self,
    statement: &ast::Statement,
    instructions: &mut Vec<Instruction>,
  ) -> Result<()> {
    match statement {
      | ast::Statement::Return(expression) => {
        let value = self.emit_expression(expression, instructions)?;
        instructions.push(Instruction::Return(value));

        Ok(())
      },
      | ast::Statement::Expression(expression) => {
        self.emit_expression(expression, instructions)?;

        Ok(())
      },
      | ast::Statement::If(conditional) => self.emit_if(conditional, instructions),
      | ast::Statement::Goto(goto) => self.emit_goto(goto, instructions),
      | ast::Statement::Labeled(labeled) => self.emit_labeled(labeled, instructions),
      | ast::Statement::Block(block) => self.emit_block(block, instructions),
      | ast::Statement::For(for_) => self.emit_for_loop(for_, instructions),
      | ast::Statement::While(while_) => self.emit_while_loop(while_, instructions),
      | ast::Statement::DoWhile(do_while) => self.emit_do_while_loop(do_while, instructions),
      | ast::Statement::Break(break_) => self.emit_break(break_, instructions),
      | ast::Statement::Continue(continue_) => self.emit_continue(continue_, instructions),
      | ast::Statement::Switch(switch) => self.emit_switch(switch, instructions),
      | ast::Statement::Case(case) => self.emit_case(case, instructions),
      | ast::Statement::DefaultCase(case) => self.emit_default_case(case, instructions),
      | ast::Statement::Null { .. } => Ok(()),
    }
  }

  /// Emits instructions for `switch`.
  ///
  /// Switches are lowered to the following TAC:
  ///
  /// ```plaintext,ignore
  /// control = <control result>
  /// JumpIfEqual(control, case1, label1)
  /// JumpIfEqual(control, case2, label2)
  /// JumpIfEqual(control, caseN, labelN)
  /// Jump(default_label)
  /// Jump(break_label)
  /// <body instructions>
  /// Label(break_label)
  /// ```
  fn emit_switch(
    &mut self,
    switch: &ast::Switch,
    instructions: &mut Vec<Instruction>,
  ) -> Result<()> {
    if let Some(switch_label) = switch.switch_label {
      // Labels.
      let break_label = break_label(switch_label);

      // Evaluate control expression.
      let control = self.emit_expression(&switch.control, instructions)?;

      // Evaluate case expressions and push instructions.
      for (key, label) in &switch.cases {
        // If Some, then this is a case statement.
        if let Some(result) = key {
          instructions.push(Instruction::JumpIfEqual {
            left: control,
            right: Value::Constant(*result),
            label: *label,
          });
        }
      }

      // Push jump to default label if present.
      if let Some(entry) = switch.cases.get(&None) {
        instructions.push(Instruction::Jump(*entry))
      }

      // Push jump to break label.
      instructions.push(Instruction::Jump(break_label));

      // Evaluate body and push instructions.
      self.emit_statement(&switch.body, instructions)?;

      // Push break label.
      instructions.push(Instruction::Label(break_label));

      Ok(())
    } else {
      Err(LoweringError::new(
        "switch statement has no associated label id",
        switch.location,
      ))
    }
  }

  /// Emits instructions for `case` statements.
  fn emit_case(&mut self, case: &ast::Case, instructions: &mut Vec<Instruction>) -> Result<()> {
    if let Some(label) = case.switch_label {
      instructions.push(Instruction::Label(label));

      self.emit_statement(&case.body, instructions)
    } else {
      Err(LoweringError::new(
        "case statement has no associated label id",
        case.location,
      ))
    }
  }

  /// Emits instructions for `default` case statements.
  fn emit_default_case(
    &mut self,
    case: &ast::DefaultCase,
    instructions: &mut Vec<Instruction>,
  ) -> Result<()> {
    if let Some(label) = case.switch_label {
      instructions.push(Instruction::Label(label));

      self.emit_statement(&case.body, instructions)
    } else {
      Err(LoweringError::new(
        "default case statement has no associated label id",
        case.location,
      ))
    }
  }

  /// Emits instructions for `do-while` loops.
  ///
  /// `do-while` loops are lowered to the following TAC:
  ///
  /// ```plaintext,ignore
  /// Label(start_label)
  /// <body instructions>
  /// Label(continue_label)
  /// <condition instructions>
  /// condition = <condition result>
  /// JumpIfNotZero(condition, start_label)
  /// Label(break_label)
  /// ```
  fn emit_do_while_loop(
    &mut self,
    do_while: &ast::DoWhile,
    instructions: &mut Vec<Instruction>,
  ) -> Result<()> {
    if let Some(loop_label) = do_while.loop_label {
      // Labels.
      let start_label = self.ctx.gen_label("do_while_start");
      let continue_label = continue_label(loop_label);
      let break_label = break_label(loop_label);

      // Instruction vectors.
      let mut condition_instructions = vec![];
      let mut body_instructions = vec![];

      // Emit condition instructions.
      let condition = self.emit_expression(&do_while.condition, &mut condition_instructions)?;

      // Emit body instructions.
      self.emit_statement(&do_while.body, &mut body_instructions)?;

      // Combine instruction vectors.
      instructions.push(Instruction::Label(start_label));
      instructions.extend(body_instructions);
      instructions.push(Instruction::Label(continue_label));
      instructions.extend(condition_instructions);
      instructions.extend([
        Instruction::JumpIfNotZero {
          condition,
          label: start_label,
        },
        Instruction::Label(break_label),
      ]);

      Ok(())
    } else {
      Err(LoweringError::new(
        "do-while loop has no associated label id",
        do_while.location,
      ))
    }
  }

  /// Emits instructions for `while` loops.
  ///
  /// `while` loops are lowered to the following TAC:
  ///
  /// ```plaintext,ignore
  /// Label(continue_label)
  /// <condition instructions>
  /// condition = <condition result>
  /// JumpIfZero(condition, break_label)
  /// <body instructions>
  /// Jump(continue_label)
  /// Label(break_label)
  /// ```
  fn emit_while_loop(
    &mut self,
    while_: &ast::While,
    instructions: &mut Vec<Instruction>,
  ) -> Result<()> {
    if let Some(loop_label) = while_.loop_label {
      // Labels.
      let continue_label = continue_label(loop_label);
      let break_label = break_label(loop_label);

      // Instruction vectors.
      let mut condition_instructions = vec![];
      let mut body_instructions = vec![];

      // Emit condition instructions.
      let condition = self.emit_expression(&while_.condition, &mut condition_instructions)?;

      // Emit body instructions.
      self.emit_statement(&while_.body, &mut body_instructions)?;

      // Combine instruction vectors.
      instructions.push(Instruction::Label(continue_label));
      instructions.extend(condition_instructions);
      instructions.push(Instruction::JumpIfZero {
        condition,
        label: break_label,
      });
      instructions.extend(body_instructions);
      instructions.extend([
        Instruction::Jump(continue_label),
        Instruction::Label(break_label),
      ]);

      Ok(())
    } else {
      Err(LoweringError::new(
        "while loop has no associated label id",
        while_.location,
      ))
    }
  }

  /// Emits instructions for `for` loops.
  ///
  /// `for` loops are lowered to the following TAC:
  ///
  /// ```plaintext,ignore
  /// <initializer instructions>
  /// Label(start_label)
  /// <condition instructions>
  /// condition = <condition result>
  /// JumpIfZero(condition, break_label)
  /// <body instructions>
  /// Label(continue_label)
  /// <postcondition instructions>
  /// Jump(start_label)
  /// Label(break_label)
  /// ```
  fn emit_for_loop(&mut self, for_: &ast::For, instructions: &mut Vec<Instruction>) -> Result<()> {
    if let Some(loop_label) = for_.loop_label {
      // Labels.
      let start_label = self.ctx.gen_label("for_start");
      let continue_label = continue_label(loop_label);
      let break_label = break_label(loop_label);

      // Instruction vectors.
      let mut initializer_instructions = vec![];
      let mut condition_instructions = vec![];
      let mut body_instructions = vec![];
      let mut postcondition_instructions = vec![];

      // Emit initializer instructions.
      if let ast::Initializer::Declaration(declaration) = &for_.initializer {
        self.emit_declaration(declaration, &mut initializer_instructions)?;
      } else if let ast::Initializer::Expression(expression) = &for_.initializer {
        self.emit_expression(expression, &mut initializer_instructions)?;
      }

      // Emit condition instructions.
      if let Some(condition) = &for_.condition {
        let condition = self.emit_expression(condition, &mut condition_instructions)?;

        condition_instructions.push(Instruction::JumpIfZero {
          condition,
          label: break_label,
        });
      }

      // Emit postcondition instructions.
      if let Some(postcondition) = &for_.postcondition {
        self.emit_expression(postcondition, &mut postcondition_instructions)?;
      }

      // Emit body instructions.
      self.emit_statement(&for_.body, &mut body_instructions)?;

      // Combine instruction vectors.
      instructions.extend(initializer_instructions);
      instructions.push(Instruction::Label(start_label));
      instructions.extend(condition_instructions);
      instructions.extend(body_instructions);
      instructions.push(Instruction::Label(continue_label));
      instructions.extend(postcondition_instructions);
      instructions.extend([
        Instruction::Jump(start_label),
        Instruction::Label(break_label),
      ]);

      Ok(())
    } else {
      Err(LoweringError::new(
        "for loop has no associated label id",
        for_.location,
      ))
    }
  }

  /// Emits instruction for `break`.
  fn emit_break(&mut self, break_: &ast::Break, instructions: &mut Vec<Instruction>) -> Result<()> {
    match break_.loop_label {
      | Some(label) => {
        let label = break_label(label);
        let instruction = Instruction::Jump(label);

        instructions.push(instruction);

        Ok(())
      },
      | None => {
        Err(LoweringError::new(
          "break statement has no associated label id",
          break_.location,
        ))
      },
    }
  }

  /// Emits instruction for `continue`.
  fn emit_continue(
    &mut self,
    continue_: &ast::Continue,
    instructions: &mut Vec<Instruction>,
  ) -> Result<()> {
    match continue_.loop_label {
      | Some(label) => {
        let label = continue_label(label);
        let instruction = Instruction::Jump(label);

        instructions.push(instruction);

        Ok(())
      },
      | None => {
        Err(LoweringError::new(
          "continue statement has no associated label id",
          continue_.location,
        ))
      },
    }
  }

  /// Emits instructions for expressions.
  fn emit_expression(
    &mut self,
    expression: &ast::Expression,
    instructions: &mut Vec<Instruction>,
  ) -> Result<Value> {
    // `Value`s are Copy, so we don't need to `clone` them, they'll be copied.
    match expression {
      | ast::Expression::Constant(int) => Ok(Value::Constant(int.value)),
      | ast::Expression::Var(ident) => Ok(Value::Var(ident.value)),
      | ast::Expression::Unary(unary) => self.emit_unary(unary, instructions),
      | ast::Expression::Binary(binary) if binary.is_and() => self.emit_and(binary, instructions),
      | ast::Expression::Binary(binary) if binary.is_or() => self.emit_or(binary, instructions),
      | ast::Expression::Binary(binary) => self.emit_binary(binary, instructions),
      | ast::Expression::Postfix(postfix) => self.emit_postfix(postfix, instructions),
      | ast::Expression::Ternary(ternary) => self.emit_ternary(ternary, instructions),
      | ast::Expression::Assignment(assignment) => self.emit_assignment(assignment, instructions),
      | ast::Expression::CompoundAssignment(assignment) => {
        self.emit_compound_assignment(assignment, instructions)
      },
    }
  }

  /// Emits instructions for unary operators.
  fn emit_unary(
    &mut self,
    unary: &ast::Unary,
    instructions: &mut Vec<Instruction>,
  ) -> Result<Value> {
    let op = self.lower_unary_op(&unary.op, unary.location)?;
    let src = self.emit_expression(&unary.expression, instructions)?;
    let dst = Value::Var(self.ctx.gen_temporary());

    instructions.push(Instruction::Unary { op, src, dst });

    Ok(dst)
  }

  /// Emits instructions for binary operators.
  fn emit_binary(
    &mut self,
    binary: &ast::Binary,
    instructions: &mut Vec<Instruction>,
  ) -> Result<Value> {
    let op = self.lower_binary_op(&binary.op, binary.location)?;
    let left = self.emit_expression(&binary.left, instructions)?;
    let right = self.emit_expression(&binary.right, instructions)?;
    let dst = Value::Var(self.ctx.gen_temporary());

    instructions.push(Instruction::Binary {
      op,
      left,
      right,
      dst,
    });

    Ok(dst)
  }

  /// Emits instructions for assignment.
  fn emit_assignment(
    &mut self,
    assignment: &ast::Assignment,
    instructions: &mut Vec<Instruction>,
  ) -> Result<Value> {
    match *assignment.left {
      | ast::Expression::Var(ident) => {
        let src = self.emit_expression(&assignment.right, instructions)?;
        let dst = Value::Var(ident.value);

        instructions.push(Instruction::Copy { src, dst });

        Ok(dst)
      },
      | _ => {
        Err(LoweringError::new(
          "invalid lvalue of assignment",
          *assignment.left.location(),
        ))
      },
    }
  }

  /// Emits instructions for compound assignments.
  fn emit_compound_assignment(
    &mut self,
    assignment: &ast::CompoundAssignment,
    instructions: &mut Vec<Instruction>,
  ) -> Result<Value> {
    match *assignment.left {
      | ast::Expression::Var(ident) => {
        let op = self.lower_binary_op(&assignment.op, assignment.location)?;
        let right = self.emit_expression(&assignment.right, instructions)?;
        let dst = Value::Var(ident.value);

        instructions.push(Instruction::Binary {
          op,
          left: dst,
          right,
          dst,
        });

        Ok(dst)
      },
      | _ => {
        Err(LoweringError::new(
          "invalid lvalue of compound assignment",
          *assignment.left.location(),
        ))
      },
    }
  }

  /// Emits instructions for if statements.
  ///
  /// For if-else statements, the following TAC sequence is generated:
  ///
  /// ```plaintext,ignore
  /// <condition instructions>
  /// condition = <condition result>
  /// JumpIfZero(condition, else_label)
  /// <then instructions>
  /// Jump(end_label)
  /// Label(else_label)
  /// <else instructions>
  /// Label(end_label)
  /// ```
  ///
  /// For if statements without else, generates:
  ///
  /// ```plaintext,ignore
  /// <condition instructions>
  /// condition = <condition result>
  /// JumpIfZero(condition, end_label)
  /// <then instructions>
  /// Label(end_label)
  /// ```
  fn emit_if(&mut self, conditional: &ast::If, instructions: &mut Vec<Instruction>) -> Result<()> {
    match &conditional.otherwise {
      | Some(otherwise) => {
        // Separate instruction vectors for each block to maintain evaluation order.
        let mut condition_instructions = vec![];
        let mut then_instructions = vec![];
        let mut otherwise_instructions = vec![];

        // Generate unique labels for branching.
        let else_label = self.ctx.gen_label("else");
        let end_label = self.ctx.gen_label("");

        // Generate code for condition and both branches.
        let condition =
          self.emit_expression(&conditional.condition, &mut condition_instructions)?;

        self.emit_statement(&conditional.then, &mut then_instructions)?;
        self.emit_statement(otherwise, &mut otherwise_instructions)?;

        // Combine instructions in proper order with control flow.
        instructions.extend(condition_instructions);
        instructions.push(Instruction::JumpIfZero {
          condition,
          label: else_label,
        });
        instructions.extend(then_instructions);
        instructions.extend([Instruction::Jump(end_label), Instruction::Label(else_label)]);
        instructions.extend(otherwise_instructions);
        instructions.push(Instruction::Label(end_label));

        Ok(())
      },
      | None => {
        // Similar to if-else but without else block.
        let mut condition_instructions = vec![];
        let mut then_instructions = vec![];

        // Generate label for branching.
        let end_label = self.ctx.gen_label("if_end");

        // Generate code for condition and then branch.
        let condition =
          self.emit_expression(&conditional.condition, &mut condition_instructions)?;

        self.emit_statement(&conditional.then, &mut then_instructions)?;

        // Combine instructions with control flow.
        instructions.extend(condition_instructions);
        instructions.push(Instruction::JumpIfZero {
          condition,
          label: end_label,
        });
        instructions.extend(then_instructions);
        instructions.push(Instruction::Label(end_label));

        Ok(())
      },
    }
  }

  /// Emits instructions for goto statements.
  fn emit_goto(&mut self, goto: &ast::Goto, instructions: &mut Vec<Instruction>) -> Result<()> {
    instructions.push(Instruction::Jump(goto.label.value));

    Ok(())
  }

  /// Emits instructions for labeled statements.
  fn emit_labeled(
    &mut self,
    labeled: &ast::Labeled,
    instructions: &mut Vec<Instruction>,
  ) -> Result<()> {
    instructions.push(Instruction::Label(labeled.label.value));

    self.emit_statement(&labeled.statement, instructions)
  }

  /// Emits instructions for postfix expressions.
  fn emit_postfix(
    &mut self,
    postfix: &ast::Postfix,
    instructions: &mut Vec<Instruction>,
  ) -> Result<Value> {
    match *postfix.operand {
      | ast::Expression::Var(ident) => {
        // Converting postfix operator to a binary operator first.
        let binary_op = self.convert_postfix_op(&postfix.op);
        let op = self.lower_binary_op(&binary_op, postfix.location)?;
        let src = Value::Var(ident.value);
        let dst = Value::Var(self.ctx.gen_temporary());

        // 1. Copy x's value to temp (to return original value).
        // 2. Add/subtract 1 to/from x and store back in x.
        instructions.extend([
          Instruction::Copy { src, dst },
          Instruction::Binary {
            op,
            left: src,
            right: Value::Constant(1),
            dst: src,
          },
        ]);

        Ok(dst)
      },
      | _ => {
        Err(LoweringError::new(
          "invalid operand of postfix expression",
          *postfix.operand.location(),
        ))
      },
    }
  }

  /// Emits instructions for the ternary operator (`?:`).
  ///
  /// Generates TAC in the following form:
  ///
  /// ```plaintext,ignore
  /// <condition instructions>
  /// condition = <condition result>
  /// JumpIfZero(condition, else_label)
  /// <then instructions>
  /// then_result = <then result>
  /// result = then_result
  /// Jump(end_label)
  /// Label(else_label)
  /// <else instructions>
  /// else_result = <else result>
  /// result = else_result
  /// Label(end_label)
  /// ```
  fn emit_ternary(
    &mut self,
    ternary: &ast::Ternary,
    instructions: &mut Vec<Instruction>,
  ) -> Result<Value> {
    // Create separate instruction vectors to maintain evaluation order.
    let mut condition_instructions = vec![];
    let mut then_instructions = vec![];
    let mut otherwise_instructions = vec![];

    // Generate code for all three expressions.
    let condition = self.emit_expression(&ternary.condition, &mut condition_instructions)?;
    let then = self.emit_expression(&ternary.then, &mut then_instructions)?;
    let otherwise = self.emit_expression(&ternary.otherwise, &mut otherwise_instructions)?;

    // Generate labels for control flow.
    let else_label = self.ctx.gen_label("ternary_else");
    let end_label = self.ctx.gen_label("ternary_end");
    let dst = Value::Var(self.ctx.gen_temporary());

    // Emit condition and branch to else block if false.
    instructions.extend(condition_instructions);
    instructions.push(Instruction::JumpIfZero {
      condition,
      label: else_label,
    });

    // Emit then block and jump to end.
    instructions.extend(then_instructions);
    instructions.extend([
      Instruction::Copy { src: then, dst },
      Instruction::Jump(end_label),
      Instruction::Label(else_label),
    ]);

    // Emit else block and jump to end.
    instructions.extend(otherwise_instructions);
    instructions.extend([
      Instruction::Copy {
        src: otherwise,
        dst,
      },
      Instruction::Label(end_label),
    ]);

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
  ) -> Result<Value> {
    // We don't push instructions emitted for subexpressions straight to the main instructions
    // vector, because we need to interleave them with the jump instructions.
    let mut left_instructions = vec![];
    let mut right_instructions = vec![];

    let left = self.emit_expression(&binary.left, &mut left_instructions)?;
    let right = self.emit_expression(&binary.right, &mut right_instructions)?;

    // Generate jump labels.
    let false_label = self.ctx.gen_label("and_false");
    let end_label = self.ctx.gen_label("and_end");

    let dst = Value::Var(self.ctx.gen_temporary());

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
  ) -> Result<Value> {
    // We don't push instructions emitted for subexpressions straight to the main instructions
    // vector, because we need to interleave them with the jump instructions.
    let mut left_instructions = vec![];
    let mut right_instructions = vec![];

    let left = self.emit_expression(&binary.left, &mut left_instructions)?;
    let right = self.emit_expression(&binary.right, &mut right_instructions)?;

    // Generate jump labels.
    let true_label = self.ctx.gen_label("or_true");
    let end_label = self.ctx.gen_label("or_end");

    let dst = Value::Var(self.ctx.gen_temporary());

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

  /// Converts a postfix operator (AST) to an binary operator (also AST).
  fn convert_postfix_op(&self, op: &ast::PostfixOp) -> ast::BinaryOp {
    match op {
      | ast::PostfixOp::Dec => ast::BinaryOp::Add,
      | ast::PostfixOp::Inc => ast::BinaryOp::Sub,
    }
  }

  /// Lowers an unary operator to its TAC equivalent.
  fn lower_unary_op(&self, op: &ast::UnaryOp, location: Location) -> Result<UnaryOp> {
    match op {
      // Arithmetics operators.
      | ast::UnaryOp::Negate => Ok(UnaryOp::Negate),
      // Bitwise operators.
      | ast::UnaryOp::BitNot => Ok(UnaryOp::BitNot),
      // Logical operators.
      | ast::UnaryOp::Not => Ok(UnaryOp::Not),
      // Other operators.
      | ast::UnaryOp::Dec | ast::UnaryOp::Inc => {
        Err(LoweringError::new(
          "++ and -- operators cannot be directly lowered to TAC",
          location,
        ))
      },
    }
  }

  /// Lowers a binary operator to its TAC equivalent.
  fn lower_binary_op(&self, op: &ast::BinaryOp, location: Location) -> Result<BinaryOp> {
    match op {
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
          location,
        ))
      },
    }
  }
}

/// Creates a label for a `continue` statement.
fn continue_label(label: ast::LoopLabel) -> Intern<String> {
  format!("continue.{label}").into()
}

/// Creates a label for a `break` statement.
fn break_label(label: ast::LoopLabel) -> Intern<String> {
  format!("break.{label}").into()
}
