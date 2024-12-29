use std::collections::HashMap;

use internment::Intern;
use thiserror::Error;

use crate::ir::aast::*;
use crate::ir::tac;

#[derive(Debug, Error)]
#[error("TAC lowering error: {0}")]
pub struct LoweringError(String);

impl LoweringError {
  pub fn new(message: impl AsRef<str> + Into<String>) -> Self {
    Self(message.into())
  }
}

/// Pass to transform (lower) TAC to AAST.
///
/// This pass transforms TAC to AAST by lowering each instruction to its AAST equivalent (if any).
/// For example, the TAC instruction `Return(Constant(5))` is lowered to the AAST instruction
/// `Mov(Imm(5), Reg(AX))` and `Ret`.
pub struct LoweringPass;

impl LoweringPass {
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

          instructions.extend(vec![Instruction::Mov { src, dst }, Instruction::Ret]);
        },
        | tac::Instruction::Unary { op, src, dst } => {
          let op = self.lower_unary_op(op);
          let src = self.lower_value(src);
          let dst = self.lower_value(dst);

          instructions.extend(vec![
            Instruction::Mov { src, dst },
            Instruction::Unary { op, operand: dst },
          ]);
        },
        | tac::Instruction::Binary {
          op,
          left,
          right,
          dst,
        } => {
          let left = self.lower_value(left);
          let right = self.lower_value(right);
          let dst = self.lower_value(dst);

          match op {
            // Division/Modulo.
            | tac::BinaryOp::Div | tac::BinaryOp::Mod => {
              // We need to use DX for the remainder.
              let reg = if op == &tac::BinaryOp::Div {
                Operand::Reg(Reg::AX)
              } else {
                Operand::Reg(Reg::DX)
              };

              instructions.extend(vec![
                Instruction::Mov {
                  src: left,
                  dst: Operand::Reg(Reg::AX),
                },
                Instruction::Cdq,
                Instruction::Idiv(right),
                Instruction::Mov { src: reg, dst },
              ]);
            },
            // Bitwise shifts.
            | tac::BinaryOp::BitShl | tac::BinaryOp::BitShr => {
              let op = self.lower_binary_op(op)?;

              if let Operand::Imm(..) = right {
                instructions.extend(vec![
                  Instruction::Mov { src: left, dst },
                  Instruction::Binary {
                    op,
                    src: right,
                    dst,
                  },
                ]);
              } else {
                instructions.extend(vec![
                  Instruction::Mov { src: left, dst },
                  Instruction::Mov {
                    src: right,
                    dst: Operand::Reg(Reg::CX),
                  },
                  Instruction::Binary {
                    op,
                    src: Operand::Reg(Reg::CX),
                    dst,
                  },
                ]);
              }
            },
            // Addition/Subtraction/Multiplication/And/Or/Xor.
            | op => {
              let op = self.lower_binary_op(op)?;

              instructions.extend(vec![
                Instruction::Mov { src: left, dst },
                Instruction::Binary {
                  op,
                  src: right,
                  dst,
                },
              ]);
            },
          }
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

  fn lower_unary_op(&self, op: &tac::UnaryOp) -> UnaryOp {
    match op {
      | tac::UnaryOp::BitNot => UnaryOp::Not,
      | tac::UnaryOp::Negate => UnaryOp::Neg,
    }
  }

  fn lower_binary_op(&self, op: &tac::BinaryOp) -> Result<BinaryOp, LoweringError> {
    match op {
      | tac::BinaryOp::Add => Ok(BinaryOp::Add),
      | tac::BinaryOp::BitAnd => Ok(BinaryOp::And),
      | tac::BinaryOp::BitOr => Ok(BinaryOp::Or),
      | tac::BinaryOp::BitShl => Ok(BinaryOp::Sal),
      | tac::BinaryOp::BitShr => Ok(BinaryOp::Sar),
      | tac::BinaryOp::BitXor => Ok(BinaryOp::Xor),
      | tac::BinaryOp::Mul => Ok(BinaryOp::Mul),
      | tac::BinaryOp::Sub => Ok(BinaryOp::Sub),
      | tac::BinaryOp::Div | tac::BinaryOp::Mod => {
        Err(LoweringError::new(
          "division cannot be handled like other binary operators",
        ))
      },
    }
  }
}

#[derive(Debug, Error)]
#[error("pseudo replacement pass error: {0}")]
pub struct PseudoReplacementError(String);

impl PseudoReplacementError {
  pub fn new(message: impl AsRef<str> + Into<String>) -> Self {
    Self(message.into())
  }
}

/// Pass to replace pseudoregisters.
///
/// This pass replaces each `Pseudo` operand with a `Stack` operand, leaving the rest of the AAST
/// unchanged. We replace the first temporary variable we see with `Stack(-4)`, the next with
/// `Stack(-8)`, and so on. We subtract 4 for each new variable, since every temporary variable is a
/// 4-byte integer.
///
/// We also maintain a map from identifiers to offsets as we go so we can replace each
/// pseudoregister with the same address on the stack every time it appears. For example, if we
/// process the instructions:
///
/// ```text
/// Mov(Imm(2), Pseudo("a"))
/// Unary(Neg, Pseudo("a"))
/// ```
///
/// We replace `Pseudo("a")` with the same `Stack` operand in both instructions.
///
/// This compiler pass also returns the stack offset of the final temporary variable, because
/// that tells us how many bytes to allocate on the stack in the next pass (instructions fixup).
pub struct PseudoReplacementPass {
  offset: isize,
  offset_map: HashMap<Intern<String>, isize>,
}

impl PseudoReplacementPass {
  pub fn new() -> Self {
    Self {
      offset: 0,
      offset_map: HashMap::new(),
    }
  }

  pub fn run(&mut self, program: &Program) -> Result<(Program, isize), PseudoReplacementError> {
    let program = self.replace_pseudos_in_program(program)?;
    let offset = self.offset;

    Ok((program, offset.abs()))
  }

  fn replace_pseudos_in_program(
    &mut self,
    program: &Program,
  ) -> Result<Program, PseudoReplacementError> {
    let function = self.replace_pseudos_in_function(&program.function)?;

    Ok(Program { function })
  }

  fn replace_pseudos_in_function(
    &mut self,
    function: &Function,
  ) -> Result<Function, PseudoReplacementError> {
    let mut instructions = Vec::new();

    for instruction in &function.instructions {
      instructions.push(self.replace_pseudos_in_instruction(instruction)?);
    }

    Ok(Function {
      name: function.name,
      instructions,
    })
  }

  fn replace_pseudos_in_instruction(
    &mut self,
    instruction: &Instruction,
  ) -> Result<Instruction, PseudoReplacementError> {
    match instruction {
      | Instruction::Mov { src, dst } => {
        let src = self.replace_operand(src);
        let dst = self.replace_operand(dst);

        Ok(Instruction::Mov { src, dst })
      },
      | Instruction::Unary { op, operand } => {
        let op = *op;
        let operand = self.replace_operand(operand);

        Ok(Instruction::Unary { op, operand })
      },
      | Instruction::Binary { op, src, dst } => {
        let op = *op;
        let src = self.replace_operand(src);
        let dst = self.replace_operand(dst);

        Ok(Instruction::Binary { op, src, dst })
      },
      | Instruction::Idiv(operand) => {
        let operand = self.replace_operand(operand);

        Ok(Instruction::Idiv(operand))
      },
      | Instruction::Cdq => Ok(Instruction::Cdq),
      | Instruction::Ret => Ok(Instruction::Ret),
      | Instruction::AllocateStack(..) => {
        Err(PseudoReplacementError::new(
          "unexpected AllocateStack instruction",
        ))
      },
    }
  }

  fn replace_operand(&mut self, operand: &Operand) -> Operand {
    match operand {
      | Operand::Pseudo(identifier) => {
        // We've already assigned this operand a stack slot, so we don't need to do anything.
        if let Some(offset) = self.offset_map.get(identifier) {
          Operand::Stack(*offset)
        }
        // Otherwise we need to assign a stack slot to this operand.
        else {
          self.offset -= 4;
          self.offset_map.insert(*identifier, self.offset);

          Operand::Stack(self.offset)
        }
      },
      | other => *other,
    }
  }
}

#[derive(Debug, Error)]
#[error("instruction fixup pass error: {0}")]
pub struct InstructionFixupError(String);

impl InstructionFixupError {
  pub fn new(message: impl AsRef<str> + Into<String>) -> Self {
    Self(message.into())
  }
}

/// Pass to fix up instructions.
///
/// This pass does the following:
///
/// - Inserts an `AllocateStack` instruction at the beginning of the instruction list with the stack
///   offset of the last temporary variable. This is needed to allocate enough space on the stack to
///   accomodate every address we use.
///
/// - Rewrites invalid `Mov` instructions. For example, the instruction `Mov(Stack(0), Stack(4))`
///   should be rewritten to `Mov(Stack(0), Reg(R10))` and then `Mov(Reg(R10), Stack(4))`, since
///   `mov`, like many other instructions, can't have memory addresses as both the source and the
///   destination. So we make use of scratch register, specifically R10D, to do the rewriting.
pub struct InstructionFixupPass {
  stack_size: isize,
}

impl InstructionFixupPass {
  pub fn new(stack_size: isize) -> Self {
    Self { stack_size }
  }

  pub fn run(&self, program: &Program) -> Result<Program, InstructionFixupError> {
    self.fixup_program(program)
  }

  fn fixup_program(&self, program: &Program) -> Result<Program, InstructionFixupError> {
    let function = self.fixup_function(&program.function)?;

    Ok(Program { function })
  }

  fn fixup_function(&self, function: &Function) -> Result<Function, InstructionFixupError> {
    let mut instructions = vec![Instruction::AllocateStack(self.stack_size)];

    for instruction in &function.instructions {
      instructions.extend(self.fixup_instruction(instruction)?);
    }

    Ok(Function {
      name: function.name,
      instructions,
    })
  }

  fn fixup_instruction(
    &self,
    instruction: &Instruction,
  ) -> Result<Vec<Instruction>, InstructionFixupError> {
    match *instruction {
      // Mov can't move a value from one memory address to another.
      | Instruction::Mov {
        src: src @ Operand::Stack(..),
        dst: dst @ Operand::Stack(..),
      } => {
        let reg = Operand::Reg(Reg::R10);

        Ok(vec![
          Instruction::Mov { src, dst: reg },
          Instruction::Mov { src: reg, dst },
        ])
      },
      // Add/Sub/And/Or/Xor can't use memory addresses for both operands.
      | Instruction::Binary {
        op: op @ (BinaryOp::Add | BinaryOp::Sub | BinaryOp::And | BinaryOp::Or | BinaryOp::Xor),
        src: src @ Operand::Stack(..),
        dst: dst @ Operand::Stack(..),
      } => {
        let reg = Operand::Reg(Reg::R10);

        Ok(vec![
          Instruction::Mov { src, dst: reg },
          Instruction::Binary { op, src: reg, dst },
        ])
      },
      // Destination of Mult can't be in memory.
      | Instruction::Binary {
        op: op @ BinaryOp::Mul,
        src,
        dst: dst @ Operand::Stack(..),
      } => {
        let reg = Operand::Reg(Reg::R11);

        Ok(vec![
          Instruction::Mov { src: dst, dst: reg },
          Instruction::Binary { op, src, dst: reg },
          Instruction::Mov { src: reg, dst },
        ])
      },
      // Idiv can't operate on constants.
      | Instruction::Idiv(src @ Operand::Imm(..)) => {
        let reg = Operand::Reg(Reg::R10);

        Ok(vec![
          Instruction::Mov { src, dst: reg },
          Instruction::Idiv(reg),
        ])
      },
      | other => Ok(vec![other]),
    }
  }
}
