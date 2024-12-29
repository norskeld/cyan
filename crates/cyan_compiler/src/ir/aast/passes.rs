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

          instructions.push(Instruction::Mov { src, dst });
          instructions.push(Instruction::Ret);
        },
        | tac::Instruction::Unary { operator, src, dst } => {
          let operator = self.lower_unary_operator(operator);
          let src = self.lower_value(src);
          let dst = self.lower_value(dst);

          instructions.push(Instruction::Mov { src, dst });
          instructions.push(Instruction::Unary {
            operator,
            operand: dst,
          });
        },
        | tac::Instruction::Binary {
          operator: tac::BinaryOp::Divide,
          left,
          right,
          dst,
        } => {
          let left = self.lower_value(left);
          let right = self.lower_value(right);
          let dst = self.lower_value(dst);

          instructions.extend(vec![
            Instruction::Mov {
              src: left,
              dst: Operand::Reg(Reg::AX),
            },
            Instruction::Cdq,
            Instruction::Idiv(right),
            Instruction::Mov {
              src: Operand::Reg(Reg::AX),
              dst,
            },
          ]);
        },
        | tac::Instruction::Binary {
          operator: tac::BinaryOp::Mod,
          left,
          right,
          dst,
        } => {
          let left = self.lower_value(left);
          let right = self.lower_value(right);
          let dst = self.lower_value(dst);

          instructions.extend(vec![
            Instruction::Mov {
              src: left,
              dst: Operand::Reg(Reg::AX),
            },
            Instruction::Cdq,
            Instruction::Idiv(right),
            Instruction::Mov {
              src: Operand::Reg(Reg::DX),
              dst,
            },
          ]);
        },
        | tac::Instruction::Binary {
          operator,
          left,
          right,
          dst,
        } => {
          let operator = self.lower_binary_operator(operator)?;
          let left = self.lower_value(left);
          let right = self.lower_value(right);
          let dst = self.lower_value(dst);

          instructions.extend(vec![
            Instruction::Mov { src: left, dst },
            Instruction::Binary {
              operator,
              src: right,
              dst,
            },
          ]);
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

  fn lower_unary_operator(&self, operator: &tac::UnaryOp) -> UnaryOp {
    match operator {
      | tac::UnaryOp::BitwiseNot => UnaryOp::Not,
      | tac::UnaryOp::Negate => UnaryOp::Neg,
    }
  }

  fn lower_binary_operator(&self, operator: &tac::BinaryOp) -> Result<BinaryOp, LoweringError> {
    match operator {
      | tac::BinaryOp::Add => Ok(BinaryOp::Add),
      | tac::BinaryOp::Subtract => Ok(BinaryOp::Sub),
      | tac::BinaryOp::Multiply => Ok(BinaryOp::Mult),
      | tac::BinaryOp::Divide | tac::BinaryOp::Mod => {
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
      | Instruction::Unary { operator, operand } => {
        let operator = *operator;
        let operand = self.replace_operand(operand);

        Ok(Instruction::Unary { operator, operand })
      },
      | Instruction::Binary { operator, src, dst } => {
        let operator = *operator;
        let src = self.replace_operand(src);
        let dst = self.replace_operand(dst);

        Ok(Instruction::Binary { operator, src, dst })
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
  last_stack_slot: isize,
}

impl InstructionFixupPass {
  pub fn new(last_stack_slot: isize) -> Self {
    Self { last_stack_slot }
  }

  pub fn run(&self, program: &Program) -> Result<Program, InstructionFixupError> {
    self.fixup_program(program)
  }

  fn fixup_program(&self, program: &Program) -> Result<Program, InstructionFixupError> {
    let function = self.fixup_function(&program.function)?;

    Ok(Program { function })
  }

  fn fixup_function(&self, function: &Function) -> Result<Function, InstructionFixupError> {
    let mut instructions = vec![Instruction::AllocateStack(self.last_stack_slot)];

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
        let fixed = vec![
          Instruction::Mov {
            src,
            dst: Operand::Reg(Reg::R10),
          },
          Instruction::Mov {
            src: Operand::Reg(Reg::R10),
            dst,
          },
        ];

        Ok(fixed)
      },
      // Add/Sub can't use memory addresses for both operands.
      | Instruction::Binary {
        operator: operator @ (BinaryOp::Add | BinaryOp::Sub),
        src: src @ Operand::Stack(..),
        dst: dst @ Operand::Stack(..),
      } => {
        Ok(vec![
          Instruction::Mov {
            src,
            dst: Operand::Reg(Reg::R10),
          },
          Instruction::Binary {
            operator,
            src: Operand::Reg(Reg::R10),
            dst,
          },
        ])
      },
      // Destination of Mult can't be in memory.
      | Instruction::Binary {
        operator: BinaryOp::Mult,
        src,
        dst: dst @ Operand::Stack(..),
      } => {
        Ok(vec![
          Instruction::Mov {
            src: dst,
            dst: Operand::Reg(Reg::R11),
          },
          Instruction::Binary {
            operator: BinaryOp::Mult,
            src,
            dst: Operand::Reg(Reg::R11),
          },
          Instruction::Mov {
            src: Operand::Reg(Reg::R11),
            dst,
          },
        ])
      },
      // Idiv can't operate on constants.
      | Instruction::Idiv(operand @ Operand::Imm(..)) => {
        Ok(vec![
          Instruction::Mov {
            src: operand,
            dst: Operand::Reg(Reg::R10),
          },
          Instruction::Idiv(Operand::Reg(Reg::R10)),
        ])
      },
      | other => Ok(vec![other]),
    }
  }
}
