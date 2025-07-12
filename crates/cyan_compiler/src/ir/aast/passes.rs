use std::collections::HashMap;

use thiserror::Error;

use crate::ir::aast::*;
use crate::ir::tac;

/// Argument registers.
const ARG_REGISTERS: [Reg; 6] = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];

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
    // let function = self.lower_function(&program.function)?;
    let mut definitions = Vec::new();

    for function in &program.definitions {
      let definition = self.lower_func(function)?;
      definitions.push(definition);
    }

    Ok(Program { definitions })
  }

  /// Lowers function parameters to the given function.
  ///
  /// According to the System V calling convention:
  ///
  /// - the first 6 integer arguments are passed in registers:
  ///   - 32-bit integers: EDI, ESI, EDX, ECX, R8D, and R9D;
  ///   - 64-bit integers: RDI, RSI, RDX, RCX, R8, and R9.
  ///
  /// - the rest of the arguments are passed on the stack in reverse order.
  fn lower_func_params(&self, params: &[Symbol]) -> Result<Vec<Instruction>, LoweringError> {
    let mut instructions = Vec::new();

    // Get the first 6 parameters for registers and the rest for the stack.
    let (register_params, stack_params) = params.split_at(params.len().min(6));

    // Pass params in registers.
    for (idx, param) in register_params.iter().enumerate() {
      let src = Operand::Reg(ARG_REGISTERS[idx]);
      let dst = Operand::Pseudo(*param);

      instructions.push(Instruction::Mov { src, dst });
    }

    // Pass params on the stack.
    for (idx, param) in stack_params.iter().enumerate() {
      let src = Operand::Stack(16 + (8 * idx) as isize);
      let dst = Operand::Pseudo(*param);

      instructions.push(Instruction::Mov { src, dst });
    }

    Ok(instructions)
  }

  fn lower_func_call(
    &self,
    name: &Symbol,
    args: &[tac::Value],
    dst: &tac::Value,
  ) -> Result<Vec<Instruction>, LoweringError> {
    let mut instructions = vec![];

    // Get the first 6 parameters for registers and the rest for the stack.
    let (register_args, stack_args) = args.split_at(args.len().min(6));

    // Adjust stack alignment.
    let stack_padding = if stack_args.len() % 2 == 0 { 0 } else { 8 };

    if stack_padding > 0 {
      instructions.push(Instruction::AllocateStack(stack_padding));
    }

    // Pass arguments in registers.
    for (idx, arg) in register_args.iter().enumerate() {
      let src = self.lower_value(arg);
      let dst = Operand::Reg(ARG_REGISTERS[idx]);

      instructions.push(Instruction::Mov { src, dst });
    }

    // Pass arguments on the stack in reverse order.
    for arg in stack_args.iter().rev() {
      match self.lower_value(arg) {
        | operand @ (Operand::Imm(..) | Operand::Reg(..)) => {
          instructions.push(Instruction::Push(operand));
        },
        | operand => {
          instructions.extend([
            Instruction::Mov {
              src: operand,
              dst: Operand::Reg(Reg::AX),
            },
            Instruction::Push(Operand::Reg(Reg::AX)),
          ]);
        },
      };
    }

    // Adjust stack pointer.
    instructions.push(Instruction::Call(*name));

    // Deallocate stack if needed.
    let dealloc_bytes = (8 * stack_args.len()) + stack_padding;

    if dealloc_bytes > 0 {
      instructions.push(Instruction::DeallocateStack(dealloc_bytes));
    }

    // Get return value.
    let dst = self.lower_value(dst);
    let src = Operand::Reg(Reg::AX);

    instructions.push(Instruction::Mov { src, dst });

    Ok(instructions)
  }

  fn lower_func(&self, function: &tac::Function) -> Result<Function, LoweringError> {
    let mut instructions = Vec::new();

    instructions.extend(self.lower_func_params(&function.params)?);
    instructions.extend(self.lower_instructions(&function.instructions)?);

    Ok(Function {
      name: function.name,
      instructions,
    })
  }

  fn lower_instructions(
    &self,
    tac_instructions: &[tac::Instruction],
  ) -> Result<Vec<Instruction>, LoweringError> {
    let mut instructions = Vec::new();

    for instruction in tac_instructions {
      match instruction {
        | tac::Instruction::Return(value) => {
          let src = self.lower_value(value);
          let dst = Operand::Reg(Reg::AX);

          instructions.extend([Instruction::Mov { src, dst }, Instruction::Ret]);
        },
        | tac::Instruction::Unary {
          op: tac::UnaryOp::Not,
          src,
          dst,
        } => {
          let src = self.lower_value(src);
          let dst = self.lower_value(dst);

          instructions.extend([
            Instruction::Cmp {
              left: Operand::Imm(0),
              right: src,
            },
            Instruction::Mov {
              src: Operand::Imm(0),
              dst,
            },
            Instruction::SetCC {
              code: CondCode::E,
              dst,
            },
          ]);
        },
        | tac::Instruction::Unary { op, src, dst } => {
          let op = self.lower_unary_op(op)?;
          let src = self.lower_value(src);
          let dst = self.lower_value(dst);

          instructions.extend([
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
            // Relational.
            | tac::BinaryOp::Equal
            | tac::BinaryOp::NotEqual
            | tac::BinaryOp::Greater
            | tac::BinaryOp::GreaterEqual
            | tac::BinaryOp::Less
            | tac::BinaryOp::LessEqual => {
              let code = self.lower_cond_code(op)?;

              instructions.extend([
                Instruction::Cmp { left, right },
                Instruction::Mov {
                  src: Operand::Imm(0),
                  dst,
                },
                Instruction::SetCC { code, dst },
              ]);
            },
            // Division/Modulo.
            | tac::BinaryOp::Div | tac::BinaryOp::Mod => {
              // We need to use DX for the remainder.
              let reg = if op == &tac::BinaryOp::Div {
                Operand::Reg(Reg::AX)
              } else {
                Operand::Reg(Reg::DX)
              };

              instructions.extend([
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
                instructions.extend([
                  Instruction::Mov { src: left, dst },
                  Instruction::Binary {
                    op,
                    src: right,
                    dst,
                  },
                ]);
              } else {
                instructions.extend([
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
            // Addition/Subtraction/Multiplication/BitAnd/BitOr/BitXor.
            | op => {
              let op = self.lower_binary_op(op)?;

              instructions.extend([
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
        | tac::Instruction::Copy { src, dst } => {
          let src = self.lower_value(src);
          let dst = self.lower_value(dst);

          instructions.push(Instruction::Mov { src, dst });
        },
        | tac::Instruction::Jump(label) => {
          instructions.push(Instruction::Jmp(*label));
        },
        | tac::Instruction::JumpIfEqual { left, right, label } => {
          let left = self.lower_value(left);
          let right = self.lower_value(right);

          instructions.extend([
            Instruction::Cmp { left, right },
            Instruction::JmpCC {
              code: CondCode::E,
              label: *label,
            },
          ])
        },
        | tac::Instruction::JumpIfZero { condition, label } => {
          let right = self.lower_value(condition);

          instructions.extend([
            Instruction::Cmp {
              left: Operand::Imm(0),
              right,
            },
            Instruction::JmpCC {
              code: CondCode::E,
              label: *label,
            },
          ])
        },
        | tac::Instruction::JumpIfNotZero { condition, label } => {
          let right = self.lower_value(condition);

          instructions.extend([
            Instruction::Cmp {
              left: Operand::Imm(0),
              right,
            },
            Instruction::JmpCC {
              code: CondCode::NE,
              label: *label,
            },
          ])
        },
        | tac::Instruction::Label(label) => {
          instructions.push(Instruction::Label(*label));
        },
        | tac::Instruction::FuncCall { name, args, dst } => {
          let call_instructions = self.lower_func_call(name, args, dst)?;
          instructions.extend(call_instructions);
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

  fn lower_unary_op(&self, op: &tac::UnaryOp) -> Result<UnaryOp, LoweringError> {
    match op {
      | tac::UnaryOp::BitNot => Ok(UnaryOp::Not),
      | tac::UnaryOp::Negate => Ok(UnaryOp::Neg),
      | tac::UnaryOp::Not => {
        Err(LoweringError::new(
          "can't lower TAC logical not directly to AAST",
        ))
      },
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
      | tac::BinaryOp::Div
      | tac::BinaryOp::Mod
      | tac::BinaryOp::Equal
      | tac::BinaryOp::NotEqual
      | tac::BinaryOp::Greater
      | tac::BinaryOp::GreaterEqual
      | tac::BinaryOp::Less
      | tac::BinaryOp::LessEqual => Err(LoweringError::new("not a binary assembly instruction")),
    }
  }

  fn lower_cond_code(&self, op: &tac::BinaryOp) -> Result<CondCode, LoweringError> {
    match op {
      | tac::BinaryOp::Equal => Ok(CondCode::E),
      | tac::BinaryOp::NotEqual => Ok(CondCode::NE),
      | tac::BinaryOp::Greater => Ok(CondCode::G),
      | tac::BinaryOp::GreaterEqual => Ok(CondCode::GE),
      | tac::BinaryOp::Less => Ok(CondCode::L),
      | tac::BinaryOp::LessEqual => Ok(CondCode::LE),
      | _ => Err(LoweringError::new("not a condition code")),
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
  offset_map: HashMap<Symbol, isize>,
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
    let mut definitions = vec![];

    for function in &program.definitions {
      let function = self.replace_pseudos_in_function(function)?;
      definitions.push(function);
    }

    Ok(Program { definitions })
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
      | Instruction::Cmp { left, right } => {
        let left = self.replace_operand(left);
        let right = self.replace_operand(right);

        Ok(Instruction::Cmp { left, right })
      },
      | Instruction::SetCC { code, dst } => {
        let code = *code;
        let dst = self.replace_operand(dst);

        Ok(Instruction::SetCC { code, dst })
      },
      | Instruction::Push(operand) => {
        let operand = self.replace_operand(operand);

        Ok(Instruction::Push(operand))
      },
      | instruction @ (Instruction::Ret
      | Instruction::Cdq
      | Instruction::Label(..)
      | Instruction::Jmp(..)
      | Instruction::JmpCC { .. }
      | Instruction::Call { .. }
      | Instruction::AllocateStack(..)
      | Instruction::DeallocateStack(..)) => Ok(*instruction),
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
  stack_size: usize,
}

impl InstructionFixupPass {
  pub fn new(stack_size: usize) -> Self {
    Self { stack_size }
  }

  pub fn run(&self, program: &Program) -> Result<Program, InstructionFixupError> {
    self.fixup_program(program)
  }

  fn fixup_program(&self, program: &Program) -> Result<Program, InstructionFixupError> {
    let mut definitions = vec![];

    for function in &program.definitions {
      let function = self.fixup_function(function)?;
      definitions.push(function);
    }

    Ok(Program { definitions })
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
      // Add/Sub/BitAnd/BitOr/BitXor can't use memory addresses for both operands.
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
      // Cmp's both operands can't be in memory.
      | Instruction::Cmp {
        left: left @ Operand::Stack(..),
        right: right @ Operand::Stack(..),
      } => {
        let reg = Operand::Reg(Reg::R10);

        Ok(vec![
          Instruction::Mov {
            src: left,
            dst: reg,
          },
          Instruction::Cmp { left: reg, right },
        ])
      },
      // Cmp's second operand can't be a constant.
      | Instruction::Cmp {
        left,
        right: right @ Operand::Imm(..),
      } => {
        let reg = Operand::Reg(Reg::R11);

        Ok(vec![
          Instruction::Mov {
            src: right,
            dst: reg,
          },
          Instruction::Cmp { left, right: reg },
        ])
      },
      | other => Ok(vec![other]),
    }
  }
}
