use std::fmt;

use crate::ir::aast::*;

#[derive(Debug, Default)]
pub struct Output {
  bytes: Vec<u8>,
}

impl Output {
  /// Creates a new output.
  pub fn new() -> Self {
    Self { bytes: Vec::new() }
  }

  /// Writes a string to the output.
  pub fn write(&mut self, text: impl AsRef<str>) {
    self.bytes.extend(text.as_ref().as_bytes());
  }

  /// Writes a newline to the output.
  pub fn writeln(&mut self, text: impl AsRef<str>) {
    self.write(text);
    self.write("\n");
  }

  /// Returns the output as a byte vector.
  pub fn into_bytes(self) -> Vec<u8> {
    self.bytes
  }

  pub fn as_bytes(&self) -> &[u8] {
    &self.bytes
  }
}

impl fmt::Display for Output {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", String::from_utf8_lossy(&self.bytes))
  }
}

/// Assembly emitter. Emits x86-64 assembly using AT&T syntax.
#[derive(Debug, Default)]
pub struct Emitter {
  output: Output,
}

impl Emitter {
  pub fn new() -> Self {
    Self {
      output: Output::new(),
    }
  }

  /// Emits assembly code for the given program.
  pub fn emit(mut self, program: &Program) -> Output {
    self.emit_program(program);

    self.output
  }
}

impl Emitter {
  /// Emits assembly code for the program, including platform-specific sections.
  fn emit_program(&mut self, program: &Program) {
    self.emit_function(&program.function);

    if cfg!(target_os = "linux") {
      self
        .output
        .writeln(".section .note.GNU-stack,\"\",@progbits\n");
    }
  }

  /// Emits assembly code for a function, including prologue and instructions.
  fn emit_function(&mut self, function: &Function) {
    let name = self.gen_label(&function.name);

    self.output.writeln(format!(".globl {name}\n"));
    self.output.writeln(format!("{name}:"));

    // Emit function prologue.
    self.output.writeln("\tpushq %rbp");
    self.output.writeln("\tmovq %rsp, %rbp");

    for instruction in &function.instructions {
      self.emit_instruction(instruction);
    }
  }

  /// Emits x86-64 assembly code for a single instruction.
  fn emit_instruction(&mut self, instruction: &Instruction) {
    match instruction {
      | Instruction::Mov { src, dst } => {
        let src = self.translate_operand(src);
        let dst = self.translate_operand(dst);

        self.output.writeln(format!("\tmovl {src}, {dst}"));
      },
      | Instruction::Unary { op, operand } => {
        let op = self.translate_unary_op(op);
        let operand = self.translate_operand(operand);

        self.output.writeln(format!("\t{op} {operand}"));
      },
      | Instruction::Binary {
        op: op @ (BinaryOp::Sal | BinaryOp::Sar),
        src,
        dst,
      } => {
        let op = self.translate_binary_op(op);
        let src = self.translate_byte_operand(src);
        let dst = self.translate_operand(dst);

        self.output.writeln(format!("\t{op} {src}, {dst}"));
      },
      | Instruction::Binary { op, src, dst } => {
        let op = self.translate_binary_op(op);
        let src = self.translate_operand(src);
        let dst = self.translate_operand(dst);

        self.output.writeln(format!("\t{op} {src}, {dst}"));
      },
      | Instruction::Cmp { left, right } => {
        let left = self.translate_operand(left);
        let right = self.translate_operand(right);

        self.output.writeln(format!("\tcmpl {left}, {right}"));
      },
      | Instruction::Idiv(operand) => {
        let operand = self.translate_operand(operand);

        self.output.writeln(format!("\tidivl {operand}"));
      },
      | Instruction::Cdq => {
        self.output.writeln("\tcdq");
      },
      | Instruction::Jmp(label) => {
        let label = self.gen_local_label(label);

        self.output.writeln(format!("\tjmp {label}"));
      },
      | Instruction::JmpCC { code, label } => {
        let code = self.translate_cond_code(code);
        let label = self.gen_local_label(label);

        self.output.writeln(format!("\tj{code} {label}"));
      },
      | Instruction::SetCC { code, dst } => {
        let code = self.translate_cond_code(code);
        let dst = self.translate_byte_operand(dst);

        self.output.writeln(format!("\tset{code} {dst}"));
      },
      | Instruction::Label(label) => {
        let label = self.gen_local_label(label);

        self.output.writeln(format!("{label}:"));
      },
      | Instruction::AllocateStack(size) => {
        self.output.writeln(format!("\tsubq ${size}, %rsp"));
      },
      | Instruction::Ret => {
        // Emit function epilogue.
        self.output.writeln("\tmovq %rbp, %rsp");
        self.output.writeln("\tpopq %rbp");

        self.output.writeln("\tret");
      },
    }
  }

  /// Translates an operand to its assembly representation.
  fn translate_operand(&mut self, operand: &Operand) -> String {
    match operand {
      | Operand::Imm(int) => format!("${int}"),
      | Operand::Reg(reg) => self.translate_register(reg),
      | Operand::Stack(offset) => format!("{offset}(%rbp)"),
      | Operand::Pseudo(..) => unreachable!("unexpected pseudoregister"),
    }
  }

  /// Translates an operand to its byte-sized assembly representation.
  fn translate_byte_operand(&mut self, operand: &Operand) -> String {
    if let Operand::Reg(reg) = operand {
      let value = match reg {
        | Reg::AX => "%al",
        | Reg::CX => "%cl",
        | Reg::DX => "%dl",
        | Reg::R10 => "%r10b",
        | Reg::R11 => "%r11b",
      };

      value.to_string()
    } else {
      self.translate_operand(operand)
    }
  }

  /// Converts a unary operation to its assembly mnemonic.
  fn translate_unary_op(&self, op: &UnaryOp) -> String {
    match op {
      | UnaryOp::Neg => "negl".to_string(),
      | UnaryOp::Not => "notl".to_string(),
    }
  }

  /// Converts a binary operation to its assembly mnemonic.
  fn translate_binary_op(&self, op: &BinaryOp) -> String {
    match op {
      | BinaryOp::And => "andl".to_string(),
      | BinaryOp::Or => "orl".to_string(),
      | BinaryOp::Sal => "sall".to_string(),
      | BinaryOp::Sar => "sarl".to_string(),
      | BinaryOp::Xor => "xorl".to_string(),
      | BinaryOp::Add => "addl".to_string(),
      | BinaryOp::Mul => "imull".to_string(),
      | BinaryOp::Sub => "subl".to_string(),
    }
  }

  /// Translates a register to its assembly representation.
  fn translate_register(&self, register: &Reg) -> String {
    match register {
      | Reg::AX => "%eax".to_string(),
      | Reg::CX => "%ecx".to_string(),
      | Reg::DX => "%edx".to_string(),
      | Reg::R10 => "%r10d".to_string(),
      | Reg::R11 => "%r11d".to_string(),
    }
  }

  /// Translates a condition code to its assembly suffix.
  fn translate_cond_code(&self, code: &CondCode) -> String {
    match code {
      | CondCode::E => "e".to_string(),
      | CondCode::NE => "ne".to_string(),
      | CondCode::G => "g".to_string(),
      | CondCode::GE => "ge".to_string(),
      | CondCode::L => "l".to_string(),
      | CondCode::LE => "le".to_string(),
    }
  }

  /// Generates a platform-specific label name.
  fn gen_label(&self, label: &str) -> String {
    if cfg!(target_os = "macos") {
      format!("_{label}")
    } else {
      label.to_string()
    }
  }

  /// Generates a platform-specific local label name.
  fn gen_local_label(&self, label: &str) -> String {
    if cfg!(target_os = "macos") {
      format!("L{label}")
    } else {
      format!(".L{label}")
    }
  }
}
