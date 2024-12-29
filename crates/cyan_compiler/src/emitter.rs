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

  pub fn emit(mut self, program: &Program) -> Output {
    self.emit_program(program);

    self.output
  }
}

impl Emitter {
  fn emit_program(&mut self, program: &Program) {
    self.emit_function(&program.function);

    if cfg!(target_os = "linux") {
      self
        .output
        .writeln(".section .note.GNU-stack,\"\",@progbits\n");
    }
  }

  fn emit_function(&mut self, function: &Function) {
    let name = if cfg!(target_os = "macos") {
      format!("_{}", function.name)
    } else {
      function.name.to_string()
    };

    self.output.writeln(format!(".globl {name}\n"));
    self.output.writeln(format!("{name}:"));

    // Emit function prologue.
    self.output.writeln("\tpushq %rbp");
    self.output.writeln("\tmovq %rsp, %rbp");

    for instruction in &function.instructions {
      self.emit_instruction(instruction);
    }
  }

  fn emit_instruction(&mut self, instruction: &Instruction) {
    match instruction {
      | Instruction::Mov { src, dst } => {
        let src = self.emit_operand(src);
        let dst = self.emit_operand(dst);

        self.output.writeln(format!("\tmovl {src}, {dst}"));
      },
      | Instruction::Ret => {
        // Emit function epilogue.
        self.output.writeln("\tmovq %rbp, %rsp");
        self.output.writeln("\tpopq %rbp");

        self.output.writeln("\tret");
      },
      | Instruction::Unary { op, operand } => {
        let op = self.emit_unary_op(op);
        let operand = self.emit_operand(operand);

        self.output.writeln(format!("\t{op} {operand}"));
      },
      | Instruction::AllocateStack(size) => {
        self.output.writeln(format!("\tsubq ${size}, %rsp"));
      },
      | Instruction::Binary { op, src, dst } => {
        let op = self.emit_binary_op(op);
        let src = self.emit_operand(src);
        let dst = self.emit_operand(dst);

        self.output.writeln(format!("\t{op} {src}, {dst}"));
      },
      | Instruction::Idiv(operand) => {
        let operand = self.emit_operand(operand);

        self.output.writeln(format!("\tidivl {operand}"));
      },
      | Instruction::Cdq => {
        self.output.writeln("\tcdq");
      },
    }
  }

  fn emit_operand(&mut self, operand: &Operand) -> String {
    match operand {
      | Operand::Imm(int) => format!("${int}"),
      | Operand::Reg(reg) => self.emit_register(reg),
      | Operand::Stack(offset) => format!("{offset}(%rbp)"),
      | Operand::Pseudo(..) => unreachable!("unexpected pseudoregister"),
    }
  }

  fn emit_unary_op(&self, op: &UnaryOp) -> String {
    match op {
      | UnaryOp::Neg => "negl".to_string(),
      | UnaryOp::Not => "notl".to_string(),
    }
  }

  fn emit_binary_op(&self, op: &BinaryOp) -> String {
    match op {
      | BinaryOp::Add => "addl".to_string(),
      | BinaryOp::Sub => "subl".to_string(),
      | BinaryOp::Mul => "imull".to_string(),
    }
  }

  fn emit_register(&self, register: &Reg) -> String {
    match register {
      | Reg::AX => "%eax".to_string(),
      | Reg::DX => "%edx".to_string(),
      | Reg::R10 => "%r10d".to_string(),
      | Reg::R11 => "%r11d".to_string(),
    }
  }
}
