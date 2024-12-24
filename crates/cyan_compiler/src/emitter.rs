use crate::trees::aast;

#[derive(Debug, Default)]
pub struct Emitter {
  output: Vec<u8>,
}

impl Emitter {
  pub fn new() -> Self {
    Self { output: Vec::new() }
  }

  pub fn emit(mut self, program: &aast::Program) -> Vec<u8> {
    self.emit_program(program);

    self.output
  }

  fn write(&mut self, text: impl AsRef<str>) {
    self.output.extend(text.as_ref().as_bytes());
  }

  fn writeln(&mut self, text: impl AsRef<str>) {
    self.write(text);
    self.write("\n");
  }
}

impl Emitter {
  fn emit_program(&mut self, program: &aast::Program) {
    self.emit_function(&program.function);

    if cfg!(target_os = "linux") {
      self.writeln(".section .note.GNU-stack,\"\",@progbits\n");
    }
  }

  fn emit_function(&mut self, function: &aast::Function) {
    let name = if cfg!(target_os = "macos") {
      format!("_{}", function.name)
    } else {
      function.name.to_string()
    };

    self.writeln(format!(".globl {name}\n"));
    self.writeln(format!("{name}:"));

    for instruction in &function.instructions {
      self.emit_instruction(instruction);
    }
  }

  fn emit_instruction(&mut self, instruction: &aast::Instruction) {
    match instruction {
      | aast::Instruction::Mov { src, dst } => {
        let src = self.emit_operand(src);
        let dst = self.emit_operand(dst);

        self.writeln(format!("\tmovl {src}, {dst}"));
      },
      | aast::Instruction::Ret => {
        self.writeln("\tret");
      },
      | aast::Instruction::Unary { .. } => unimplemented!(),
      | aast::Instruction::AllocateStack(..) => unimplemented!(),
    }
  }

  fn emit_operand(&mut self, operand: &aast::Operand) -> String {
    match operand {
      | aast::Operand::Imm(int) => format!("${int}"),
      | aast::Operand::Reg(..) => "%eax".to_string(),
      | aast::Operand::Pseudo(..) => unimplemented!("pseudoregisters"),
      | aast::Operand::Stack(..) => unimplemented!("stack"),
    }
  }
}
