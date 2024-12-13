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
  }

  fn emit_function(&mut self, function: &aast::Function) {
    self.writeln(format!("\t.globl _{}", &function.name));
    self.writeln(format!("_{}:", &function.name));

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
    }
  }

  fn emit_operand(&mut self, operand: &aast::Operand) -> String {
    match operand {
      | aast::Operand::Imm(int) => format!("${int}"),
      | aast::Operand::Register => "%eax".to_string(),
    }
  }
}
