mod extensions;

use std::fmt;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::{self, Command};

use clap::{Parser, ValueEnum};
use cyan_compiler::analysis;
use cyan_compiler::context;
use cyan_compiler::emitter;
use cyan_compiler::ir::aast;
use cyan_compiler::ir::tac;
use cyan_compiler::lexer;
use cyan_compiler::parser;
use extensions::WithoutFileExtension;

/// Helper macro to bail on a specified [CompileStage].
macro_rules! bail_on {
  ($options: expr, $stage: expr) => {
    if $options.should_bail($stage) {
      let message = format!("WARN | Bailed on stage '{}'", $stage);
      let message = format!("{}", boxed(&message));

      println!("{message}");

      return Ok(CompileStatus::Bailed);
    }
  };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum CompileStage {
  Lex,
  Parse,
  Verify,
  Tac,
  Codegen,
  Emit,
  Link,
}

impl fmt::Display for CompileStage {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let stage = match self {
      | CompileStage::Lex => "Lex",
      | CompileStage::Parse => "Parse",
      | CompileStage::Verify => "Verify",
      | CompileStage::Tac => "TAC",
      | CompileStage::Codegen => "Codegen",
      | CompileStage::Emit => "Emit",
      | CompileStage::Link => "Link",
    };

    write!(f, "{stage}")
  }
}

#[derive(Clone, Copy, Debug)]
enum CompileStatus {
  Success,
  Bailed,
}

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Cli {
  /// C program to compile.
  input: String,
  /// Output file.
  #[arg(short, long)]
  output: Option<String>,
  /// Stages to print debug output for.
  #[arg(short, long, num_args = 1.., value_enum, value_delimiter = ' ')]
  print: Vec<CompileStage>,
  /// Stage to stop compilation at.
  #[arg(short, long, value_enum)]
  stage: Option<CompileStage>,
  /// Whether to cleanup the intermediate files.
  #[arg(short, long)]
  cleanup: Option<bool>,
}

#[derive(Debug)]
struct CompileOptions {
  /// Path to the file to compile.
  input: PathBuf,
  /// Output file.
  output: PathBuf,
  /// Whether to cleanup the intermediate files.
  cleanup: bool,
  /// Stages to print debug output for.
  print: Vec<CompileStage>,
  /// Stage to stop compilation at.
  stage: Option<CompileStage>,
}

impl CompileOptions {
  /// Returns `true` if the given stage should be printed.
  fn should_print(&self, stage: CompileStage) -> bool {
    self.print.contains(&stage)
  }

  /// Returns `true` if the given stage should be bailed on.
  fn should_bail(&self, target: CompileStage) -> bool {
    match self.stage {
      | Some(stage) => stage == target,
      | None => false,
    }
  }
}

/// Returns a string with an ASCII box around the given text.
fn boxed(text: &str) -> String {
  let mut header = String::new();
  let size = text.len();

  let line = "+-".to_string() + "-".repeat(size).as_str() + "-+\n";
  let message = format!("| {text:<size$} |\n", size = size);

  header.push_str(&line);
  header.push_str(&message);
  header.push_str(&line);

  header
}

/// Compiles the given C source file.
fn compile(options: &CompileOptions) -> anyhow::Result<CompileStatus> {
  let preprocessed = options.input.with_extension("i");
  let assembly = options.input.with_extension("s");

  let contents = fs::read_to_string(&preprocessed)?;

  // -----------------------------------------------------------------------------------------------
  // Lex:

  let mut lexer = lexer::Lexer::new(contents.as_bytes());
  let tokens = lexer.lex();

  if options.should_print(CompileStage::Lex) {
    println!("{}", boxed("Stage | Lex (tokens)"));
    println!("[");

    for token in &tokens {
      println!("  {token}");
    }

    println!("]\n");
  }

  bail_on!(options, CompileStage::Lex);

  // -----------------------------------------------------------------------------------------------
  // Parse:

  let mut parser = parser::Parser::new(tokens);
  let ast = parser.parse()?;

  if options.should_print(CompileStage::Parse) {
    println!("{}", boxed("Stage | Parse (AST)"));
    println!("{ast:#?}");
    println!();
  }

  bail_on!(options, CompileStage::Parse);

  // -----------------------------------------------------------------------------------------------
  // Analysis/verification passes:

  let mut ctx = context::Context::new();

  let mut pass = analysis::VarResolutionPass::new(&mut ctx);
  let ast = pass.run(&ast)?;

  let mut pass = analysis::LabelsResolutionPass::new();
  pass.run(&ast)?;

  let mut pass = analysis::LoopLabelingPass::new(&mut ctx);
  let ast = pass.run(&ast)?;

  let mut pass = analysis::SwitchResolutionPass::new(&mut ctx);
  let ast = pass.run(&ast)?;

  if options.should_print(CompileStage::Verify) {
    println!("{}", boxed("Stage | Verify (AST)"));
    println!("{ast:#?}\n");
  }

  bail_on!(options, CompileStage::Verify);

  // -----------------------------------------------------------------------------------------------
  // TAC:

  let mut lowerer = tac::LoweringPass::new(&mut ctx);
  let tac = lowerer.lower(&ast)?;

  if options.should_print(CompileStage::Tac) {
    println!("{}", boxed("Stage | Three Address Code (TAC)"));
    println!("{tac:#?}\n");
  }

  bail_on!(options, CompileStage::Tac);

  // -----------------------------------------------------------------------------------------------
  // Codegen:

  // Lower TAC to AAST.
  let lowerer = aast::LoweringPass::new();
  let aast = lowerer.lower(&tac)?;

  // Replace pseudoregisters and get the stack size.
  let mut pass = aast::PseudoReplacementPass::new();
  let (aast, stack_size) = pass.run(&aast)?;

  // Fix up instructions.
  let pass = aast::InstructionFixupPass::new(stack_size);
  let aast = pass.run(&aast)?;

  if options.should_print(CompileStage::Codegen) {
    println!("{}", boxed("Stage | Codegen (AAST)"));
    println!("stack_size = {stack_size}");
    println!("{aast:#?}\n");
  }

  bail_on!(options, CompileStage::Codegen);

  // -----------------------------------------------------------------------------------------------
  // Emission:

  // Emit assembly.
  let emitter = emitter::Emitter::new();
  let emitted = emitter.emit(&aast);

  if options.should_print(CompileStage::Codegen) {
    println!("{}", boxed("Stage | Emission (Assembly)"));
    println!("{}\n", &emitted.to_string());
  }

  bail_on!(options, CompileStage::Emit);

  // Write assembly to file.
  fs::write(&assembly, emitted.into_bytes())
    .map_err(|_| anyhow::anyhow!("Failed to write assembly file: {}", assembly.display()))?;

  // Link.
  if options.should_print(CompileStage::Link) {
    println!("{}", boxed("Stage | Link"));
    println!("<unimplemented>\n");
  }

  bail_on!(options, CompileStage::Link);

  Ok(CompileStatus::Success)
}

/// Preprocesses the given C source file using GCC.
fn preprocess(options: &CompileOptions) -> anyhow::Result<()> {
  let preprocessed = options.input.with_extension("i");

  let output = Command::new("gcc")
    .arg("-E")
    .args(["-P", &options.input.display().to_string()])
    .args(["-o", &preprocessed.display().to_string()])
    .output()?;

  if !output.status.success() {
    io::stdout().write_all(&output.stdout)?;
    io::stderr().write_all(&output.stderr)?;
  }

  Ok(())
}

/// Links the given assembly file using GCC.
fn link(options: &CompileOptions) -> anyhow::Result<()> {
  let assembly = options.input.with_extension("s");

  if !assembly.is_file() {
    return Err(anyhow::anyhow!("Assembly file not found"));
  }

  let output = Command::new("gcc")
    .arg(assembly.display().to_string())
    .args(["-o", &options.output.display().to_string()])
    .output()?;

  if !output.status.success() {
    io::stdout().write_all(&output.stdout)?;
    io::stderr().write_all(&output.stderr)?;
  }

  Ok(())
}

/// Cleans up the intermediate files produced by GCC.
fn cleanup_compile(options: &CompileOptions) -> anyhow::Result<()> {
  if !options.cleanup {
    return Ok(());
  }

  let preprocessed = options.input.with_extension("i");

  if preprocessed.is_file() {
    fs::remove_file(&preprocessed).map_err(|_| {
      anyhow::anyhow!(
        "Failed to remove preprocessed file: {}",
        preprocessed.display()
      )
    })?;
  }

  Ok(())
}

/// Cleans up the intermediate files produced by the compiler.
fn cleanup_link(options: &CompileOptions) -> anyhow::Result<()> {
  if !options.cleanup {
    return Ok(());
  }

  let assembly = options.input.with_extension("s");

  if assembly.is_file() {
    fs::remove_file(&assembly)
      .map_err(|_| anyhow::anyhow!("Failed to remove assembly file: {}", assembly.display()))?;
  }

  Ok(())
}

/// Returns the input and output paths from CLI.
fn get_paths(cli: &Cli) -> anyhow::Result<(PathBuf, PathBuf)> {
  let input = PathBuf::from(&cli.input).canonicalize()?;

  let output = cli
    .output
    .as_ref()
    .map(PathBuf::from)
    .unwrap_or(input.without_extension());

  Ok((input, output))
}

/// Executes the compiler.
fn execute(options: &CompileOptions) -> anyhow::Result<()> {
  // Preprocess.
  preprocess(options)?;

  // Compile and then link.
  match compile(options) {
    | Ok(CompileStatus::Success) => {
      cleanup_compile(options)?;
      link(options)?;
      cleanup_link(options)
    },
    | Ok(CompileStatus::Bailed) => {
      cleanup_compile(options)?;
      Ok(())
    },
    | Err(err) => {
      cleanup_compile(options)?;
      Err(err)
    },
  }
}

/// Compiler driver.
fn driver() -> anyhow::Result<()> {
  let cli = Cli::parse();

  // Get paths.
  let (input, output) = get_paths(&cli)?;

  // Compile options from CLI.
  let options = CompileOptions {
    input,
    output,
    cleanup: cli.cleanup.unwrap_or(true),
    print: cli.print,
    stage: cli.stage,
  };

  execute(&options)
}

fn main() {
  if let Err(err) = driver() {
    eprintln!("{err}");
    process::exit(1);
  }
}
