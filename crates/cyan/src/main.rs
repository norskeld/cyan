use std::fmt;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::Command;

use clap::{Parser, ValueEnum};
use cyan_compiler::lexer;
use cyan_compiler::parser;
use cyan_compiler::trees::aast;

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
  Codegen,
  Link,
}

impl fmt::Display for CompileStage {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let stage = match self {
      | CompileStage::Lex => "Lex",
      | CompileStage::Parse => "Parse",
      | CompileStage::Codegen => "Codegen",
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
  /// Stages to print debug output for.
  #[arg(short, long, num_args = 1.., value_enum, value_delimiter = ' ')]
  print: Vec<CompileStage>,
  /// Stage to stop compilation at.
  #[arg(short, long, value_enum)]
  stage: Option<CompileStage>,
}

#[derive(Debug)]
struct CompileOptions {
  /// Path to the file to compile.
  path: PathBuf,
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
  let preprocessed = options.path.with_extension("i");
  let contents = fs::read_to_string(&preprocessed)?;

  // Lex.
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

  // Parse.
  let mut parser = parser::Parser::new(tokens);
  let program = parser.parse()?;

  if options.should_print(CompileStage::Parse) {
    println!("{}", boxed("Stage | Parse (AST)"));
    println!("{program:#?}");
    println!();
  }

  bail_on!(options, CompileStage::Parse);

  // Codegen.
  let aast = aast::Lowerer::new(program).lower()?;

  if options.should_print(CompileStage::Codegen) {
    println!("{}", boxed("Stage | AAST (Assembly AST)"));
    println!("{aast:#?}");
    println!();

    println!("{}", boxed("Stage | Codegen"));
    println!("<unimplemented>");
    println!();
  }

  bail_on!(options, CompileStage::Codegen);

  // Link.
  if options.should_print(CompileStage::Link) {
    println!("{}", boxed("Stage | Link"));
    println!("<unimplemented>");
    println!();
  }

  bail_on!(options, CompileStage::Link);

  Ok(CompileStatus::Success)
}

/// Preprocesses the given C source file using GCC.
fn preprocess(options: &CompileOptions) -> anyhow::Result<()> {
  let preprocessed = options.path.with_extension("i");

  let output = Command::new("gcc")
    .arg("-E")
    .args(["-P", &options.path.display().to_string()])
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
  let assembly = options.path.with_extension("s");

  if !assembly.is_file() {
    return Err(anyhow::anyhow!("Assembly file not found"));
  }

  let stem = assembly
    .file_stem()
    .map(|it| it.to_string_lossy().to_string())
    .expect("Should contain valid file name");

  let output = Command::new("gcc")
    .arg(assembly.display().to_string())
    .args(["-o", &stem])
    .output()?;

  if !output.status.success() {
    io::stdout().write_all(&output.stdout)?;
    io::stderr().write_all(&output.stderr)?;
  }

  Ok(())
}

fn cleanup(options: &CompileOptions) -> anyhow::Result<()> {
  let assembly = options.path.with_extension("s");
  let preprocessed = options.path.with_extension("i");

  if preprocessed.is_file() {
    fs::remove_file(&preprocessed).map_err(|_| {
      anyhow::anyhow!(
        "Failed to remove preprocessed file: {}",
        preprocessed.display()
      )
    })?;
  }

  if assembly.is_file() {
    fs::remove_file(&assembly)
      .map_err(|_| anyhow::anyhow!("Failed to remove assembly file: {}", assembly.display()))?;
  }

  Ok(())
}

fn execute(options: &CompileOptions) -> anyhow::Result<()> {
  // Preprocess.
  preprocess(options)?;

  // Compile and then link.
  match compile(options) {
    | Ok(CompileStatus::Success) => link(options),
    | Ok(CompileStatus::Bailed) => Ok(()),
    | Err(err) => Err(err),
  }
}

/// Compiler driver.
fn main() -> anyhow::Result<()> {
  let cli = Cli::parse();

  // Compile options from CLI.
  let options = CompileOptions {
    path: PathBuf::from(&cli.input).canonicalize()?,
    print: cli.print,
    stage: cli.stage,
  };

  match execute(&options) {
    | Ok(..) => cleanup(&options),
    | Err(err) => {
      println!("{err}");
      cleanup(&options)
    },
  }
}
