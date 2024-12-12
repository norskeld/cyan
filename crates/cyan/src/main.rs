use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;

use clap::Parser;
use cyan_compiler::lexer;
use cyan_compiler::parser;

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Cli {
  /// C program to compile.
  input: String,
  /// Run the lexer, but stop before parsing.
  #[arg(short, long)]
  lex: bool,
  /// Run the lexer and parser, but stop before codegen.
  #[arg(short, long)]
  parse: bool,
  /// Perform lexing, parsing, and assembly generation, but stop before codegen.
  #[arg(short, long)]
  codegen: bool,
  /// Generate assembly, but do not perform linking.
  #[arg(short = 'S', long)]
  link: bool,
}

#[derive(Debug)]
struct CompileOptions {
  lex: bool,
  parse: bool,
  codegen: bool,
  link: bool,
}

fn header(text: &str) -> String {
  let mut header = String::new();

  header.push_str("+----------------------------+\n");
  header.push_str(format!("| {text:<26} |\n").as_str());
  header.push_str("+----------------------------+\n");

  header
}

fn compile(path: impl AsRef<Path>, options: CompileOptions) -> anyhow::Result<()> {
  let preprocessed = path.as_ref().with_extension("i");
  let _assembly = path.as_ref().with_extension("s");

  let contents = fs::read_to_string(&preprocessed)?;

  let mut lexer = lexer::Lexer::new(contents.as_bytes());
  let tokens = lexer.lex();

  if options.lex {
    println!("{}", header("Lex (tokens)"));
    println!("[");

    for token in &tokens {
      println!("  {token}");
    }

    println!("]\n");
  }

  let mut parser = parser::Parser::new(tokens);
  let program = parser.parse()?;

  if options.parse {
    println!("{}", header("Parse (AST)"));
    println!("{program:#?}");
    println!();
  }

  if options.codegen {
    println!("{}", header("Codegen (assembly)"));
    println!("<unimplemented>");
    println!();
  }

  if options.link {
    println!("{}", header("Link"));
    println!("<unimplemented>");
    println!();
  }

  fs::remove_file(&preprocessed)?;

  Err(anyhow::anyhow!("Compilation is not implemented yet"))
}

fn preprocess(path: impl AsRef<Path>) -> anyhow::Result<()> {
  let preprocessed = path.as_ref().with_extension("i");

  let output = Command::new("gcc")
    .arg("-E")
    .args(["-P", &path.as_ref().display().to_string()])
    .args(["-o", &preprocessed.display().to_string()])
    .output()?;

  if !output.status.success() {
    io::stdout().write_all(&output.stdout)?;
    io::stderr().write_all(&output.stderr)?;
  }

  Ok(())
}

fn link(path: impl AsRef<Path>) -> anyhow::Result<()> {
  let assembly = path.as_ref().with_extension("s");

  if !assembly.is_file() {
    return Ok(());
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

  fs::remove_file(&assembly)?;

  Ok(())
}

/// Compiler driver.
fn main() -> anyhow::Result<()> {
  let cli = Cli::parse();

  // Absolute file path.
  let path = PathBuf::from(cli.input).canonicalize()?;

  // Compile options.
  let compile_options = CompileOptions {
    lex: cli.lex,
    parse: cli.parse,
    codegen: cli.codegen,
    link: cli.link,
  };

  // Preprocess via gcc/clang.
  preprocess(&path)
    .and_then(|_| compile(&path, compile_options))
    .and_then(|_| link(&path))
}
