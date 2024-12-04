use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;

use clap::Parser;

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Cli {
  /// C program to compile.
  input: String,
  /// Run the lexer, but stop before parsing.
  #[arg(short, long, exclusive = true)]
  lex: bool,
  /// Run the lexer and parser, but stop before codegen.
  #[arg(short, long, exclusive = true)]
  parse: bool,
  /// Perform lexing, parsing, and assembly generation, but stop before codegen.
  #[arg(short, long, exclusive = true)]
  codegen: bool,
  /// Generate assembly, but do not perform linking.
  #[arg(short = 'S', long, exclusive = true)]
  link: bool,
}

#[derive(Debug)]
struct CompileOptions {
  lex: bool,
  parse: bool,
  codegen: bool,
  link: bool,
}

fn compile(path: impl AsRef<Path>, options: CompileOptions) -> anyhow::Result<()> {
  let preprocessed = path.as_ref().with_extension("i");
  let _assembly = path.as_ref().with_extension("s");

  if options.lex {
    return Ok(println!("Lexing"));
  } else if options.parse {
    return Ok(println!("Lexing >> Parsing"));
  } else if options.codegen {
    return Ok(println!("Lexing >> Parsing >> Codegen"));
  } else if options.link {
    return Ok(println!("Lexing >> Parsing >> Codegen >> Emit"));
  }

  fs::remove_file(&preprocessed)?;

  unimplemented!("compilation");
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

  let stem = assembly
    .file_stem()
    .map(|it| it.to_string_lossy().to_string())
    .expect("Should contain valid file name");

  let output = Command::new("gcc")
    .arg(&assembly.display().to_string())
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

  // Preprocess via gcc/clang.
  preprocess(&path)?;

  // Compile via cyan.
  compile(
    &path,
    CompileOptions {
      lex: cli.lex,
      parse: cli.parse,
      codegen: cli.codegen,
      link: cli.link,
    },
  )?;

  // Link via gcc/clang.
  link(&path)?;

  Ok(())
}
