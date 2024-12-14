use std::path::PathBuf;
use std::process::Command;

use rayon::prelude::*;

#[test]
fn test_compiler() {
  let suites = vec![
    ("valid", "tests/fixtures/valid", true),
    ("invalid_lex", "tests/fixtures/invalid_lex", false),
    ("invalid_parse", "tests/fixtures/invalid_parse", false),
  ];

  suites.par_iter().for_each(|(name, suite, should_succeed)| {
    let suite = PathBuf::from(suite);
    let inputs = suite.read_dir().expect("should have input files");

    inputs
      .flatten()
      .filter_map(|it| {
        let binding = it.path();
        let extension = &binding.extension()?;

        if *extension == "c" {
          Some(binding)
        } else {
          None
        }
      })
      .par_bridge()
      .for_each(|input| {
        if !input.is_file() {
          return;
        }

        let file = input
          .file_name()
          .expect("should have file name")
          .to_string_lossy()
          .to_string();

        // Buffer output for this file.
        let mut message = String::new();

        message.push_str(&format!("{name}/{file} ... "));

        let output = Command::new("cargo")
          .args(["run", "--quiet", "--package", "cyan", "--"])
          .args([&input.display().to_string()])
          .output()
          .expect("should get output");

        let succeeded = output.status.success();

        if should_succeed == &succeeded {
          message.push_str("PASS\n");
        } else {
          let actual_result = if succeeded { "OK" } else { "FAIL" };
          let expected_result = if *should_succeed { "OK" } else { "FAIL" };

          message.push_str(&format!(
            "{actual_result}, but expected {expected_result}\n"
          ));
        }

        // Print buffered output before assertion.
        print!("{message}");

        // Perform assertions.
        if *should_succeed {
          assert!(succeeded);
        } else {
          assert!(!succeeded);
        }
      });
  });
}
