use std::fs;
use std::path::PathBuf;
use std::process::Command;

use rayon::prelude::*;

#[test]
fn test_compiler() {
  // (suite_path, should_succeed)
  let suites = vec![
    ("tests/fixtures/valid", true),
    ("tests/fixtures/invalid_lex", false),
    ("tests/fixtures/invalid_parse", false),
    ("tests/fixtures/invalid_semantics", false),
  ];

  suites.par_iter().for_each(|(suite_path, should_succeed)| {
    let suite_path = PathBuf::from(suite_path);

    let suite_name = suite_path
      .file_name()
      .expect("should have suite name")
      .to_string_lossy()
      .to_string();

    let inputs = suite_path
      .read_dir()
      .expect("suite directory should exist")
      .filter_map(|entry| {
        entry.ok().and_then(|entry| {
          let path = entry.path();

          if path.extension().and_then(|ext| ext.to_str()) == Some("c") {
            Some(path)
          } else {
            None
          }
        })
      })
      .collect::<Vec<_>>();

    inputs.par_iter().for_each(|input| {
      let file = input
        .file_name()
        .expect("should have file name")
        .to_string_lossy()
        .to_string();

      let binary = input.with_extension("out");

      // Buffer output for this file.
      let mut message = String::new();

      message.push_str(&format!("{suite_name}/{file} ... "));

      let output = Command::new("cargo")
        .args(["run", "--quiet", "--package", "cyan", "--"])
        .args([&input.display().to_string()])
        .args(["-o", &binary.display().to_string()])
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

      // Remove generated binary.
      if binary.is_file() {
        fs::remove_file(&binary).expect("should remove binary");
      }

      // Perform assertions.
      if *should_succeed {
        assert!(succeeded);
      } else {
        assert!(!succeeded);
      }
    });
  });
}
