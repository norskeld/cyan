use internment::Intern;

/// The compiler context.
///
/// This context is used to store global information about the compiler, such as the current
/// function, the variable counter, etc.
pub struct Context {
  /// The current function.
  pub var_prefix: Intern<String>,
  /// The current variables/labels counter.
  pub var_counter: usize,
}

impl Context {
  pub fn new() -> Self {
    Self {
      var_prefix: "global".to_string().into(),
      var_counter: 0,
    }
  }

  /// Generates a label name using `label` as a prefix and increments the counter.
  pub fn gen_label(&mut self, label: &str) -> Intern<String> {
    let label = format!("{}.{}", self.var_prefix, label);
    self.var_counter += 1;

    label.into()
  }

  /// Generates a temporary variable name scoped to the current function and increments the counter.
  pub fn gen_temporary(&mut self) -> Intern<String> {
    let name = format!("{}.{}", self.var_prefix, self.var_counter);
    self.var_counter += 1;

    name.into()
  }
}
