use crate::symbol::Symbol;
use crate::symtable::Symtable;

/// The compiler context.
///
/// This context is used to store global information about the compiler, such as the current
/// function, the variable counter, etc.
#[derive(Debug)]
pub struct Context {
  /// The current function name.
  pub var_prefix: Symbol,
  /// The current variables/labels counter.
  pub var_counter: usize,
  /// The symbol table.
  pub symtable: Symtable,
}

impl Context {
  pub fn new() -> Self {
    Self {
      var_prefix: "global".to_string().into(),
      var_counter: 0,
      symtable: Symtable::new(),
    }
  }

  /// Generates a label name using `label` as a prefix and increments the counter.
  pub fn gen_label(&mut self, label: &str) -> Symbol {
    let label = format!("{}.{}.{}", self.var_prefix, label, self.var_counter);
    self.var_counter += 1;

    label.into()
  }

  /// Generates a temporary variable name scoped to the current function and increments the counter.
  pub fn gen_temporary(&mut self) -> Symbol {
    let name = format!("{}.{}", self.var_prefix, self.var_counter);
    self.var_counter += 1;

    name.into()
  }
}
