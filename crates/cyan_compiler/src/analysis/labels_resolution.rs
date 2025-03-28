use std::collections::HashSet;

use cyan_reporting::Location;
use thiserror::Error;

use crate::ir::ast::*;

type Result<T> = std::result::Result<T, LabelsResolutionError>;

#[derive(Debug, Error)]
#[error("labels resolution error {location}: {message}")]
pub struct LabelsResolutionError {
  /// The error message.
  message: String,
  /// The location of the error.
  location: Location,
}

impl LabelsResolutionError {
  pub fn new(message: impl AsRef<str> + Into<String>, location: Location) -> Self {
    Self {
      message: message.into(),
      location,
    }
  }
}

#[derive(Debug, Default)]
struct LabelsResolutionState {
  defined_labels: HashSet<Ident>,
  used_labels: HashSet<Ident>,
}

impl LabelsResolutionState {
  /// Get difference between used and defined labels.
  pub(crate) fn diff(&self) -> Option<Vec<Ident>> {
    let diff: Vec<_> = self
      .used_labels
      .difference(&self.defined_labels)
      .copied()
      .collect();

    if diff.is_empty() {
      None
    } else {
      Some(diff)
    }
  }

  /// Adds a label to the set of used labels.
  pub(crate) fn use_label(&mut self, label: Ident) {
    self.used_labels.insert(label);
  }

  /// Adds a label to the set of defined labels.
  pub(crate) fn define_label(&mut self, label: Ident) {
    self.defined_labels.insert(label);
  }

  /// Returns the defined label with the given name.
  pub(crate) fn get_defined_label(&self, label: &Ident) -> Option<&Ident> {
    self.defined_labels.get(label)
  }
}

pub struct LabelsResolutionPass {
  state: LabelsResolutionState,
}

impl LabelsResolutionPass {
  pub fn new() -> Self {
    Self {
      state: LabelsResolutionState::default(),
    }
  }

  pub fn run(&mut self, program: &Program) -> Result<()> {
    self.resolve_function(&program.function)
  }

  fn resolve_function(&mut self, function: &Function) -> Result<()> {
    self.resolve_block(&function.body)?;

    let diff = self.state.diff();

    // TODO: When diagnostics are implemented, we should report _all_ errors.
    if let Some(ident) = diff.and_then(|it| it.first().cloned()) {
      Err(LabelsResolutionError::new(
        format!("label '{}' was referenced but not defined", ident.value),
        ident.location,
      ))
    } else {
      Ok(())
    }
  }

  fn resolve_block(&mut self, block: &Block) -> Result<()> {
    for block_item in &block.body {
      self.resolve_block_item(block_item)?;
    }

    Ok(())
  }

  fn resolve_block_item(&mut self, block_item: &BlockItem) -> Result<()> {
    match block_item {
      | BlockItem::Declaration(..) => Ok(()),
      | BlockItem::Statement(statement) => self.resolve_statement(statement),
    }
  }

  fn resolve_statement(&mut self, statement: &Statement) -> Result<()> {
    match statement {
      | Statement::Goto(goto) => self.resolve_goto(goto),
      | Statement::Labeled(labeled) => self.resolve_labeled(labeled),
      | Statement::If(conditional) => self.resolve_if(conditional),
      | Statement::Block(block) => self.resolve_block(block),
      | Statement::For(for_) => self.resolve_statement(&for_.body),
      | Statement::While(while_) => self.resolve_statement(&while_.body),
      | Statement::DoWhile(do_while) => self.resolve_statement(&do_while.body),
      | Statement::Switch(switch) => self.resolve_statement(&switch.body),
      | Statement::Case(case) => self.resolve_statement(&case.body),
      | Statement::DefaultCase(default_case) => self.resolve_statement(&default_case.body),
      | Statement::Expression(..)
      | Statement::Break(..)
      | Statement::Continue(..)
      | Statement::Return(..)
      | Statement::Null { .. } => Ok(()),
    }
  }

  fn resolve_goto(&mut self, goto: &Goto) -> Result<()> {
    self.state.use_label(goto.label);

    Ok(())
  }

  fn resolve_labeled(&mut self, labeled: &Labeled) -> Result<()> {
    if let Some(label) = self.state.get_defined_label(&labeled.label) {
      return Err(LabelsResolutionError::new(
        format!("duplicate label '{}'", label.value),
        label.location,
      ));
    }

    self.state.define_label(labeled.label);

    self.resolve_statement(&labeled.statement)
  }

  fn resolve_if(&mut self, conditional: &If) -> Result<()> {
    self.resolve_statement(&conditional.then)?;

    match &conditional.otherwise {
      | Some(otherwise) => self.resolve_statement(otherwise),
      | None => Ok(()),
    }
  }
}
