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
  fn diff(&self) -> Option<Vec<Ident>> {
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
}

pub struct LabelsResolutionPass;

impl LabelsResolutionPass {
  pub fn new() -> Self {
    Self
  }

  pub fn run(&self, program: &Program) -> Result<()> {
    self.resolve_function(&program.function)
  }

  fn resolve_function(&self, function: &Function) -> Result<()> {
    let mut state = LabelsResolutionState::default();

    self.resolve_block(&function.body, &mut state)?;

    let diff = state.diff();

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

  fn resolve_block(&self, block: &Block, state: &mut LabelsResolutionState) -> Result<()> {
    for block_item in &block.body {
      self.resolve_block_item(block_item, state)?;
    }

    Ok(())
  }

  fn resolve_block_item(
    &self,
    block_item: &BlockItem,
    state: &mut LabelsResolutionState,
  ) -> Result<()> {
    match block_item {
      | BlockItem::Declaration(..) => Ok(()),
      | BlockItem::Statement(statement) => self.resolve_statement(statement, state),
    }
  }

  fn resolve_statement(
    &self,
    statement: &Statement,
    state: &mut LabelsResolutionState,
  ) -> Result<()> {
    match statement {
      | Statement::Goto(goto) => self.resolve_goto(goto, state),
      | Statement::Labeled(labeled) => self.resolve_labeled(labeled, state),
      | Statement::If(conditional) => self.resolve_if(conditional, state),
      | Statement::Block(block) => self.resolve_block(block, state),
      | Statement::For(for_) => self.resolve_statement(&for_.body, state),
      | Statement::While(while_) => self.resolve_statement(&while_.body, state),
      | Statement::DoWhile(do_while) => self.resolve_statement(&do_while.body, state),
      | Statement::Expression(..)
      | Statement::Break(..)
      | Statement::Continue(..)
      | Statement::Return(..)
      | Statement::Null { .. } => Ok(()),
      | Statement::Switch(switch) => todo!(),
      | Statement::Case(case) => todo!(),
      | Statement::DefaultCase(default_case) => todo!(),
    }
  }

  fn resolve_goto(&self, goto: &Goto, state: &mut LabelsResolutionState) -> Result<()> {
    state.used_labels.insert(goto.label);

    Ok(())
  }

  fn resolve_labeled(&self, labeled: &Labeled, state: &mut LabelsResolutionState) -> Result<()> {
    if let Some(label) = state.defined_labels.get(&labeled.label) {
      return Err(LabelsResolutionError::new(
        format!("duplicate label '{}'", label.value),
        label.location,
      ));
    }

    state.defined_labels.insert(labeled.label);

    self.resolve_statement(&labeled.statement, state)
  }

  fn resolve_if(&self, conditional: &If, state: &mut LabelsResolutionState) -> Result<()> {
    self.resolve_statement(&conditional.then, state)?;

    match &conditional.otherwise {
      | Some(otherwise) => self.resolve_statement(otherwise, state),
      | None => Ok(()),
    }
  }
}
