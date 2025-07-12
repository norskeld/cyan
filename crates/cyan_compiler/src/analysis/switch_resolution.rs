use cyan_reporting::{Located, Location};
use thiserror::Error;

use crate::context::Context;
use crate::ir::ast::*;
use crate::symbol::Symbol;

type Result<T> = std::result::Result<T, SwitchResolutionError>;

#[derive(Debug, Error)]
#[error("switch resolution error {location}: {message}")]
pub struct SwitchResolutionError {
  /// The error message.
  message: String,
  /// The location of the error.
  location: Location,
}

impl SwitchResolutionError {
  pub fn new(message: impl AsRef<str> + Into<String>, location: Location) -> Self {
    Self {
      message: message.into(),
      location,
    }
  }
}

/// Traverse AST and collect case and default statements associated with each switch.
pub struct SwitchResolutionPass<'ctx> {
  ctx: &'ctx mut Context,
  inside_switch: bool,
}

impl<'ctx> SwitchResolutionPass<'ctx> {
  pub fn new(ctx: &'ctx mut Context) -> Self {
    Self {
      ctx,
      inside_switch: false,
    }
  }

  pub fn run(&mut self, program: &Program) -> Result<Program> {
    self.resolve_program(program)
  }

  fn resolve_program(&mut self, program: &Program) -> Result<Program> {
    let mut declarations = vec![];

    for declaration in &program.declarations {
      if declaration.is_definition() {
        let function = self.resolve_function(declaration)?;
        declarations.push(function);
      }
    }

    Ok(Program {
      declarations,
      location: program.location,
    })
  }

  fn resolve_function(&mut self, function: &FuncDeclaration) -> Result<FuncDeclaration> {
    self.inside_switch = false;
    let mut cases = CaseMap::default();
    // NOTE: We already ensured the body is present.
    let body = self.resolve_block(&function.body.as_ref().unwrap(), &mut cases)?;

    Ok(FuncDeclaration {
      name: function.name,
      body: Some(body),
      params: function.params.clone(),
      location: function.location,
    })
  }

  fn resolve_block(&mut self, block: &Block, cases: &mut CaseMap) -> Result<Block> {
    let mut body = Vec::with_capacity(block.body.len());

    for block_item in &block.body {
      body.push(self.resolve_block_item(block_item, cases)?);
    }

    Ok(Block {
      body,
      location: block.location,
    })
  }

  fn resolve_block_item(
    &mut self,
    block_item: &BlockItem,
    cases: &mut CaseMap,
  ) -> Result<BlockItem> {
    match block_item {
      | BlockItem::Statement(statement) => {
        self
          .resolve_statement(statement, cases)
          .map(BlockItem::Statement)
      },
      | declaration => Ok(declaration.clone()),
    }
  }

  fn resolve_statement(&mut self, statement: &Statement, cases: &mut CaseMap) -> Result<Statement> {
    match statement {
      | Statement::Case(case) => {
        let key = if let Expression::Constant(Int { value, .. }) = *case.value {
          Some(value)
        } else {
          return Err(SwitchResolutionError::new(
            "non-constant label in case statement",
            *case.value.location(),
          ));
        };

        let (body, case_id) = self.resolve_case(&key, cases, "switch_case", &case.body)?;

        Ok(Statement::Case(Case {
          value: case.value.clone(),
          body: Box::new(body),
          switch_label: Some(case_id),
          location: case.location,
        }))
      },
      | Statement::DefaultCase(default_case) => {
        let (body, case_id) =
          self.resolve_case(&None, cases, "switch_default", &default_case.body)?;

        Ok(Statement::DefaultCase(DefaultCase {
          body: Box::new(body),
          switch_label: Some(case_id),
          location: default_case.location,
        }))
      },
      | Statement::Switch(switch) => {
        self.inside_switch = true;
        let mut cases = CaseMap::default();
        let body = self
          .resolve_statement(&switch.body, &mut cases)
          .map(Box::new)?;

        Ok(Statement::Switch(Switch {
          body,
          cases,
          ..switch.clone()
        }))
      },
      | Statement::If(if_) => {
        let then = self.resolve_statement(&if_.then, cases).map(Box::new)?;

        let otherwise = if_
          .otherwise
          .as_ref()
          .map(|otherwise| self.resolve_statement(otherwise, cases))
          .transpose()?
          .map(Box::new);

        Ok(Statement::If(If {
          then,
          otherwise,
          ..if_.clone()
        }))
      },
      | Statement::Block(block) => {
        let block = self.resolve_block(block, cases)?;

        Ok(Statement::Block(block))
      },
      | Statement::For(for_) => {
        let body = self.resolve_statement(&for_.body, cases).map(Box::new)?;

        Ok(Statement::For(For {
          body,
          ..for_.clone()
        }))
      },
      | Statement::While(while_) => {
        let body = self.resolve_statement(&while_.body, cases).map(Box::new)?;

        Ok(Statement::While(While {
          body,
          ..while_.clone()
        }))
      },
      | Statement::DoWhile(do_while) => {
        let body = self
          .resolve_statement(&do_while.body, cases)
          .map(Box::new)?;

        Ok(Statement::DoWhile(DoWhile {
          body,
          ..do_while.clone()
        }))
      },
      | Statement::Labeled(labeled) => {
        let body = self
          .resolve_statement(&labeled.statement, cases)
          .map(Box::new)?;

        Ok(Statement::Labeled(Labeled {
          statement: body,
          ..labeled.clone()
        }))
      },
      // Other statements don't have bodies and can be left intact.
      | other => Ok(other.clone()),
    }
  }

  fn resolve_case(
    &mut self,
    key: &CaseKey,
    cases: &mut CaseMap,
    case_label: &str,
    statement: &Statement,
  ) -> Result<(Statement, Symbol)> {
    if !self.inside_switch {
      return Err(SwitchResolutionError::new(
        "case statement outside of switch",
        *statement.location(),
      ));
    }

    // Check for duplicates.
    if cases.contains_key(key) {
      return if let Some(key) = key {
        Err(SwitchResolutionError::new(
          format!("duplicate case in switch statement: '{key}'"),
          *statement.location(),
        ))
      } else {
        Err(SwitchResolutionError::new(
          "duplicate default case in switch statement".to_string(),
          *statement.location(),
        ))
      };
    }

    // Generate new id for "case" or "default" label.
    let case_id = self.ctx.gen_label(case_label);
    cases.insert(*key, case_id);

    // Resolve inner statements.
    let resolved = self.resolve_statement(statement, cases)?;

    Ok((resolved, case_id))
  }
}
