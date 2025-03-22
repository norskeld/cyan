use cyan_reporting::Location;
use thiserror::Error;

use crate::context::Context;
use crate::ir::ast::*;

type Result<T> = std::result::Result<T, LoopLabelingError>;

#[derive(Debug, Error)]
#[error("loop labeling error {location}: {message}")]
pub struct LoopLabelingError {
  /// The error message.
  message: String,
  /// The location of the error.
  location: Location,
}

impl LoopLabelingError {
  pub fn new(message: impl AsRef<str> + Into<String>, location: Location) -> Self {
    Self {
      message: message.into(),
      location,
    }
  }
}

pub struct LoopLabelingPass<'ctx> {
  ctx: &'ctx mut Context,
}

impl<'ctx> LoopLabelingPass<'ctx> {
  pub fn new(ctx: &'ctx mut Context) -> Self {
    Self { ctx }
  }

  pub fn run(&mut self, program: &Program) -> Result<Program> {
    self.label_program(program)
  }

  fn label_program(&mut self, program: &Program) -> Result<Program> {
    let function = self.label_function(&program.function, (&None, &None))?;

    Ok(Program {
      function,
      location: program.location,
    })
  }

  fn label_function(
    &mut self,
    function: &Function,
    current_labels: (&Option<LoopLabel>, &Option<LoopLabel>),
  ) -> Result<Function> {
    let body = self.label_block(&function.body, current_labels)?;

    Ok(Function {
      name: function.name,
      body,
      location: function.location,
    })
  }

  fn label_block(
    &mut self,
    block: &Block,
    current_labels: (&Option<LoopLabel>, &Option<LoopLabel>),
  ) -> Result<Block> {
    let mut body = Vec::with_capacity(block.body.len());

    for item in &block.body {
      body.push(self.label_block_item(item, current_labels)?);
    }

    Ok(Block {
      body,
      location: block.location,
    })
  }

  fn label_block_item(
    &mut self,
    block_item: &BlockItem,
    current_labels: (&Option<LoopLabel>, &Option<LoopLabel>),
  ) -> Result<BlockItem> {
    match block_item {
      | BlockItem::Statement(statement) => {
        self
          .label_statement(statement, current_labels)
          .map(BlockItem::Statement)
      },
      | declaration => Ok(declaration.clone()),
    }
  }

  fn label_statement(
    &mut self,
    statement: &Statement,
    (current_break_label, current_continue_label): (&Option<LoopLabel>, &Option<LoopLabel>),
  ) -> Result<Statement> {
    match statement {
      | Statement::Break(break_) => {
        current_break_label
          .map(|it| {
            Statement::Break(Break {
              loop_label: Some(it),
              location: break_.location,
            })
          })
          .ok_or_else(|| LoopLabelingError::new("break outside of loop", break_.location))
      },
      | Statement::Continue(continue_) => {
        current_continue_label
          .map(|it| {
            Statement::Continue(Continue {
              loop_label: Some(it),
              location: continue_.location,
            })
          })
          .ok_or_else(|| LoopLabelingError::new("continue outside of loop", continue_.location))
      },
      | Statement::While(while_) => {
        let new_label = Some(self.ctx.gen_label("while"));

        let body = self
          .label_statement(&while_.body, (&new_label, &new_label))
          .map(Box::new)?;

        Ok(Statement::While(While {
          body,
          loop_label: new_label,
          ..while_.clone()
        }))
      },
      | Statement::DoWhile(do_while) => {
        let new_label = Some(self.ctx.gen_label("do_while"));

        let body = self
          .label_statement(&do_while.body, (&new_label, &new_label))
          .map(Box::new)?;

        Ok(Statement::DoWhile(DoWhile {
          body,
          loop_label: new_label,
          ..do_while.clone()
        }))
      },
      | Statement::For(for_) => {
        let new_label = Some(self.ctx.gen_label("for"));

        let body = self
          .label_statement(&for_.body, (&new_label, &new_label))
          .map(Box::new)?;

        Ok(Statement::For(For {
          body,
          loop_label: new_label,
          ..for_.clone()
        }))
      },
      | Statement::Block(block) => {
        let body = self.label_block(block, (current_break_label, current_continue_label))?;

        Ok(Statement::Block(body))
      },
      | Statement::If(if_) => {
        let then = self
          .label_statement(&if_.then, (current_break_label, current_continue_label))
          .map(Box::new)?;

        let otherwise = if_
          .otherwise
          .as_ref()
          .map(|otherwise| {
            self.label_statement(otherwise, (current_break_label, current_continue_label))
          })
          .transpose()?
          .map(Box::new);

        Ok(Statement::If(If {
          then,
          otherwise,
          ..if_.clone()
        }))
      },
      | Statement::Labeled(labeled) => {
        let statement = self
          .label_statement(
            &labeled.statement,
            (current_break_label, current_continue_label),
          )
          .map(Box::new)?;

        Ok(Statement::Labeled(Labeled {
          statement,
          ..labeled.clone()
        }))
      },
      | Statement::Switch(switch) => {
        let new_break_label = Some(self.ctx.gen_label("switch"));

        let body = self
          .label_statement(&switch.body, (&new_break_label, current_continue_label))
          .map(Box::new)?;

        Ok(Statement::Switch(Switch {
          body,
          switch_label: new_break_label,
          ..switch.clone()
        }))
      },
      | Statement::Case(case) => {
        let body = self
          .label_statement(&case.body, (current_break_label, current_continue_label))
          .map(Box::new)?;

        Ok(Statement::Case(Case {
          body,
          ..case.clone()
        }))
      },
      | Statement::DefaultCase(default_case) => {
        let body = self
          .label_statement(
            &default_case.body,
            (current_break_label, current_continue_label),
          )
          .map(Box::new)?;

        Ok(Statement::DefaultCase(DefaultCase {
          body,
          ..default_case.clone()
        }))
      },
      | Statement::Goto(goto) => Ok(Statement::Goto(*goto)),
      | Statement::Return(expression) => Ok(Statement::Return(expression.clone())),
      | Statement::Expression(expression) => Ok(Statement::Expression(expression.clone())),
      | Statement::Null { location } => {
        Ok(Statement::Null {
          location: *location,
        })
      },
    }
  }
}
