use std::collections::hash_map::{Entry, HashMap};

use cyan_reporting::{Located, Location};
use internment::Intern;
use thiserror::Error;

use crate::context::Context;
use crate::ir::ast::*;

pub type Result<T> = std::result::Result<T, VarResolutionError>;

/// A mapping from the user-defined variable names to the unique names to be used later.
pub type VarMap = HashMap<Intern<String>, Intern<String>>;

#[derive(Debug, Error)]
#[error("variable resolution error {location}: {message}")]
pub struct VarResolutionError {
  /// The error message.
  message: String,
  /// The location of the error.
  location: Location,
}

impl VarResolutionError {
  pub fn new(message: impl AsRef<str> + Into<String>, location: Location) -> Self {
    Self {
      message: message.into(),
      location,
    }
  }
}

pub struct VarResolutionPass<'ctx> {
  ctx: &'ctx mut Context,
}

impl<'ctx> VarResolutionPass<'ctx> {
  pub fn new(ctx: &'ctx mut Context) -> Self {
    Self { ctx }
  }

  pub fn run(&mut self, program: &Program) -> Result<Program> {
    self.resolve_program(program)
  }

  fn resolve_program(&mut self, program: &Program) -> Result<Program> {
    let function = self.resolve_function(&program.function)?;

    Ok(Program {
      function,
      location: program.location,
    })
  }

  fn resolve_function(&mut self, function: &Function) -> Result<Function> {
    let mut variables: VarMap = HashMap::new();
    let mut body = Vec::new();

    // Set the prefix for temporary variables to current function's name.
    self.ctx.var_prefix = function.name.value;

    for block_item in &function.body {
      body.push(self.resolve_block_item(block_item, &mut variables)?);
    }

    Ok(Function {
      name: function.name,
      body,
      location: function.location,
    })
  }

  fn resolve_block_item(
    &mut self,
    block_item: &BlockItem,
    variables: &mut VarMap,
  ) -> Result<BlockItem> {
    match block_item {
      | BlockItem::Declaration(declaration) => {
        self
          .resolve_declaration(declaration, variables)
          .map(BlockItem::Declaration)
      },
      | BlockItem::Statement(statement) => {
        self
          .resolve_statement(statement, variables)
          .map(BlockItem::Statement)
      },
    }
  }

  fn resolve_declaration(
    &mut self,
    declaration: &Declaration,
    variables: &mut VarMap,
  ) -> Result<Declaration> {
    if let Entry::Vacant(entry) = variables.entry(declaration.name.value) {
      // Generate unique name and associate a user-defined name with it.
      let unique_name = self.ctx.gen_label(&declaration.name.value);
      entry.insert(unique_name);

      // Resolve initializer if there is one.
      let resolved_init = declaration
        .initializer
        .as_ref()
        .map(|init| self.resolve_expression(init, variables))
        .transpose()?;

      Ok(Declaration {
        name: Ident {
          value: unique_name,
          location: declaration.name.location, // FIXME: This is probably incorrect.
        },
        initializer: resolved_init,
        location: declaration.location,
      })
    } else {
      Err(VarResolutionError::new(
        "duplicate variable declaration",
        declaration.name.location,
      ))
    }
  }

  fn resolve_statement(&self, statement: &Statement, variables: &mut VarMap) -> Result<Statement> {
    match statement {
      | Statement::Return(expression) => {
        self
          .resolve_expression(expression, variables)
          .map(Statement::Return)
      },
      | Statement::Expression(expression) => {
        self
          .resolve_expression(expression, variables)
          .map(Statement::Expression)
      },
      | Statement::Null { location } => {
        Ok(Statement::Null {
          location: *location,
        })
      },
    }
  }

  #[allow(clippy::only_used_in_recursion)]
  fn resolve_expression(
    &self,
    expression: &Expression,
    variables: &mut VarMap,
  ) -> Result<Expression> {
    match expression {
      | Expression::Constant(int) => Ok(Expression::Constant(*int)),
      | Expression::Var(ident) => {
        if let Some(name) = variables.get(&ident.value) {
          Ok(Expression::Var(Ident {
            value: *name,
            location: ident.location,
          }))
        } else {
          Err(VarResolutionError::new(
            format!("variable '{}' is undefined", ident.value),
            ident.location,
          ))
        }
      },
      | Expression::Unary(unary) => {
        let expression = self.resolve_expression(&unary.expression, variables)?;

        Ok(Expression::Unary(Unary {
          op: unary.op,
          expression: Box::new(expression),
          location: unary.location,
        }))
      },
      | Expression::Binary(binary) => {
        let left = self.resolve_expression(&binary.left, variables)?;
        let right = self.resolve_expression(&binary.right, variables)?;

        Ok(Expression::Binary(Binary {
          op: binary.op,
          left: Box::new(left),
          right: Box::new(right),
          location: binary.location,
        }))
      },
      | Expression::Assignment(assignment) => {
        if let Expression::Var(..) = &*assignment.left {
          let left = self.resolve_expression(&assignment.left, variables)?;
          let right = self.resolve_expression(&assignment.right, variables)?;

          Ok(Expression::Assignment(Assignment {
            left: Box::new(left),
            right: Box::new(right),
            location: assignment.location,
          }))
        } else {
          Err(VarResolutionError::new(
            "expression must be a modifiable lvalue",
            *assignment.left.location(),
          ))
        }
      },
      | _ => todo!("resolve expression"),
    }
  }
}
