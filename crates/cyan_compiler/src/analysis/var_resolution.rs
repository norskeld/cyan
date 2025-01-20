use std::collections::hash_map::HashMap;

use cyan_reporting::{Located, Location};
use internment::Intern;
use thiserror::Error;

use crate::context::Context;
use crate::ir::ast::*;

type Result<T> = std::result::Result<T, VarResolutionError>;

/// A mapping from the user-defined variable names to the unique names to be used later.
pub type VarMap = HashMap<Intern<String>, VarEntry>;

trait VarMapExt {
  /// Copies the variable map and returns a new one with `local` resetted to `false`.
  fn fresh(&self) -> Self;
}

impl VarMapExt for VarMap {
  fn fresh(&self) -> VarMap {
    self
      .clone()
      .into_iter()
      .map(|(k, v)| (k, VarEntry { local: false, ..v }))
      .collect()
  }
}

/// A variable entry.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VarEntry {
  /// The unique name of the variable.
  pub name: Intern<String>,
  /// Whether the variable is local to the current block scope.
  pub local: bool,
}

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
    let mut variables = VarMap::new();

    // Set the prefix for temporary variables to current function's name.
    self.ctx.var_prefix = function.name.value;

    let body = self.resolve_block(&function.body, &mut variables)?;

    Ok(Function {
      name: function.name,
      body,
      location: function.location,
    })
  }

  fn resolve_block(&mut self, block: &Block, variables: &mut VarMap) -> Result<Block> {
    let mut body = Vec::new();

    for block_item in &block.body {
      body.push(self.resolve_block_item(block_item, variables)?);
    }

    Ok(Block {
      body,
      location: block.location,
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
    match variables.get(&declaration.name.value) {
      | Some(entry) if entry.local => {
        Err(VarResolutionError::new(
          "duplicate variable declaration",
          declaration.name.location,
        ))
      },
      | _ => {
        // Generate unique name and associate a user-defined name with it.
        let unique_name = self.ctx.gen_temporary();

        variables.insert(
          declaration.name.value,
          VarEntry {
            name: unique_name,
            local: true,
          },
        );

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
      },
    }
  }

  fn resolve_statement(
    &mut self,
    statement: &Statement,
    variables: &mut VarMap,
  ) -> Result<Statement> {
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
      | Statement::If(conditional) => {
        let condition = self
          .resolve_expression(&conditional.condition, variables)
          .map(Box::new)?;

        let then = self
          .resolve_statement(&conditional.then, variables)
          .map(Box::new)?;

        let otherwise = conditional
          .otherwise
          .as_ref()
          .map(|otherwise| self.resolve_statement(otherwise, variables))
          .transpose()?
          .map(Box::new);

        Ok(Statement::If(If {
          condition,
          then,
          otherwise,
          location: conditional.location,
        }))
      },
      | Statement::Null { location } => {
        Ok(Statement::Null {
          location: *location,
        })
      },
      | Statement::Block(block) => {
        let mut fresh_variables = variables.fresh();
        let block = self.resolve_block(block, &mut fresh_variables)?;

        Ok(Statement::Block(Block {
          body: block.body,
          location: block.location,
        }))
      },
      | Statement::Goto(goto) => Ok(Statement::Goto(*goto)),
      | Statement::Labeled(labeled) => {
        let statement = self
          .resolve_statement(&labeled.statement, variables)
          .map(Box::new)?;

        Ok(Statement::Labeled(Labeled {
          statement,
          label: labeled.label,
          location: labeled.location,
        }))
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
        if let Some(entry) = variables.get(&ident.value) {
          Ok(Expression::Var(Ident {
            value: entry.name,
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
        if let UnaryOp::Dec | UnaryOp::Inc = unary.op {
          return Err(VarResolutionError::new(
            "operand of ++/-- must be a variable",
            unary.location,
          ));
        }

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
            "left-hand expression of assignment must be a modifiable lvalue",
            *assignment.left.location(),
          ))
        }
      },
      | Expression::CompoundAssignment(assignment) => {
        if let Expression::Var(..) = &*assignment.left {
          let left = self.resolve_expression(&assignment.left, variables)?;
          let right = self.resolve_expression(&assignment.right, variables)?;

          Ok(Expression::CompoundAssignment(CompoundAssignment {
            op: assignment.op,
            left: Box::new(left),
            right: Box::new(right),
            location: assignment.location,
          }))
        } else {
          Err(VarResolutionError::new(
            "left-hand expression of compound assignment must be a modifiable lvalue",
            *assignment.left.location(),
          ))
        }
      },
      | Expression::Postfix(postfix) => {
        if let Expression::Var(..) = &*postfix.operand {
          let operand = self.resolve_expression(&postfix.operand, variables)?;

          Ok(Expression::Postfix(Postfix {
            op: postfix.op,
            operand: Box::new(operand),
            location: postfix.location,
          }))
        } else {
          Err(VarResolutionError::new(
            "operand of postfix expression must be a modifiable lvalue",
            *postfix.operand.location(),
          ))
        }
      },
      | Expression::Ternary(ternary) => {
        let condition = self.resolve_expression(&ternary.condition, variables)?;
        let then = self.resolve_expression(&ternary.then, variables)?;
        let otherwise = self.resolve_expression(&ternary.otherwise, variables)?;

        Ok(Expression::Ternary(Ternary {
          condition: Box::new(condition),
          then: Box::new(then),
          otherwise: Box::new(otherwise),
          location: ternary.location,
        }))
      },
    }
  }
}
