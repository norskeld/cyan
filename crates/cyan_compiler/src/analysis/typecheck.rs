use cyan_reporting::Location;
use thiserror::Error;

use crate::context::Context;
use crate::ir::ast::*;
use crate::types::Type;

type Result<T> = std::result::Result<T, TypecheckError>;

#[derive(Debug, Error)]
#[error("typecheck error {location}: {message}")]
pub struct TypecheckError {
  message: String,
  location: Location,
}

impl TypecheckError {
  pub fn new(message: impl AsRef<str> + Into<String>, location: Location) -> Self {
    Self {
      message: message.into(),
      location,
    }
  }
}

pub struct TypecheckPass<'ctx> {
  ctx: &'ctx mut Context,
}

impl<'ctx> TypecheckPass<'ctx> {
  pub fn new(ctx: &'ctx mut Context) -> Self {
    Self { ctx }
  }

  pub fn run(&mut self, program: &Program) -> Result<()> {
    self.typecheck_program(program)
  }

  fn typecheck_program(&mut self, program: &Program) -> Result<()> {
    for declaration in &program.declarations {
      self.typecheck_function(declaration)?;
    }

    Ok(())
  }

  fn typecheck_function(&mut self, func: &FuncDeclaration) -> Result<()> {
    let func_type = Type::Func {
      params_count: func.params.len() as isize,
    };

    let prev_declaration = self.ctx.symtable.get(&func.name);

    if let Some(entry) = prev_declaration {
      if entry.typ != func_type {
        return Err(TypecheckError::new(
          format!("redeclared function {} with a different type", func.name),
          func.location,
        ));
      } else if entry.is_defined && func.is_definition() {
        return Err(TypecheckError::new(
          format!("defined body of function {} twice", func.name),
          func.location,
        ));
      }
    }

    self.ctx.symtable.add_func(
      &func.name,
      func_type,
      prev_declaration.is_some_and(|decl| decl.is_defined) || func.is_definition(),
    );

    if func.is_definition() {
      for param in &func.params {
        self.ctx.symtable.add_var(&param.value, Type::Int);
      }
    }

    if let Some(body) = &func.body {
      self.typecheck_block(body)?;
    }

    Ok(())
  }

  fn typecheck_var(&mut self, var: &VarDeclaration) -> Result<()> {
    self.ctx.symtable.add_var(&var.name, Type::Int);

    if let Some(initializer) = &var.initializer {
      self.typecheck_expression(initializer)?;
    }

    Ok(())
  }

  fn typecheck_block(&mut self, block: &Block) -> Result<()> {
    for block_item in &block.body {
      self.typecheck_block_item(block_item)?;
    }

    Ok(())
  }

  fn typecheck_block_item(&mut self, block_item: &BlockItem) -> Result<()> {
    match block_item {
      | BlockItem::Declaration(declaration) => self.typecheck_declaration(declaration),
      | BlockItem::Statement(statement) => self.typecheck_statement(statement),
    }
  }

  fn typecheck_declaration(&mut self, declaration: &Declaration) -> Result<()> {
    match declaration {
      | Declaration::Func(func_declaration) => self.typecheck_function(func_declaration),
      | Declaration::Var(var_declaration) => self.typecheck_var(var_declaration),
    }
  }

  fn typecheck_statement(&mut self, statement: &Statement) -> Result<()> {
    match statement {
      | Statement::Block(block) => {
        self.typecheck_block(block)?;
      },
      | Statement::Case(case) => {
        self.typecheck_statement(&case.body)?;
      },
      | Statement::DefaultCase(case) => {
        self.typecheck_statement(&case.body)?;
      },
      | Statement::DoWhile(do_while) => {
        self.typecheck_statement(&do_while.body)?;
        self.typecheck_expression(&do_while.condition)?;
      },
      | Statement::Expression(expression) => {
        self.typecheck_expression(expression)?;
      },
      | Statement::For(for_) => {
        match &for_.initializer {
          | Initializer::Declaration(var) => self.typecheck_var(var)?,
          | Initializer::Expression(expression) => self.typecheck_expression(expression)?,
          | Initializer::None { .. } => {},
        }

        if let Some(condiition) = &for_.condition {
          self.typecheck_expression(condiition)?;
        }

        if let Some(postcondition) = &for_.postcondition {
          self.typecheck_expression(postcondition)?;
        }

        self.typecheck_statement(&for_.body)?;
      },
      | Statement::If(if_) => {
        self.typecheck_expression(&if_.condition)?;
        self.typecheck_statement(&if_.then)?;

        if let Some(otherwise) = &if_.otherwise {
          self.typecheck_statement(otherwise)?;
        }
      },
      | Statement::Labeled(labeled) => {
        self.typecheck_statement(&labeled.statement)?;
      },
      | Statement::Return(expression) => {
        self.typecheck_expression(expression)?;
      },
      | Statement::Switch(switch) => {
        self.typecheck_expression(&switch.control)?;
        self.typecheck_statement(&switch.body)?;
      },
      | Statement::While(while_) => {
        self.typecheck_expression(&while_.condition)?;
        self.typecheck_statement(&while_.body)?;
      },
      // No-op.
      | Statement::Null { .. }
      | Statement::Break(..)
      | Statement::Continue(..)
      | Statement::Goto(..) => {},
    }

    Ok(())
  }

  fn typecheck_expression(&mut self, expression: &Expression) -> Result<()> {
    match expression {
      | Expression::Var(ident) => {
        if let Some(entry) = self.ctx.symtable.get(&ident.value) {
          match entry.typ {
            | Type::Int => {},
            | Type::Func { .. } => {
              return Err(TypecheckError::new(
                "tried to use  a function as a variable",
                ident.location,
              ))
            },
          }
        }
      },
      | Expression::Unary(unary) => {
        self.typecheck_expression(&unary.expression)?;
      },
      | Expression::Binary(binary) => {
        self.typecheck_expression(&binary.left)?;
        self.typecheck_expression(&binary.right)?;
      },
      | Expression::Postfix(postfix) => {
        self.typecheck_expression(&postfix.operand)?;
      },
      | Expression::Ternary(ternary) => {
        self.typecheck_expression(&ternary.condition)?;
        self.typecheck_expression(&ternary.then)?;
        self.typecheck_expression(&ternary.otherwise)?;
      },
      | Expression::Assignment(assignment) => {
        self.typecheck_expression(&assignment.left)?;
        self.typecheck_expression(&assignment.right)?;
      },
      | Expression::CompoundAssignment(compound_assignment) => {
        self.typecheck_expression(&compound_assignment.left)?;
        self.typecheck_expression(&compound_assignment.right)?;
      },
      | Expression::FuncCall(func_call) => {
        if let Some(entry) = self.ctx.symtable.get(&func_call.name) {
          match entry.typ {
            | Type::Int => {
              return Err(TypecheckError::new(
                "tried to use a variable as a function name",
                func_call.name.location,
              ));
            },
            | Type::Func { params_count } if params_count != func_call.args.len() as isize => {
              return Err(TypecheckError::new(
                "function called with wrong number of arguments",
                func_call.name.location,
              ));
            },
            | _ => {},
          }
        }
      },
      | _ => {},
    }

    Ok(())
  }
}
