use std::collections::HashMap;

use cyan_reporting::{Located, Location};
use thiserror::Error;

use crate::context::Context;
use crate::ir::ast::*;
use crate::symbol::Symbol;

type Result<T> = std::result::Result<T, IdResolutionError>;

/// A mapping from identifiers to the unique ones to be used later.
pub type IdMap = HashMap<Symbol, IdEntry>;

trait IdMapExt {
  /// Copies the identifier map and returns a new one with `local` resetted to `false`.
  fn fresh(&self) -> Self;
}

impl IdMapExt for IdMap {
  fn fresh(&self) -> IdMap {
    self
      .clone()
      .into_iter()
      .map(|(k, v)| {
        (
          k,
          IdEntry {
            current_scope: false,
            ..v
          },
        )
      })
      .collect()
  }
}

/// An identifier.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IdEntry {
  /// The unique name of the identifier.
  pub name: Symbol,
  /// Whether the identifier is local to the current block scope.
  pub current_scope: bool,
  /// Whether the identifier has linkage associated with it.
  pub linkage: bool,
}

impl IdEntry {
  /// Creates an identifier entry for a local variable.
  pub fn for_local_var(name: Symbol) -> Self {
    Self {
      name,
      current_scope: true,
      linkage: false,
    }
  }

  /// Creates an identifier entry for a declaration. It has linkage associated with it.
  pub fn for_declaration(name: Symbol) -> Self {
    Self {
      name,
      current_scope: true,
      linkage: true,
    }
  }
}

#[derive(Debug, Error)]
#[error("identifier resolution error {location}: {message}")]
pub struct IdResolutionError {
  /// The error message.
  message: String,
  /// The location of the error.
  location: Location,
}

impl IdResolutionError {
  pub fn new(message: impl AsRef<str> + Into<String>, location: Location) -> Self {
    Self {
      message: message.into(),
      location,
    }
  }
}

pub struct IdResolutionPass<'ctx> {
  ctx: &'ctx mut Context,
}

impl<'ctx> IdResolutionPass<'ctx> {
  pub fn new(ctx: &'ctx mut Context) -> Self {
    Self { ctx }
  }

  pub fn run(&mut self, program: &Program) -> Result<Program> {
    self.resolve_program(program)
  }

  fn resolve_program(&mut self, program: &Program) -> Result<Program> {
    let mut id_map = IdMap::new();
    let mut declarations = vec![];

    for declaration in &program.declarations {
      let declaration = self.resolve_func_declaration(declaration, &mut id_map)?;
      declarations.push(declaration);
    }

    Ok(Program {
      declarations,
      location: program.location,
    })
  }

  fn resolve_func_declaration(
    &mut self,
    function: &FuncDeclaration,
    id_map: &mut IdMap,
  ) -> Result<FuncDeclaration> {
    match id_map.get(&function.name) {
      | Some(entry) if entry.current_scope && !entry.linkage => {
        Err(IdResolutionError::new(
          "duplicate function declaration",
          function.location,
        ))
      },
      | _ => {
        // Set the prefix for temporary variables to current function's name.
        self.ctx.var_prefix = *function.name;

        // Add current function to the identifiers map.
        id_map.insert(*function.name, IdEntry::for_declaration(*function.name));

        let mut inner_map = id_map.fresh();

        // Resolve function parameters.
        let params = self.resolve_params(&function.params, &mut inner_map)?;

        // Resolve function body.
        let body = if let Some(body) = function.body.as_ref() {
          self.resolve_block(body, &mut inner_map).map(Some)?
        } else {
          None
        };

        Ok(FuncDeclaration {
          name: function.name,
          params,
          body,
          location: function.location,
        })
      },
    }
  }

  fn resolve_local_declaration(
    &mut self,
    declaration: &Declaration,
    id_map: &mut IdMap,
  ) -> Result<Declaration> {
    match declaration {
      | Declaration::Var(var) => {
        let resolved = self.resolve_local_var_declaration(var, id_map)?;

        Ok(Declaration::Var(resolved))
      },
      | Declaration::Func(func) if func.is_definition() => {
        Err(IdResolutionError::new(
          "nested function definitions are not allowed",
          func.name.location,
        ))
      },
      | Declaration::Func(func) => {
        let resolved = self.resolve_func_declaration(func, id_map)?;

        Ok(Declaration::Func(resolved))
      },
    }
  }

  fn resolve_params(&mut self, params: &[Ident], id_map: &mut IdMap) -> Result<Vec<Ident>> {
    let mut resolved_params = vec![];

    for param in params {
      let param = self.resolve_local_var(param, id_map)?;
      resolved_params.push(param);
    }

    Ok(resolved_params)
  }

  fn resolve_block(&mut self, block: &Block, id_map: &mut IdMap) -> Result<Block> {
    let mut body = Vec::new();

    for block_item in &block.body {
      let block_item = self.resolve_block_item(block_item, id_map)?;
      body.push(block_item);
    }

    Ok(Block {
      body,
      location: block.location,
    })
  }

  fn resolve_block_item(
    &mut self,
    block_item: &BlockItem,
    id_map: &mut IdMap,
  ) -> Result<BlockItem> {
    match block_item {
      | BlockItem::Declaration(declaration) => {
        self
          .resolve_local_declaration(declaration, id_map)
          .map(BlockItem::Declaration)
      },
      | BlockItem::Statement(statement) => {
        self
          .resolve_statement(statement, id_map)
          .map(BlockItem::Statement)
      },
    }
  }

  fn resolve_local_var_declaration(
    &mut self,
    declaration: &VarDeclaration,
    id_map: &mut IdMap,
  ) -> Result<VarDeclaration> {
    let name = self.resolve_local_var(&declaration.name, id_map)?;
    let initializer = self.resolve_optional_expression(&declaration.initializer, id_map)?;

    Ok(VarDeclaration {
      name,
      initializer,
      location: declaration.location,
    })
  }

  fn resolve_local_var(&mut self, ident: &Ident, id_map: &mut IdMap) -> Result<Ident> {
    match id_map.get(&ident.value) {
      | Some(entry) if entry.current_scope => {
        Err(IdResolutionError::new(
          "duplicate variable declaration",
          ident.location,
        ))
      },
      | _ => {
        // Generate unique name and associate a user-defined name with it.
        let unique_name = self.ctx.gen_temporary();

        // Add the variable identifier to the map.
        id_map.insert(ident.value, IdEntry::for_local_var(unique_name));

        Ok(Ident {
          value: unique_name,
          location: ident.location,
        })
      },
    }
  }

  fn resolve_statement(&mut self, statement: &Statement, id_map: &mut IdMap) -> Result<Statement> {
    match statement {
      | Statement::Return(expression) => {
        self
          .resolve_expression(expression, id_map)
          .map(Statement::Return)
      },
      | Statement::Expression(expression) => {
        self
          .resolve_expression(expression, id_map)
          .map(Statement::Expression)
      },
      | Statement::If(conditional) => {
        let condition = self
          .resolve_expression(&conditional.condition, id_map)
          .map(Box::new)?;

        let then = self
          .resolve_statement(&conditional.then, id_map)
          .map(Box::new)?;

        let otherwise = conditional
          .otherwise
          .as_ref()
          .map(|otherwise| self.resolve_statement(otherwise, id_map))
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
        let mut local_ids = id_map.fresh();
        let block = self.resolve_block(block, &mut local_ids)?;

        Ok(Statement::Block(Block {
          body: block.body,
          location: block.location,
        }))
      },
      | Statement::Goto(goto) => Ok(Statement::Goto(*goto)),
      | Statement::Labeled(labeled) => {
        let statement = self
          .resolve_statement(&labeled.statement, id_map)
          .map(Box::new)?;

        Ok(Statement::Labeled(Labeled {
          statement,
          label: labeled.label,
          location: labeled.location,
        }))
      },
      | Statement::For(for_) => {
        let mut local_vars = id_map.fresh();
        let initializer = self.resolve_initializer(&for_.initializer, &mut local_vars)?;
        let condition = self.resolve_optional_expression(&for_.condition, &mut local_vars)?;

        let postcondition =
          self.resolve_optional_expression(&for_.postcondition, &mut local_vars)?;

        let body = self
          .resolve_statement(&for_.body, &mut local_vars)
          .map(Box::new)?;

        Ok(Statement::For(For {
          initializer,
          condition,
          postcondition,
          body,
          loop_label: for_.loop_label,
          location: for_.location,
        }))
      },
      | Statement::While(while_) => {
        let condition = self.resolve_expression(&while_.condition, id_map)?;
        let body = self.resolve_statement(&while_.body, id_map).map(Box::new)?;

        Ok(Statement::While(While {
          condition,
          body,
          loop_label: while_.loop_label,
          location: while_.location,
        }))
      },
      | Statement::DoWhile(dowhile) => {
        let body = self
          .resolve_statement(&dowhile.body, id_map)
          .map(Box::new)?;

        let condition = self.resolve_expression(&dowhile.condition, id_map)?;

        Ok(Statement::DoWhile(DoWhile {
          condition,
          body,
          loop_label: dowhile.loop_label,
          location: dowhile.location,
        }))
      },
      | Statement::Switch(switch) => {
        let control = self
          .resolve_expression(&switch.control, id_map)
          .map(Box::new)?;

        let body = self.resolve_statement(&switch.body, id_map).map(Box::new)?;

        Ok(Statement::Switch(Switch {
          control,
          body,
          cases: switch.cases.clone(),
          switch_label: switch.switch_label,
          location: switch.location,
        }))
      },
      | Statement::Case(case) => {
        let body = self.resolve_statement(&case.body, id_map).map(Box::new)?;

        Ok(Statement::Case(Case {
          body,
          value: case.value.clone(),
          switch_label: case.switch_label,
          location: case.location,
        }))
      },
      | Statement::DefaultCase(default_case) => {
        let body = self
          .resolve_statement(&default_case.body, id_map)
          .map(Box::new)?;

        Ok(Statement::DefaultCase(DefaultCase {
          body,
          switch_label: default_case.switch_label,
          location: default_case.location,
        }))
      },
      | Statement::Break(break_) => Ok(Statement::Break(*break_)),
      | Statement::Continue(continue_) => Ok(Statement::Continue(*continue_)),
    }
  }

  fn resolve_initializer(
    &mut self,
    initializer: &Initializer,
    id_map: &mut IdMap,
  ) -> Result<Initializer> {
    match initializer {
      | Initializer::Declaration(declaration) => {
        let resolved_declaration = self.resolve_local_var_declaration(declaration, id_map)?;

        Ok(Initializer::Declaration(resolved_declaration))
      },
      | Initializer::Expression(expression) => {
        let resolved_expression = self.resolve_expression(expression, id_map)?;

        Ok(Initializer::Expression(resolved_expression))
      },
      | Initializer::None { location } => {
        Ok(Initializer::None {
          location: *location,
        })
      },
    }
  }

  fn resolve_optional_expression(
    &self,
    expression: &Option<Expression>,
    id_map: &mut IdMap,
  ) -> Result<Option<Expression>> {
    expression
      .as_ref()
      .map(|it| self.resolve_expression(it, id_map))
      .transpose()
  }

  #[allow(clippy::only_used_in_recursion)]
  fn resolve_expression(&self, expression: &Expression, id_map: &mut IdMap) -> Result<Expression> {
    match expression {
      | Expression::Constant(int) => Ok(Expression::Constant(*int)),
      | Expression::Var(ident) => {
        if let Some(entry) = id_map.get(&ident.value) {
          Ok(Expression::Var(Ident {
            value: entry.name,
            location: ident.location,
          }))
        } else {
          Err(IdResolutionError::new(
            format!("variable '{}' is undefined", ident.value),
            ident.location,
          ))
        }
      },
      | Expression::Unary(unary) => {
        if let UnaryOp::Dec | UnaryOp::Inc = unary.op {
          return Err(IdResolutionError::new(
            "operand of ++/-- must be a variable",
            unary.location,
          ));
        }

        let expression = self.resolve_expression(&unary.expression, id_map)?;

        Ok(Expression::Unary(Unary {
          op: unary.op,
          expression: Box::new(expression),
          location: unary.location,
        }))
      },
      | Expression::Binary(binary) => {
        let left = self.resolve_expression(&binary.left, id_map)?;
        let right = self.resolve_expression(&binary.right, id_map)?;

        Ok(Expression::Binary(Binary {
          op: binary.op,
          left: Box::new(left),
          right: Box::new(right),
          location: binary.location,
        }))
      },
      | Expression::Assignment(assignment) => {
        if let Expression::Var(..) = &*assignment.left {
          let left = self.resolve_expression(&assignment.left, id_map)?;
          let right = self.resolve_expression(&assignment.right, id_map)?;

          Ok(Expression::Assignment(Assignment {
            left: Box::new(left),
            right: Box::new(right),
            location: assignment.location,
          }))
        } else {
          Err(IdResolutionError::new(
            "left-hand expression of assignment must be a modifiable lvalue",
            *assignment.left.location(),
          ))
        }
      },
      | Expression::CompoundAssignment(assignment) => {
        if let Expression::Var(..) = &*assignment.left {
          let left = self.resolve_expression(&assignment.left, id_map)?;
          let right = self.resolve_expression(&assignment.right, id_map)?;

          Ok(Expression::CompoundAssignment(CompoundAssignment {
            op: assignment.op,
            left: Box::new(left),
            right: Box::new(right),
            location: assignment.location,
          }))
        } else {
          Err(IdResolutionError::new(
            "left-hand expression of compound assignment must be a modifiable lvalue",
            *assignment.left.location(),
          ))
        }
      },
      | Expression::Postfix(postfix) => {
        if let Expression::Var(..) = &*postfix.operand {
          let operand = self.resolve_expression(&postfix.operand, id_map)?;

          Ok(Expression::Postfix(Postfix {
            op: postfix.op,
            operand: Box::new(operand),
            location: postfix.location,
          }))
        } else {
          Err(IdResolutionError::new(
            "operand of postfix expression must be a modifiable lvalue",
            *postfix.operand.location(),
          ))
        }
      },
      | Expression::Ternary(ternary) => {
        let condition = self.resolve_expression(&ternary.condition, id_map)?;
        let then = self.resolve_expression(&ternary.then, id_map)?;
        let otherwise = self.resolve_expression(&ternary.otherwise, id_map)?;

        Ok(Expression::Ternary(Ternary {
          condition: Box::new(condition),
          then: Box::new(then),
          otherwise: Box::new(otherwise),
          location: ternary.location,
        }))
      },
      | Expression::FuncCall(func_call) => {
        if let Some(entry) = id_map.get(&*func_call.name).cloned() {
          let mut args = vec![];

          for arg in &func_call.args {
            let arg = self.resolve_expression(arg, id_map)?;
            args.push(arg);
          }

          Ok(Expression::FuncCall(FuncCall {
            name: Ident {
              value: entry.name,
              location: func_call.name.location,
            },
            args,
            location: func_call.location,
          }))
        } else {
          Err(IdResolutionError::new(
            format!("function '{}' is undefined", *func_call.name),
            func_call.name.location,
          ))
        }
      },
    }
  }
}
