//! AST definition.

use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;

use cyan_reporting::{Located, Location};

use crate::symbol::Symbol;

pub type CaseKey = Option<isize>;
pub type CaseMap = HashMap<CaseKey, Symbol>;

#[derive(Debug, Located, PartialEq, Eq)]
pub struct Program {
  pub declarations: Vec<FuncDeclaration>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct Block {
  pub body: Vec<BlockItem>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub enum BlockItem {
  Declaration(Declaration),
  Statement(Statement),
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub enum Declaration {
  Func(FuncDeclaration),
  Var(VarDeclaration),
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct VarDeclaration {
  pub name: Ident,
  pub initializer: Option<Expression>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct FuncDeclaration {
  pub name: Ident,
  pub params: Vec<Ident>,
  /// If body is Some, then it's a definition. Otherwise it's a declaration.
  pub body: Option<Block>,
  pub location: Location,
}

impl FuncDeclaration {
  /// Whether this node is a declaration.
  pub fn is_declaration(&self) -> bool {
    self.body.is_none()
  }

  /// Whether this node is a definition.
  pub fn is_definition(&self) -> bool {
    self.body.is_some()
  }
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub enum Statement {
  Goto(Goto),
  Labeled(Labeled),
  Return(Expression),
  Expression(Expression),
  If(If),
  Block(Block),
  For(For),
  While(While),
  DoWhile(DoWhile),
  Break(Break),
  Continue(Continue),
  Switch(Switch),
  Case(Case),
  DefaultCase(DefaultCase),
  Null { location: Location },
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct Switch {
  pub control: Box<Expression>,
  pub body: Box<Statement>,
  pub cases: CaseMap,
  pub switch_label: Option<Symbol>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct Case {
  pub value: Box<Expression>,
  pub body: Box<Statement>,
  pub switch_label: Option<Symbol>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct DefaultCase {
  pub body: Box<Statement>,
  pub switch_label: Option<Symbol>,
  pub location: Location,
}

#[derive(Clone, Copy, Debug, Located, PartialEq, Eq)]
pub struct Break {
  pub loop_label: Option<Symbol>,
  pub location: Location,
}

#[derive(Clone, Copy, Debug, Located, PartialEq, Eq)]
pub struct Continue {
  pub loop_label: Option<Symbol>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct For {
  pub initializer: Initializer,
  pub condition: Option<Expression>,
  pub postcondition: Option<Expression>,
  pub body: Box<Statement>,
  pub loop_label: Option<Symbol>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub enum Initializer {
  Declaration(VarDeclaration),
  Expression(Expression),
  None { location: Location },
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct While {
  pub condition: Expression,
  pub body: Box<Statement>,
  pub loop_label: Option<Symbol>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct DoWhile {
  pub condition: Expression,
  pub body: Box<Statement>,
  pub loop_label: Option<Symbol>,
  pub location: Location,
}

#[derive(Clone, Copy, Debug, Located, PartialEq, Eq)]
pub struct Goto {
  pub label: Ident,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct Labeled {
  pub label: Ident,
  pub statement: Box<Statement>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub enum Expression {
  Constant(Int),
  Var(Ident),
  Unary(Unary),
  Binary(Binary),
  Postfix(Postfix),
  Ternary(Ternary),
  Assignment(Assignment),
  CompoundAssignment(CompoundAssignment),
  FuncCall(FuncCall),
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct Unary {
  pub op: UnaryOp,
  pub expression: Box<Expression>,
  pub location: Location,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
  // Arithmetic operators.
  Dec,
  Inc,
  Negate,
  // Bitwise operators.
  BitNot,
  // Logical operators.
  Not,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct Binary {
  pub op: BinaryOp,
  pub left: Box<Expression>,
  pub right: Box<Expression>,
  pub location: Location,
}

impl Binary {
  pub fn is_and(&self) -> bool {
    matches!(self.op, BinaryOp::And)
  }

  pub fn is_or(&self) -> bool {
    matches!(self.op, BinaryOp::Or)
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
  // Arithmetic operators.
  Add,
  Div,
  Mod,
  Mul,
  Sub,
  // Bitwise operators.
  BitAnd,
  BitOr,
  BitShl,
  BitShr,
  BitXor,
  // Logical and relational operators.
  And,
  Equal,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  NotEqual,
  Or,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct Assignment {
  pub left: Box<Expression>,
  pub right: Box<Expression>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct CompoundAssignment {
  pub op: BinaryOp,
  pub left: Box<Expression>,
  pub right: Box<Expression>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct FuncCall {
  pub name: Ident,
  pub args: Vec<Expression>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct Postfix {
  pub op: PostfixOp,
  pub operand: Box<Expression>,
  pub location: Location,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PostfixOp {
  Inc,
  Dec,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct If {
  pub condition: Box<Expression>,
  pub then: Box<Statement>,
  pub otherwise: Option<Box<Statement>>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct Ternary {
  pub condition: Box<Expression>,
  pub then: Box<Expression>,
  pub otherwise: Box<Expression>,
  pub location: Location,
}

#[derive(Clone, Copy, Debug, Located, PartialEq, Eq)]
pub struct Int {
  pub value: isize,
  pub location: Location,
}

#[derive(Clone, Copy, Debug, Located, Eq)]
pub struct Ident {
  pub value: Symbol,
  pub location: Location,
}

impl Deref for Ident {
  type Target = Symbol;

  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl PartialEq<Ident> for Ident {
  fn eq(&self, other: &Ident) -> bool {
    self.value == other.value
  }
}

impl Hash for Ident {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.value.hash(state);
  }
}

impl fmt::Display for Ident {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.value)
  }
}
