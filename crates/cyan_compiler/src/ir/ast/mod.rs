//! AST definition.

use std::fmt;
use std::hash::{Hash, Hasher};

use cyan_reporting::{Located, Location};
use internment::Intern;

#[derive(Debug, Located, PartialEq, Eq)]
pub struct Program {
  pub function: Function,
  pub location: Location,
}

#[derive(Debug, Located, PartialEq, Eq)]
pub struct Function {
  pub name: Ident,
  pub body: Block,
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
pub struct Declaration {
  pub name: Ident,
  pub initializer: Option<Expression>,
  pub location: Location,
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
  Null { location: Location },
}

#[derive(Clone, Copy, Debug, Located, PartialEq, Eq)]
pub struct Break {
  pub label: Option<Ident>,
  pub location: Location,
}

#[derive(Clone, Copy, Debug, Located, PartialEq, Eq)]
pub struct Continue {
  pub label: Option<Ident>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct For {
  pub initializer: Initializer,
  pub condition: Option<Expression>,
  pub postcondition: Option<Expression>,
  pub body: Box<Statement>,
  pub label: Option<Ident>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub enum Initializer {
  Declaration(Declaration),
  Expression(Expression),
  None { location: Location },
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct While {
  pub condition: Expression,
  pub body: Box<Statement>,
  pub label: Option<Ident>,
  pub location: Location,
}

#[derive(Clone, Debug, Located, PartialEq, Eq)]
pub struct DoWhile {
  pub condition: Expression,
  pub body: Box<Statement>,
  pub label: Option<Ident>,
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
  pub value: Intern<String>,
  pub location: Location,
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
