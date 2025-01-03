//! AST definition.

use internment::Intern;

/// A helper macro that adds a `location` field to an AST node and implements [Located] for it.
macro_rules! located {
  // Handle structs.
  (
    $(#[$outer:meta])*
    $vis:vis struct $name:ident {
      $(
        $(#[$inner:meta])*
        $field_vis:vis $field:ident : $ty:ty
      ),* $(,)?
    }
  ) => {
    $(#[$outer])*
    $vis struct $name {
      $(
        $(#[$inner])*
        $field_vis $field : $ty,
      )*
      /// Contains the node's location.
      pub location: cyan_reporting::Location,
    }

    impl cyan_reporting::Located for $name {
      fn location(&self) -> &cyan_reporting::Location {
        &self.location
      }
    }
  };

  // Handle enums.
  (
    $(#[$outer:meta])*
    $vis:vis enum $name:ident {
      $(
        $(#[$inner:meta])*
        $variant:ident($ty:ty)
      ),* $(,)?
    }
  ) => {
    $(#[$outer])*
    $vis enum $name {
      $(
        $(#[$inner])*
        $variant($ty)
      ),*
    }

    impl cyan_reporting::Located for $name {
      fn location(&self) -> &cyan_reporting::Location {
        match self {
          $($name::$variant(expr) => expr.location(),)*
        }
      }
    }
  };
}

located! {
  #[derive(Debug, PartialEq, Eq)]
  pub struct Program {
    pub function: Function,
  }
}

located! {
  #[derive(Debug, PartialEq, Eq)]
  pub struct Function {
    pub name: Ident,
    pub body: Statement,
  }
}

located! {
  #[derive(Debug, PartialEq, Eq)]
  pub enum Statement {
    Return(Expression),
  }
}

located! {
  #[derive(Clone, Debug, PartialEq, Eq)]
  pub enum Expression {
    Constant(Int),
    Unary(Unary),
    Binary(Binary),
  }
}

located! {
  #[derive(Clone, Debug, PartialEq, Eq)]
  pub struct Unary {
    pub op: UnaryOp,
    pub expression: Box<Expression>,
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
  // Arithmetic operators.
  Negate,
  // Bitwise operators.
  BitNot,
  // Logical operators.
  Not,
}

located! {
  #[derive(Clone, Debug, PartialEq, Eq)]
  pub struct Binary {
    pub op: BinaryOp,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
  }
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

located! {
  #[derive(Clone, Debug, PartialEq, Eq)]
  pub struct Int {
    pub value: isize,
  }
}

located! {
  #[derive(Clone, Debug, PartialEq, Eq)]
  pub struct Ident {
    pub value: Intern<String>,
  }
}
