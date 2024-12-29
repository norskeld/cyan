//! AST definition.

use internment::Intern;

/// A helper macro that adds a `span` field to an AST node and implements [Spanned] for it.
macro_rules! spanned {
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
      /// Contains the node's span.
      pub span: crate::span::Span,
    }

    impl crate::span::Spanned for $name {
      fn span(&self) -> &crate::span::Span {
        &self.span
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

    impl crate::span::Spanned for $name {
      fn span(&self) -> &crate::span::Span {
        match self {
          $($name::$variant(expr) => expr.span(),)*
        }
      }
    }
  };
}

spanned! {
  #[derive(Debug, PartialEq, Eq)]
  pub struct Program {
    pub function: Function,
  }
}

spanned! {
  #[derive(Debug, PartialEq, Eq)]
  pub struct Function {
    pub name: Ident,
    pub body: Statement,
  }
}

spanned! {
  #[derive(Debug, PartialEq, Eq)]
  pub enum Statement {
    Return(Expression),
  }
}

spanned! {
  #[derive(Clone, Debug, PartialEq, Eq)]
  pub enum Expression {
    Constant(Int),
    Unary(Unary),
    Binary(Binary),
  }
}

spanned! {
  #[derive(Clone, Debug, PartialEq, Eq)]
  pub struct Unary {
    pub op: UnaryOp,
    pub expression: Box<Expression>,
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
  BitNot,
  Negate,
}

spanned! {
  #[derive(Clone, Debug, PartialEq, Eq)]
  pub struct Binary {
    pub op: BinaryOp,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
  Add,
  BitAnd,
  BitOr,
  BitShl,
  BitShr,
  BitXor,
  Div,
  Mod,
  Mul,
  Sub,
}

spanned! {
  #[derive(Clone, Debug, PartialEq, Eq)]
  pub struct Int {
    pub value: isize,
  }
}

spanned! {
  #[derive(Clone, Debug, PartialEq, Eq)]
  pub struct Ident {
    pub value: Intern<String>,
  }
}
