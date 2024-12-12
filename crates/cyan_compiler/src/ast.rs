//! AST Definition using Zephyr ASDL:
//!
//! ```zephyr
//! program             = Program(function_definition)
//! function_definition = Function(identifier name, statement body)
//! statement           = Return(expression)
//! expression          = Constant(int)
//! ```

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
    pub function_definition: FunctionDefinition,
  }
}

spanned! {
  #[derive(Debug, PartialEq, Eq)]
  pub struct FunctionDefinition {
    pub name: Identifier,
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
  #[derive(Debug, PartialEq, Eq)]
  pub enum Expression {
    Constant(Int),
  }
}

spanned! {
  #[derive(Debug, PartialEq, Eq)]
  pub struct Int {
    pub value: isize,
  }
}

spanned! {
  #[derive(Debug, PartialEq, Eq)]
  pub struct Identifier {
    pub value: String,
  }
}
