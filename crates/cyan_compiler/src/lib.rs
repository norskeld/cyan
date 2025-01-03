#![allow(
  clippy::module_inception,
  clippy::should_implement_trait,
  clippy::new_without_default
)]

pub mod emitter;
pub mod ir;
pub mod lexer;
pub mod location;
pub mod parser;
pub mod span;
