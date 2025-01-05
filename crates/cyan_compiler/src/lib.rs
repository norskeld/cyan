#![allow(
  clippy::module_inception,
  clippy::should_implement_trait,
  clippy::new_without_default
)]

pub mod analysis;
pub mod context;
pub mod emitter;
pub mod ir;
pub mod lexer;
pub mod parser;
