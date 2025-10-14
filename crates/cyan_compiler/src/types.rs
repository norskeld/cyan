#[derive(Debug, PartialEq, Eq)]
pub enum Type {
  Int,
  Func { params_count: i32 },
}
