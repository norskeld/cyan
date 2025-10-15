#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
  Int,
  Func { params_count: isize },
}
