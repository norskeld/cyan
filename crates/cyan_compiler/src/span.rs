use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt;
use std::ops::Range;

/// A trait for expressions that have a location span.
pub trait Spanned {
  /// Returns the location span of the expression.
  fn span(&self) -> &Span;
}

/// The location span of a single expression.
#[derive(Clone, PartialEq, Eq)]
pub struct Span {
  /// Absolute byte offsets in the input.
  pub offsets: Range<usize>,
  /// The first and last line of the expression.
  pub lines: Range<usize>,
  /// The first and last column of the expression.
  pub cols: Range<usize>,
}

impl Span {
  pub fn new(offsets: Range<usize>, lines: Range<usize>, cols: Range<usize>) -> Self {
    Self {
      offsets,
      lines,
      cols,
    }
  }

  /// Merges two spans into one simply by combining their ranges' start and end.
  pub fn merge(start: &Self, end: &Self) -> Self {
    Self {
      offsets: start.offsets.start..end.offsets.end,
      lines: start.lines.start..end.lines.end,
      cols: start.cols.start..end.cols.end,
    }
  }

  /// Returns starting line and column of the location span.
  pub fn start(&self) -> (usize, usize) {
    (self.lines.start, self.cols.start)
  }
}

impl Default for Span {
  fn default() -> Self {
    Self {
      offsets: 0..0,
      lines: 1..1,
      cols: 1..1,
    }
  }
}

impl fmt::Debug for Span {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "({}:{} - {}:{} @ {:?})",
      self.lines.start, self.cols.start, self.lines.end, self.cols.end, self.offsets
    )
  }
}

impl fmt::Display for Span {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "({}:{} - {}:{})",
      self.lines.start, self.cols.start, self.lines.end, self.cols.end
    )
  }
}

impl Ord for Span {
  fn cmp(&self, other: &Self) -> Ordering {
    let ord = self.lines.start.cmp(&other.lines.start);

    if ord == Ordering::Equal {
      self.cols.start.cmp(&other.cols.start)
    } else {
      ord
    }
  }
}

impl PartialOrd for Span {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}
