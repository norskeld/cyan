use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt;

use crate::span::Span;

/// A trait for nodes that should have a location.
pub trait Located {
  /// Returns the location of the node.
  fn location(&self) -> &Location;
}

/// The location of a single node.
#[derive(Clone, PartialEq, Eq)]
pub struct Location {
  /// Absolute byte offsets in the input.
  pub offsets: Span<usize>,
  /// The first and last line of the node.
  pub lines: Span<usize>,
  /// The first and last column of the node.
  pub cols: Span<usize>,
}

impl Location {
  pub fn new(offsets: Span<usize>, lines: Span<usize>, cols: Span<usize>) -> Self {
    Self {
      offsets,
      lines,
      cols,
    }
  }

  /// Merges two locations into one simply by combining their spans' start and end.
  pub fn merge(start: &Self, end: &Self) -> Self {
    Self {
      offsets: Span::new(start.offsets.start, end.offsets.end),
      lines: Span::new(start.lines.start, end.lines.end),
      cols: Span::new(start.cols.start, end.cols.end),
    }
  }

  /// Returns starting line and column of the location.
  pub fn start(&self) -> (usize, usize) {
    (self.lines.start, self.cols.start)
  }
}

impl Default for Location {
  fn default() -> Self {
    Self {
      offsets: Span::new(0, 0),
      lines: Span::new(1, 1),
      cols: Span::new(1, 1),
    }
  }
}

impl fmt::Debug for Location {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "({}:{} - {}:{} @ {:?})",
      self.lines.start, self.cols.start, self.lines.end, self.cols.end, self.offsets
    )
  }
}

impl fmt::Display for Location {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "({}:{} - {}:{})",
      self.lines.start, self.cols.start, self.lines.end, self.cols.end
    )
  }
}

impl Ord for Location {
  fn cmp(&self, other: &Self) -> Ordering {
    let ord = self.lines.start.cmp(&other.lines.start);

    if ord == Ordering::Equal {
      self.cols.start.cmp(&other.cols.start)
    } else {
      ord
    }
  }
}

impl PartialOrd for Location {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}
