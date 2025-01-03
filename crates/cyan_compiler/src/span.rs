use core::fmt;
use core::ops::{Bound, RangeBounds};

/// A half-open span, bounded inclusively below and exclusively above.
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Span<Idx> {
  pub start: Idx,
  pub end: Idx,
}

impl<Idx> Span<Idx> {
  pub fn new(start: Idx, end: Idx) -> Self {
    Self { start, end }
  }

  /// Converts [Range][core::ops::Range] into a [Span].
  pub fn from_range(range: core::ops::Range<Idx>) -> Self {
    range.into()
  }

  /// Converts [Span] into a [Range][core::ops::Range].
  pub fn into_range(self) -> core::ops::Range<Idx> {
    self.into()
  }

  /// Returns `true` if `item` is contained in the span.
  ///
  /// See [Range::contains][core::ops::Range::contains].
  pub fn contains<U>(&self, item: &U) -> bool
  where
    Idx: PartialOrd<U>,
    U: ?Sized + PartialOrd<Idx>,
  {
    <Self as RangeBounds<Idx>>::contains(self, item)
  }

  /// Returns `true` if the span contains no items.
  ///
  /// See [Range::contains][core::ops::Range::contains].
  pub fn is_empty(&self) -> bool
  where
    Idx: PartialOrd,
  {
    self.start >= self.end
  }

  /// Returns the exact length of the span.
  pub fn len(&self) -> usize
  where
    core::ops::Range<Idx>: ExactSizeIterator,
    Self: Copy,
  {
    self.into_range().len()
  }
}

impl<Idx: fmt::Debug> fmt::Debug for Span<Idx> {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.start.fmt(fmt)?;
    write!(fmt, "..")?;
    self.end.fmt(fmt)?;

    Ok(())
  }
}

impl<Idx> From<core::ops::Range<Idx>> for Span<Idx> {
  fn from(core::ops::Range { start, end }: core::ops::Range<Idx>) -> Self {
    Self { start, end }
  }
}

impl<Idx> From<Span<Idx>> for core::ops::Range<Idx> {
  fn from(value: Span<Idx>) -> Self {
    value.start..value.end
  }
}

impl<Idx> RangeBounds<Idx> for Span<Idx> {
  fn start_bound(&self) -> Bound<&Idx> {
    Bound::Included(&self.start)
  }

  fn end_bound(&self) -> Bound<&Idx> {
    Bound::Excluded(&self.end)
  }
}

impl<Idx> RangeBounds<Idx> for Span<&Idx> {
  fn start_bound(&self) -> Bound<&Idx> {
    Bound::Included(self.start)
  }

  fn end_bound(&self) -> Bound<&Idx> {
    Bound::Excluded(self.end)
  }
}

impl<Idx> IntoIterator for Span<Idx>
where
  core::ops::Range<Idx>: Iterator<Item = Idx>,
{
  type IntoIter = core::ops::Range<Idx>;
  type Item = Idx;

  fn into_iter(self) -> Self::IntoIter {
    self.start..self.end
  }
}
