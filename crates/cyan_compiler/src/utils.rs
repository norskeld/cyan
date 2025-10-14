/// Rounds a given integer `x` to the nearest multiple of another integer `n`, rounding away from
/// zero.
pub fn round_away_from_zero(n: isize, x: isize) -> isize {
  match x % n {
    | 0 => x,
    | rem if x < 0 => x - n - rem,
    | rem => x + n - rem,
  }
}
