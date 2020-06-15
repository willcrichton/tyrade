use tyrant_macro::tyrant;
use crate::tyrant_bool::*;

tyrant! {
  enum TNum {
    Z,
    S(TNum)
  }

  fn TAdd(N1: TNum, N2: TNum) -> TNum {
    match N1 {
      Z => N2,
      S(N3 @ TNum) => TAdd(N3, S(N2))
    }
  }

  fn TLessThanEqual(N1: TNum, N2: TNum) -> TBool {
    match N1 {
      Z => TTrue,
      S(N3 @ TNum) => match N2 {
        Z => TFalse,
        S(N4 @ TNum) => TLessThanEqual(N3, N4)
      }
    }
  }
}

#[test]
fn num_tests() {
  use crate::test_utils::assert_type_eq;
  assert_type_eq::<S<Z>, TAdd<Z, S<Z>>>();
  assert_type_eq::<S<S<Z>>, TAdd<S<Z>, S<Z>>>();
  assert_type_eq::<TTrue, TLessThanEqual<S<Z>, S<S<Z>>>>();
  assert_type_eq::<TFalse, TLessThanEqual<S<Z>, Z>>();
}
