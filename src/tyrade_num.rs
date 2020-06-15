use tyrade_macro::tyrade;
use crate::tyrade_types::*;
use crate::tyrade_bool::*;

tyrade! {
  fn TAdd(N1: TNum, N2: TNum) -> TNum {
    match N1 {
      Z => N2,
      S(N3 @ TNum) => TAdd(N3, S(N2))
    }
  }

  fn TSub(N1: TNum, N2: TNum) -> TNum {
    match N2 {
      Z => N1,
      S(N3 @ TNum) => match N1 {
        Z => Z,
        S(N4 @ TNum) => TSub(N4, N3)
      }
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

  fn TIsZero(N: TNum) -> TBool {
    match N {
      Z => TTrue,
      S(N1 @ TNum) => TFalse
    }
  }

  fn TDivide(N1: TNum, N2: TNum) -> TNum {
    if N2 <= N1 {
      S(Z) + (N1 - N2) / N2
    } else {
      Z
    }
  }
}

#[test]
fn num_tests() {
  use crate::test_utils::assert_type_eq;
  assert_type_eq::<S<Z>, TAdd<Z, S<Z>>>();
  assert_type_eq::<S<S<Z>>, TAdd<S<Z>, S<Z>>>();

  assert_type_eq::<TTrue, TLessThanEqual<S<Z>, S<S<Z>>>>();
  assert_type_eq::<TFalse, TLessThanEqual<S<S<Z>>, Z>>();

  assert_type_eq::<S<Z>, TSub<S<S<Z>>, S<Z>>>();

  // TODO: this causes overflow?
  // assert_type_eq::<S<S<Z>>, TDivide<S<S<S<S<Z>>>>, S<S<Z>>>>();
}
