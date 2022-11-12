use crate::tbool::*;
use crate::tcore::*;
use tyrade_macro::tyrade;

tyrade! {
  enum TNum {
    Z,
    S(TNum)
  }

  fn TAdd<N1, N2>() {
    match N1 {
      Z => N2,
      S(N3) => TAdd(N3, S(N2))
    }
  }

  fn TSub<N1, N2>() {
    match N2 {
      Z => N1,
      S(N3) => match N1 {
        Z => Z,
        S(N4) => TSub(N4, N3)
      }
    }
  }

  fn TLessThanEqual<N1, N2>() {
    match N1 {
      Z => TTrue,
      S(N3) => match N2 {
        Z => TFalse,
        S(N4) => TLessThanEqual(N3, N4)
      }
    }
  }

  fn TIsZero<N>() {
    match N {
      Z => TTrue,
      S(N1) => TFalse
    }
  }

  // FIXME: this function overflows stack when specialization is turned on
  // fn TDivide<N1, N2>() {
  //   if N2 <= N1 {
  //     S(Z) + (N1 - N2) / N2
  //   } else {
  //     Z
  //   }
  // }
}

#[test]
fn num_tests() {
  use crate::test_utils::assert_type_eq;
  assert_type_eq::<S<Z>, TAdd<Z, S<Z>>>();
  assert_type_eq::<S<S<Z>>, TAdd<S<Z>, S<Z>>>();

  assert_type_eq::<TTrue, TLessThanEqual<S<Z>, S<S<Z>>>>();
  assert_type_eq::<TFalse, TLessThanEqual<S<S<Z>>, S<Z>>>();
  assert_type_eq::<TFalse, TLessThanEqual<S<Z>, Z>>();

  assert_type_eq::<S<Z>, TSub<S<S<Z>>, S<Z>>>();

  // TODO: this causes overflow?
  // assert_type_eq::<Z, TDivide<Z, S<Z>>>();
  // assert_type_eq::<S<S<Z>>, TDivide<S<S<Z>>, S<Z>>>();
}
