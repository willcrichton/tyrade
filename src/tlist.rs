use tyrade_macro::tyrade;
use crate::tnum::*;
use crate::tcore::*;

tyrade! {
  enum TList {
    Nil,
    Cons(Type, TList)
  }

  fn TListLen<L>() {
    match L {
      Nil => Z,
      Cons(X, XS) => S(TListLen(XS))
    }
  }

  fn TListNth<L, I>() {
    match L {
      Nil => (),
      Cons(X, XS) => match I {
        Z => X,
        S(I2) => TListNth(XS, I2)
      }
    }
  }

  fn TListSkip<L, I>() {
    match L {
      Nil => Nil,
      Cons(X, XS) => match I {
        Z => L,
        S(I2) => TListSkip(XS, I2)
      }
    }
  }

  fn TListMap<L, F>() {
    match L {
      Nil => Nil,
      Cons(X, XS) => Cons(F(X), TListMap(XS, F))
    }
  }
}

#[test]
fn list_test() {
  use crate::test_utils::assert_type_eq;
  use crate::tbool::*;

  assert_type_eq::<S<S<Z>>, TListLen<Cons<(), Cons<(), Nil>>>>();
  assert_type_eq::<u64, TListNth<Cons<i32, Cons<u64, Nil>>, S<Z>>>();
  assert_type_eq::<Cons<TFalse, Cons<TTrue, Nil>>, TListMap<Cons<S<Z>, Cons<Z, Nil>>, TIsZeroFamily>>();
}
