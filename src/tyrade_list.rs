use tyrade_macro::tyrade;
use crate::tyrade_types::*;

tyrade! {

  fn TListLen(L: TList) -> TNum {
    match L {
      Nil => Z,
      Cons(X @ Type, XS @ TList) => S(TListLen(XS))
    }
  }

  fn TListNth(L: TList, I: TNum) -> Type {
    match L {
      Nil => (),
      Cons(X @ Type, XS @ TList) => match I {
        Z => X,
        S(I2 @ TNum) => TListNth(XS, I2)
      }
    }
  }

  fn TListSkip(L: TList, I: TNum) -> TList {
    match L {
      Nil => Nil,
      Cons(X @ Type, XS @ TList) => match I {
        Z => L,
        S(I2 @ TNum) => TListSkip(XS, I2)
      }
    }
  }
}

#[test]
fn list_test() {
  use crate::test_utils::assert_type_eq;
  assert_type_eq::<S<S<Z>>, TListLen<Cons<(), Cons<(), Nil>>>>();
  assert_type_eq::<u64, TListNth<Cons<i32, Cons<u64, Nil>>, S<Z>>>();
}
