#![feature(specialization)]

use tyrant::*;

// tyrant! {
//   enum List {
//     Nil,
//     Cons(X, List)
//   }

//   fn Len(L: List) -> Unsigned {
//     match L {
//       Nil => 0,
//       Cons(X, XS) => 1 + Len(XS)
//     }
//   }
// }

tyrant! {
  enum Security {
    Low,
    High
  }

  // fn MaxSec(S1: Security, S2: Security) -> Security {
  //   if S1 == Low && S2 == Low { Low }
  //   else { High }
  //   // match (S1, S2) {
  //   //   (Low, Low) => Low,
  //   //   _ => High
  //   // }
  // }
}

// trait ComputeMaxSec<S2: Security> { type Output; }
// type MaxSec<S1, S2> = <S1 as ComputeMaxSec<S2>>::Output;

// impl<S1: Security, S2: Security> ComputeMaxSec<S2> for S1
// where S1: ComputeTypeEquals<Low>, TypeEquals<S1, Low>: ComputeTIf<Low, High> {
//   type Output = TIf<TypeEquals<S1, Low>, Low, High>;
// }

// impl<S1: Security, S2: Security> ComputeMaxSec<S2> for S1
// {
//   default type Output = High;
// }

// impl<S1: Security, S2: Security> ComputeMaxSec<S2> for S1
// where (S1, High): NotEqual, (S2, High): NotEqual {
//   type Output = Low;
// }

// fn assert_type_eq<T1: 'static, T2: 'static>() {
//   use std::any::TypeId;
//   assert_eq!(TypeId::of::<T1>(), TypeId::of::<T2>());
// }

// #[test]
// fn test() {
//   assert_type_eq::<Low, MaxSec<Low, Low>>();
//   assert_type_eq::<High, MaxSec<High, Low>>();
//   assert_type_eq::<High, MaxSec<Low, High>>();
//   assert_type_eq::<High, MaxSec<High, High>>();
// }
