use tyrant_macro::tyrant;

tyrant! {
  enum TBool {
    TTrue,
    TFalse
  }
}

pub trait ComputeTypeEquals<T> { type Output: TBool; }
pub type TypeEquals<T1, T2> = <T1 as ComputeTypeEquals<T2>>::Output;

impl<T> ComputeTypeEquals<T> for T { type Output = TTrue; }
impl<T1, T2> ComputeTypeEquals<T2> for T1 { default type Output = TFalse; }

tyrant! {
  fn TIf(Cond: TBool, Then: Type, Else: Type) -> Type {
    match Cond {
      TTrue => Then,
      TFalse => Else
    }
  }

  fn TAnd(Left: TBool, Right: TBool) -> TBool {
    match Left {
      TTrue => match Right {
        TTrue => TTrue,
        TFalse => TFalse
      }
      TFalse => TFalse
    }
  }
}

#[test]
fn bool_tests() {
  use crate::test_utils::assert_type_eq;
  assert_type_eq::<TTrue, TAnd<TTrue, TTrue>>();
  assert_type_eq::<TFalse, TAnd<TTrue, TFalse>>();
  assert_type_eq::<TFalse, TAnd<TFalse, TTrue>>();
  assert_type_eq::<TFalse, TAnd<TFalse, TFalse>>();
}
