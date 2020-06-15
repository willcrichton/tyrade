use tyrade_macro::tyrade;
use crate::tyrade_types::*;

tyrade! {
  fn TIf<K>(Cond: TBool, Then: K, Else: K) -> K {
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
