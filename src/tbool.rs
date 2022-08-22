use crate::tcore::*;
use tyrade_macro::tyrade;

tyrade! {
  enum TBool {
    TTrue,
    TFalse
  }

  fn TIf<Cond, Then, Else>() {
    match Cond {
      TTrue => Then,
      TFalse => Else
    }
  }

  fn TAnd<Left, Right>() {
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
