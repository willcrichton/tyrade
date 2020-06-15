use tyrade::*;

tyrade! {
  enum Security {
    Low,
    High
  }

  fn MaxSec(S1: Security, S2: Security) -> Security {
    match S1 {
      Low => match S2 {
        Low => Low,
        High => High
      }
      High => High
    }
  }
}

#[test]
fn sec_test() {
  assert_type_eq::<Low, MaxSec<Low, Low>>();
  assert_type_eq::<High, MaxSec<Low, High>>();
  assert_type_eq::<High, MaxSec<High, Low>>();
  assert_type_eq::<High, MaxSec<High, High>>();
}
