use tyrade_macro::tyrade;

tyrade! {
  enum TBool {
    TTrue,
    TFalse
  }

  enum TNum {
    Z,
    S(TNum)
  }

  enum TList {
    Nil,
    Cons(Type, TList)
  }
}
