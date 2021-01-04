pub struct TyradeError;

// TODO: generate these via macro
pub trait Family1 {
  type Func<A>;
}

pub trait Family2 {
  type Func<A, B>;
}

pub trait Family3 {
  type Func<A, B, C>;
}
