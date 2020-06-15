#![feature(specialization)]

mod tyrade_bool;
mod tyrade_num;
mod test_utils;

pub use tyrade_macro::tyrade;
pub use bool::*;
pub use num::*;
pub use test_utils::*;
