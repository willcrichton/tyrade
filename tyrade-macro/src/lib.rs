use proc_macro::TokenStream;

mod trans;
mod macro_utils;

#[proc_macro]
pub fn tyrade(tokens: TokenStream) -> TokenStream {
  macro_utils::tyrade(tokens)
}
