use proc_macro::TokenStream;
use syn::{parse_macro_input, Item};
use syn::parse::{Result, Parse, ParseStream};
use quote::quote;

use crate::trans;

pub struct ItemVec(pub Vec<Item>);

impl Parse for ItemVec {
  fn parse(input: ParseStream) -> Result<Self> {
    let mut items = Vec::new();
    while !input.cursor().eof() {
      items.push(input.parse::<Item>()?);
    }
    Ok(ItemVec(items))
  }
}

pub fn tyrade(tokens: TokenStream) -> TokenStream {
  let items = parse_macro_input!(tokens as ItemVec).0;
  let tokens = items.into_iter().map(trans::translate_item).collect::<Vec<_>>();
  TokenStream::from(quote!{
    #(#tokens)*
  })
}
