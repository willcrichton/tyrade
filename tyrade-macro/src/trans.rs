use std::collections::{HashMap, HashSet};
use syn::{Item, ItemEnum, ItemFn, Fields, Type, TypePath, Ident, FnArg, Pat, PatIdent, Path, Stmt, Expr, BinOp, PathSegment, ExprBlock, parse2, Block, ReturnType, GenericParam, TypeParam};
use quote::ToTokens;
use syn::visit_mut::{self, VisitMut};
use proc_macro2::{TokenStream, Span};
use quote::quote;
use itertools::Itertools;
use std::hash::Hash;

#[allow(dead_code)]
fn fmt<T: ToTokens>(t: &T) -> String {
  format!("{}", t.to_token_stream())
}

macro_rules! tpanic {
  ($args:tt) => {
    panic!("[{}:{}] {}", file!(), line!(), format!($args))
  }
}

fn type_to_ident(ty: &Type) -> Option<Ident> {
  if let Type::Path(TypePath { path, .. }) = ty {
    Some(path.get_ident().unwrap_or_else(|| tpanic!("type_to_ident path.get_ident()")).clone())
  } else {
    None
  }
}

fn ident_to_type(ident: &Ident) -> Type {
  let segment = PathSegment::from(ident.clone());
  let path = Path::from(segment);
  Type::Path(TypePath { path, qself: None })
}

fn str_to_type<T: Into<String>>(t: T) -> Type {
  let ident = Ident::new(&t.into(), Span::call_site());
  ident_to_type(&ident)
}

fn block_to_expr(block: Block) -> Expr {
  Expr::Block(ExprBlock {
    attrs: vec![],
    label: None,
    block
  })
}

fn kind_to_type(kind: &Ident) -> Option<Type> {
  if &format!("{}", kind) == "Type" {
    None
  } else {
    Some(ident_to_type(kind))
  }
}

struct SubstituteVisitor {
  src: Ident,
  dst: Type
}

impl VisitMut for SubstituteVisitor {
  fn visit_type_mut(&mut self, node: &mut Type) {
    visit_mut::visit_type_mut(self, node);

    if let Type::Path(path) = node {
      if let Some(ident) = path.path.get_ident() {
        if ident == &self.src {
          *node = self.dst.clone();
        }
      }
    }
  }
}

fn substitute(ty: &mut Type, src: &Ident, dst: &Type) {
  let mut renamer = SubstituteVisitor { src: src.clone(), dst: dst.clone() };
  renamer.visit_type_mut(ty);
}

struct RenameVisitor {
  src: Ident,
  dst: Ident
}

impl VisitMut for RenameVisitor {
  fn visit_ident_mut(&mut self, node: &mut Ident) {
    if &self.src == node {
      *node = self.dst.clone();
    }
  }
}

fn rename(fn_: &mut ItemFn, src: Ident, dst: Ident) {
  let mut renamer = RenameVisitor { src, dst };
  renamer.visit_item_fn_mut(fn_);
}


pub fn translate_enum(enum_: ItemEnum) -> TokenStream {
  let trait_name = &enum_.ident;

  let types = enum_.variants.iter().map(|variant| {
    let name = &variant.ident;
    let params = match &variant.fields {
      Fields::Unnamed(unnamed) => {
        unnamed.unnamed.iter().enumerate().map(|(i, field)| {
          let kind = type_to_ident(&field.ty)
            .unwrap_or_else(|| unimplemented!());
          (str_to_type(format!("T{}", i)), kind)
        }).collect::<Vec<_>>()
      },
      Fields::Unit => vec![],
      _ => unimplemented!()
    };

    let params_with_bounds = params.iter().map(|(id, kind)| {
      match kind_to_type(kind) {
        Some(_) => quote!{ #id: #kind },
        None => quote!{ #id }
      }
    }).collect::<Vec<_>>();

    let just_params = params.iter().map(|(id, _)| quote!{ #id } )
      .collect::<Vec<_>>();

    let compute_trait = if !just_params.is_empty() {
      let compute_name = Ident::new(
        &format!("Compute{}", name), Span::call_site());
      let first_param = &just_params[0];
      let remaining_params = &just_params[1..];
      quote! {
        pub trait #compute_name<#(#remaining_params),*> {}
        impl<#(#params_with_bounds),*> #compute_name<#(#remaining_params),*> for #first_param {}
      }
    } else {
      quote! {}
    };

    quote!{
      pub struct #name<#(#params_with_bounds),*>(
        pub ::std::marker::PhantomData<(#(#just_params),*)>
      );
      impl<#(#params_with_bounds),*> #trait_name for #name<#(#just_params),*> {}
      #compute_trait
    }
  }).collect::<Vec<_>>();


  quote! {
    pub trait #trait_name {}
    #(#types)*
  }
}

#[derive(Clone, Debug)]
struct FnTransEnv {
  args: Vec<Type>,
  quantifiers: Vec<Ident>,
  bounds: HashMap<Type, Vec<Type>>,
  substitutions: HashMap<Ident, Type>
}

fn merge_vecs<T: Clone + Eq + Hash>(v1: &[T], v2: &[T]) -> Vec<T> {
  let h1 = v1.iter().collect::<HashSet<_>>();
  let h2 = v2.iter().collect::<HashSet<_>>();
  (&h1 | &h2).iter().cloned().cloned().collect::<Vec<_>>()
}

impl FnTransEnv {
  fn merge(mut self, other: FnTransEnv) -> FnTransEnv {
    let args = self.args.iter()
      .zip(other.args.iter())
      .map(|(arg1, arg2)| {
        // Pick the "biggest" arg (e.g. prefer F<X> over Y)
        // If both are specialized, should be an error
        if let Type::Path(_) = arg1 {
          arg2
        } else {
          arg1
        }
      })
      .cloned()
      .collect::<Vec<_>>();

    let quantifiers = merge_vecs(&self.quantifiers, &other.quantifiers);

    let mut bounds = self.bounds.clone();
    for (k, v) in other.bounds.clone().into_iter() {
      let v2 = bounds.entry(k).or_insert_with(Vec::new);
      *v2 = merge_vecs(&v, v2);
    }

    // TODO: is this correct?
    self.substitutions.extend(other.substitutions.into_iter());

    FnTransEnv { args, quantifiers, bounds, substitutions: self.substitutions }
  }
}

#[derive(Debug, Clone)]
struct FnTransOutput {
  output_ty: TokenStream,
  env: FnTransEnv
}

impl FnTransOutput {
  fn merge<F>(outputs: Vec<Vec<FnTransOutput>>, f: F)
           -> Vec<FnTransOutput>
    where F: Fn(FnTransEnv, Vec<TokenStream>) -> FnTransOutput
  {
    outputs.into_iter()
      .map(|v| v.into_iter())
      .multi_cartesian_product()
      .map(|outp| {
        let args = outp.clone().into_iter()
          .map(|o| o.output_ty).collect::<Vec<_>>();
        let first_env = outp[0].env.clone();
        let merged_env = outp.into_iter().skip(1).map(|o| o.env)
            .fold(first_env,
                  |env1, env2| env1.merge(env2));
        f(merged_env, args)
      })
      .collect::<Vec<_>>()
  }
}

fn translate_expr(env: &FnTransEnv, cur_kind: &Ident, expr: &Expr) -> Vec<FnTransOutput> {
  match expr {
    Expr::Match(match_) => {
      let matched_ident = if let Expr::Path(path) = &*match_.expr {
        path.path.get_ident().unwrap_or_else(|| tpanic!("path.get_ident()"))
      } else {
        unimplemented!("match expr")
      };

      match_.arms.iter().map(|arm| {
        let (variant, fields) = match &arm.pat {
          Pat::Ident(PatIdent { ident, .. }) => {
            (ident.clone(), vec![])
          }
          Pat::TupleStruct(tuple_struct) => {
            let variant = tuple_struct.path.get_ident().unwrap_or_else(|| tpanic!("path.get_ident()")).clone();
            let fields = tuple_struct.pat.elems.iter().map(|p| {
              match &p {
                Pat::Ident(PatIdent { ident, subpat, .. }) => {
                  let kind = if let Some((_, p2)) = subpat {
                    match &**p2 {
                      Pat::Ident(PatIdent { ident, .. }) => ident.clone(),
                      _ => tpanic!("RHS of @ must be an ident")
                    }
                  } else {
                    tpanic!("Match tuple struct must have @ trait annotation")
                  };

                  (ident.clone(), kind)
                },
                _ => unimplemented!("match pat ident: {:?}", p)
              }
            }).collect::<Vec<_>>();
            (variant, fields)
          },
          _ => unimplemented!("match pat")
        };

        let mut env = env.clone();
        let field_names = fields.iter()
          .map(|(ident, _)| ident)
          .cloned()
          .collect::<Vec<_>>();

        // if args = [X, Y] and expr is match Y { Q(Z) => ... }
        // then update args to be [X, Q<Z>]
        let new_type: Type = parse2(quote! {
          #variant<#(#field_names),*>
        }).unwrap_or_else(|_| tpanic!("parse failed"));

        for arg in env.args.iter_mut() {
          substitute(arg, matched_ident, &new_type);
        }

        // if quantifiers = [X, Y] then replace [Y] with [Z]
        env.quantifiers = env.quantifiers.into_iter()
          .filter(|ident| ident != matched_ident)
          .collect::<Vec<_>>();
        env.quantifiers.extend(field_names);

        // if bounds = {Y: Foo<X>} then rename to {Q<Z>: Foo<X>}
        env.bounds = env.bounds.into_iter()
          .map(|(mut k, mut v)| {
            substitute(&mut k, matched_ident, &new_type);
            for ty in v.iter_mut() {
              substitute(ty, matched_ident, &new_type);
            }
            (k, v)
          })
          .collect::<HashMap<_, _>>();

        // add field bounds
        for (ident, kind) in fields.iter() {
          if let Some(kind) = kind_to_type(kind) {
            env.bounds.insert(ident_to_type(ident), vec![kind]);
          }
        }

        // add substitution for Y -> Q<Z>
        for (_, v) in env.substitutions.iter_mut() {
          substitute(v, matched_ident, &new_type);
        }
        env.substitutions.insert(matched_ident.clone(), new_type);

        translate_expr(&env, cur_kind, &arm.body)
      }).flatten().collect::<Vec<_>>()
    },

    Expr::Path(path) => {
      let ident = path.path.get_ident().unwrap_or_else(|| tpanic!("path.get_ident()"));
      let ty = env.substitutions.get(&ident).cloned()
        .unwrap_or_else(|| ident_to_type(ident));
      vec![FnTransOutput {
        env: env.clone(),
        output_ty: quote! { #ty }
      }]
    }

    Expr::Tuple(tuple) => {
      if tuple.elems.is_empty() {
        vec![FnTransOutput {
          env: env.clone(),
          output_ty: quote! { () }
        }]
      } else {
        unimplemented!("tuple")
      }
    }

    Expr::Binary(binop) => {
      let left = &binop.left;
      let right = &binop.right;
      let op = match &binop.op {
        BinOp::Eq(_) => quote!{ TypeEquals },
        BinOp::And(_) => quote!{ TAnd },
        BinOp::Le(_) => quote!{ TLessThanEqual },
        BinOp::Add(_) => quote!{ TAdd },
        BinOp::Div(_) => quote!{ TDivide },
        BinOp::Sub(_) => quote!{ TSub },
        _ => unimplemented!("binop {:?}", binop.op)
      };
      let trans_expr: Expr = parse2(quote!{ #op(#left, #right) }).unwrap_or_else(|_| tpanic!("trans_expr parse"));
      translate_expr(env, cur_kind, &trans_expr)
    }

    Expr::If(if_) => {
      let cond = &if_.cond;
      let then = &if_.then_branch;
      let else_ = &if_.else_branch.as_ref()
        .unwrap_or_else(|| tpanic!("If expression must have an 'else'")).1;
      let if_name = Ident::new(
        &format!("TIf{}", cur_kind), Span::call_site());
      let trans_expr: Expr = parse2(quote!{ #if_name(#cond, #then, #else_) }).unwrap_or_else(|_| tpanic!("trans_expr parse"));
      translate_expr(env, cur_kind, &trans_expr)
    }

    Expr::Block(block) => {
      if let Stmt::Expr(expr) = &block.block.stmts[0] {
        translate_expr(env, cur_kind, &expr)
      } else {
        unimplemented!("block")
      }
    }

    Expr::Call(call) => {
      let func_ident = if let Expr::Path(path) = &*call.func {
        path.path.get_ident().unwrap_or_else(|| tpanic!("path.get_ident()"))
      } else {
        unimplemented!("func ident")
      };

      let args = call.args.iter()
        .map(|arg| translate_expr(env, cur_kind, arg))
        .collect::<Vec<_>>();

      FnTransOutput::merge(args, |mut env, args| {
        let first_arg: Type = parse2(args[0].clone()).unwrap_or_else(|_| tpanic!("first_arg parse"));
        let bounds = env.bounds
          .entry(first_arg)
          .or_insert_with(Vec::new);
        let compute_ident = Ident::new(
          &format!("Compute{}", func_ident), Span::call_site());
        let remaining_args = &args[1..];
        bounds.push(
          parse2(quote!{ #compute_ident<#(#remaining_args),*> }).unwrap_or_else(|_| tpanic!("bounds parse")));

        let output_ty = quote!{ #func_ident<#(#args),*> };
        FnTransOutput { output_ty, env }
      })
    }

    Expr::Paren(paren) => {
      translate_expr(env, cur_kind, &paren.expr)
    }

    _ => unimplemented!("expr: {:?}", expr)
  }
}

// HACK: have to do this without HKT :(
const BASE_KINDS: &[&str] = &["TNum", "TBool", "TList"];

fn gen_impls(fn_: ItemFn) -> TokenStream {
  let fn_name = &fn_.sig.ident;
  let compute_name = Ident::new(
    &format!("Compute{}", fn_.sig.ident), Span::call_site());

  let args = fn_.sig.inputs.iter().map(|arg| {
    if let FnArg::Typed(pat_type) = arg {
      if let Pat::Ident(PatIdent { ident, .. }) = &*pat_type.pat  {
        (ident, type_to_ident(&pat_type.ty).unwrap())
      } else {
        unimplemented!("fn arg pat")
      }
    } else {
      unreachable!("fn arg")
    }
  }).collect::<Vec<_>>();

  // Construct initial translation environment from args
  let init_args = args.iter()
    .map(|(arg, _)| ident_to_type(arg))
    .collect::<Vec<_>>();
  let init_quantifiers = args.iter()
    .map(|(arg, _)| (**arg).clone())
    .collect::<Vec<_>>();
  let init_bounds = args.iter()
    .filter_map(|(arg, kind)| {
      kind_to_type(kind).map(|kind| (ident_to_type(arg), vec![kind]))
    })
    .collect::<HashMap<_, _>>();
  let init_env = FnTransEnv {
    args: init_args,
    quantifiers: init_quantifiers,
    bounds: init_bounds,
    substitutions: HashMap::new()
  };

  let return_kind: Type = if let ReturnType::Type(_, kind) = &fn_.sig.output {
    (**kind).clone()
  } else {
    tpanic!("Function must have return type")
  };

  // Run translation
  let fn_trans_outputs = translate_expr(
    &init_env,
    &type_to_ident(&return_kind).unwrap(),
    &block_to_expr(*fn_.block.clone()));

  // Convert each path into an impl block
  let impls = fn_trans_outputs.iter().map(|outp| {
    let output_ty = &outp.output_ty;
    let args = &outp.env.args;
    let first_arg = &args[0];
    let remaining_args = &args[1..];

    let quantifiers = &outp.env.quantifiers;
    let bounds = outp.env.bounds.iter()
      .map(|(ty, bounds)| quote!{ #ty : #(#bounds)+* })
      .collect::<Vec<_>>();

    quote! {
      impl<#(#quantifiers),*> #compute_name<#(#remaining_args),*>
        for #first_arg
      where #(#bounds),*
      {
        type Output = #output_ty;
      }
    }
  }).collect::<Vec<_>>();

  // Get names needed for trait declaration
  let (arg_names, _): (Vec<_>, Vec<_>) =
    args.iter().cloned().skip(1).unzip();
  let (first_name, first_ty) = args[0].clone();
  let return_kind = kind_to_type(&type_to_ident(&return_kind).unwrap())
      .map(|kind| quote! { #kind })
      .unwrap_or_else(TokenStream::new);
  quote! {
    pub trait #compute_name<#(#arg_names),*>: #first_ty {
      type Output: #return_kind;
    }

    pub type #fn_name<#first_name,#(#arg_names),*> =
      <#first_name as #compute_name<#(#arg_names),*>>::Output;

    #(#impls)*
  }
}

pub fn translate_fn(fn_: ItemFn) -> TokenStream {
  let generics = &fn_.sig.generics.params;
  let impls = if !generics.is_empty() {
    if let GenericParam::Type(TypeParam { ident, .. }) = &generics[0] {
      BASE_KINDS.iter()
        .map(|base_kind| {
          let mut fn_ = fn_.clone();

          // Instantiate generic kind K as base_kind
          rename(
            &mut fn_,
            ident.clone(),
            Ident::new(base_kind, Span::call_site()));

          let fn_name = fn_.sig.ident.clone();
          let new_name = format!("{}{}", fn_name, base_kind);
          rename(
            &mut fn_,
            fn_name,
            Ident::new(&new_name, Span::call_site()));

          gen_impls(fn_)
        })
        .collect::<Vec<TokenStream>>()
    } else {
      unimplemented!("generics")
    }
  } else {
    vec![gen_impls(fn_)]
  };

  quote! { #(#impls)* }
}

pub fn translate_item(item: Item) -> TokenStream {
  match item {
    Item::Enum(enum_) => translate_enum(enum_),
    Item::Fn(fn_) => translate_fn(fn_),
    _ => { unimplemented!("item") }
  }
}
