use itertools::Itertools;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use quote::ToTokens;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use syn::visit_mut::{self, VisitMut};
use syn::{
  parse2, BinOp, Block, Expr, ExprBlock, Fields, GenericParam, Ident, Item, ItemEnum,
  ItemFn, Pat, PatIdent, Path, PathSegment, Stmt, Type, TypeParam, TypePath,
};

#[allow(dead_code)]
fn fmt<T: ToTokens>(t: &T) -> String {
  format!("{}", t.to_token_stream())
}

macro_rules! tpanic {
  ($args:tt) => {
    panic!("[{}:{}] {}", file!(), line!(), format!($args))
  };
}

fn type_to_ident(ty: &Type) -> Option<Ident> {
  if let Type::Path(TypePath { path, .. }) = ty {
    Some(
      path
        .get_ident()
        .unwrap_or_else(|| tpanic!("type_to_ident path.get_ident()"))
        .clone(),
    )
  } else {
    None
  }
}

fn ident_to_type(ident: &Ident) -> Type {
  let segment = PathSegment::from(ident.clone());
  let path = Path::from(segment);
  Type::Path(TypePath { path, qself: None })
}

fn str_to_ident<T: Into<String>>(t: T) -> Ident {
  Ident::new(&t.into(), Span::call_site())
}

fn str_to_type<T: Into<String>>(t: T) -> Type {
  ident_to_type(&str_to_ident(t))
}

fn block_to_expr(block: Block) -> Expr {
  Expr::Block(ExprBlock {
    attrs: vec![],
    label: None,
    block,
  })
}

fn _kind_to_type(kind: &Ident) -> Option<Type> {
  if &format!("{}", kind) == "Type" {
    None
  } else {
    Some(ident_to_type(kind))
  }
}

struct SubstituteVisitor {
  src: Ident,
  dst: Type,
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
  let mut renamer = SubstituteVisitor {
    src: src.clone(),
    dst: dst.clone(),
  };
  renamer.visit_type_mut(ty);
}

struct RenameVisitor {
  src: Ident,
  dst: Ident,
}

impl VisitMut for RenameVisitor {
  fn visit_ident_mut(&mut self, node: &mut Ident) {
    if &self.src == node {
      *node = self.dst.clone();
    }
  }
}

fn _rename(fn_: &mut ItemFn, src: Ident, dst: Ident) {
  let mut renamer = RenameVisitor { src, dst };
  renamer.visit_item_fn_mut(fn_);
}

pub fn translate_enum(enum_: ItemEnum) -> TokenStream {
  let trait_name = &enum_.ident;

  let types = enum_
    .variants
    .iter()
    .map(|variant| {
      let name = &variant.ident;
      let params = match &variant.fields {
        Fields::Unnamed(unnamed) => unnamed
          .unnamed
          .iter()
          .enumerate()
          .map(|(i, field)| {
            let kind = type_to_ident(&field.ty).unwrap_or_else(|| unimplemented!());
            (str_to_type(format!("T{}", i)), kind)
          })
          .collect::<Vec<_>>(),
        Fields::Unit => vec![],
        _ => unimplemented!(),
      };

      let params_with_bounds = params
        .iter()
        .map(|(id, _kind)| quote! { #id })
        .collect::<Vec<_>>();

      let just_params = params
        .iter()
        .map(|(id, _)| quote! { #id })
        .collect::<Vec<_>>();

      let compute_trait = if !just_params.is_empty() {
        let compute_name = Ident::new(&format!("Compute{}", name), Span::call_site());
        let first_param = &just_params[0];
        let remaining_params = &just_params[1..];
        quote! {
          pub trait #compute_name<#(#remaining_params),*> {}
          impl<#(#params_with_bounds),*> #compute_name<#(#remaining_params),*> for #first_param {}
        }
      } else {
        quote! {}
      };

      quote! {
        pub struct #name<#(#params_with_bounds),*>(
          pub ::std::marker::PhantomData<(#(#just_params),*)>
        );
        impl<#(#params_with_bounds),*> #trait_name for #name<#(#just_params),*> {}
        #compute_trait
      }
    })
    .collect::<Vec<_>>();

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
  substitutions: HashMap<Ident, Type>,
}

fn merge_vecs<T: Clone + Eq + Hash>(v1: &[T], v2: &[T]) -> Vec<T> {
  let h1 = v1.iter().collect::<HashSet<_>>();
  let h2 = v2.iter().collect::<HashSet<_>>();
  (&h1 | &h2).iter().cloned().cloned().collect::<Vec<_>>()
}

impl FnTransEnv {
  fn merge(mut self, other: FnTransEnv) -> FnTransEnv {
    let args = self
      .args
      .iter()
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

    FnTransEnv {
      args,
      quantifiers,
      bounds,
      ..self
    }
  }
}

#[derive(Debug, Clone)]
struct FnTransOutput {
  output_ty: Type,
  env: FnTransEnv,
}

impl FnTransOutput {
  fn merge<F>(outputs: Vec<Vec<FnTransOutput>>, f: F) -> Vec<FnTransOutput>
  where
    F: Fn(FnTransEnv, Vec<Type>) -> FnTransOutput,
  {
    outputs
      .into_iter()
      .map(|v| v.into_iter())
      .multi_cartesian_product()
      .map(|outp| {
        let args = outp
          .clone()
          .into_iter()
          .map(|o| o.output_ty)
          .collect::<Vec<_>>();
        let first_env = outp[0].env.clone();
        let merged_env = outp
          .into_iter()
          .skip(1)
          .map(|o| o.env)
          .fold(first_env, |env1, env2| env1.merge(env2));
        f(merged_env, args)
      })
      .collect::<Vec<_>>()
  }
}

fn translate_expr(env: &FnTransEnv, expr: &Expr) -> Vec<FnTransOutput> {
  match expr {
    Expr::Match(match_) => {
      let matched_ident = if let Expr::Path(path) = &*match_.expr {
        path
          .path
          .get_ident()
          .unwrap_or_else(|| tpanic!("path.get_ident()"))
      } else {
        unimplemented!("match expr")
      };

      match_
        .arms
        .iter()
        .map(|arm| {
          let (variant, fields) = match &arm.pat {
            Pat::Ident(PatIdent { ident, .. }) => (ident.clone(), vec![]),
            Pat::TupleStruct(tuple_struct) => {
              let variant = tuple_struct
                .path
                .get_ident()
                .unwrap_or_else(|| tpanic!("path.get_ident()"))
                .clone();
              let fields = tuple_struct
                .pat
                .elems
                .iter()
                .map(|p| match &p {
                  Pat::Ident(PatIdent { ident, .. }) => ident.clone(),
                  _ => unimplemented!("match pat ident: {:?}", p),
                })
                .collect::<Vec<_>>();
              (variant, fields)
            }
            _ => unimplemented!("match pat"),
          };

          let mut env = env.clone();
          let field_names = fields;

          // if args = [X, Y] and expr is match Y { Q(Z) => ... }
          // then update args to be [X, Q<Z>]
          let new_type: Type = parse2(quote! {
            #variant<#(#field_names),*>
          })
          .unwrap_or_else(|_| tpanic!("parse failed"));

          for arg in env.args.iter_mut() {
            substitute(arg, matched_ident, &new_type);
          }

          // if quantifiers = [X, Y] then replace [Y] with [Z]
          env.quantifiers = env
            .quantifiers
            .into_iter()
            .filter(|ident| ident != matched_ident)
            .collect::<Vec<_>>();
          env.quantifiers.extend(field_names);

          // if bounds = {Y: Foo<X>} then rename to {Q<Z>: Foo<X>}
          env.bounds = env
            .bounds
            .into_iter()
            .map(|(mut k, mut v)| {
              substitute(&mut k, matched_ident, &new_type);
              for ty in v.iter_mut() {
                substitute(ty, matched_ident, &new_type);
              }
              (k, v)
            })
            .collect::<HashMap<_, _>>();

          // add substitution for Y -> Q<Z>
          for (_, v) in env.substitutions.iter_mut() {
            substitute(v, matched_ident, &new_type);
          }
          env.substitutions.insert(matched_ident.clone(), new_type);

          translate_expr(&env, &arm.body)
        })
        .flatten()
        .collect::<Vec<_>>()
    }

    Expr::Path(path) => {
      let ident = path
        .path
        .get_ident()
        .unwrap_or_else(|| tpanic!("path.get_ident()"));
      let ty = env
        .substitutions
        .get(&ident)
        .cloned()
        .unwrap_or_else(|| ident_to_type(ident));
      vec![FnTransOutput {
        env: env.clone(),
        output_ty: parse2(quote! { #ty }).unwrap(),
      }]
    }

    Expr::Tuple(tuple) => {
      if tuple.elems.is_empty() {
        vec![FnTransOutput {
          env: env.clone(),
          output_ty: parse2(quote! { () }).unwrap(),
        }]
      } else {
        unimplemented!("tuple")
      }
    }

    Expr::Binary(binop) => {
      let left = &binop.left;
      let right = &binop.right;
      let op = match &binop.op {
        BinOp::Eq(_) => quote! { TypeEquals },
        BinOp::And(_) => quote! { TAnd },
        BinOp::Le(_) => quote! { TLessThanEqual },
        BinOp::Add(_) => quote! { TAdd },
        BinOp::Div(_) => quote! { TDivide },
        BinOp::Sub(_) => quote! { TSub },
        _ => unimplemented!("binop {:?}", binop.op),
      };
      let trans_expr: Expr =
        parse2(quote! { #op(#left, #right) }).unwrap_or_else(|_| tpanic!("trans_expr parse"));
      translate_expr(env, &trans_expr)
    }

    Expr::If(if_) => {
      let cond = &if_.cond;
      let then = &if_.then_branch;
      let else_ = &if_
        .else_branch
        .as_ref()
        .unwrap_or_else(|| tpanic!("If expression must have an 'else'"))
        .1;
      let if_name = Ident::new("TIf", Span::call_site());
      let trans_expr: Expr = parse2(quote! { #if_name(#cond, #then, #else_) })
        .unwrap_or_else(|_| tpanic!("trans_expr parse"));
      translate_expr(env, &trans_expr)
    }

    Expr::Block(block) => {
      if let Stmt::Expr(expr) = &block.block.stmts[0] {
        translate_expr(env, &expr)
      } else {
        unimplemented!("block")
      }
    }

    Expr::Call(call) => {
      let func_ident = if let Expr::Path(path) = &*call.func {
        path
          .path
          .get_ident()
          .unwrap_or_else(|| tpanic!("path.get_ident()"))
      } else {
        unimplemented!("func ident")
      };

      let args = call
        .args
        .iter()
        .map(|arg| translate_expr(env, arg))
        .collect::<Vec<_>>();

      FnTransOutput::merge(args, |mut env, args| {
        let compute_ty = if env.quantifiers.iter().any(|i| i == func_ident) {
          let family = str_to_ident(format!("Family{}", args.len()));
          let bounds = env
            .bounds
            .entry(parse2(quote! { #func_ident }).unwrap())
            .or_insert_with(Vec::new);
          bounds.push(parse2(quote! { #family }).unwrap());
          quote! { #func_ident ::Func }
        } else {
          let first_arg: Type = args[0].clone();
          let ident = str_to_ident(format!("Compute{}", func_ident));
          let bounds = env
            .bounds
            .entry(first_arg)
            .or_insert_with(Vec::new);
          let remaining_args = &args[1..];
          bounds.push(
            parse2(quote! { #ident<#(#remaining_args),*> })
              .unwrap_or_else(|_| tpanic!("bounds parse")),
          );
          quote! { #func_ident }
        };

        let output_ty = parse2(quote! { #compute_ty<#(#args),*> }).unwrap();
        FnTransOutput { output_ty, env }
      })
    }

    Expr::Paren(paren) => translate_expr(env, &paren.expr),

    _ => unimplemented!("expr: {:?}", expr),
  }
}

fn gen_impls(fn_: ItemFn) -> TokenStream {
  let fn_name = &fn_.sig.ident;
  let compute_name = str_to_ident(format!("Compute{}", fn_.sig.ident));
  let compute_family_name = str_to_ident(format!("Compute{}Family", fn_.sig.ident));

  let args: Vec<_> = fn_
    .sig
    .generics
    .params
    .iter()
    .map(|param| {
      if let GenericParam::Type(TypeParam { ident, .. }) = param {
        ident
      } else {
        unimplemented!("generics")
      }
    })
    .collect();

  let family_trait = str_to_ident(format!("Family{}", args.len()));

  // Construct initial translation environment from args
  let init_args = args
    .iter()
    .map(|arg| ident_to_type(arg))
    .collect::<Vec<_>>();
  let init_quantifiers = args.iter().map(|arg| (**arg).clone()).collect::<Vec<_>>();
  let init_bounds = HashMap::new();
  let init_env = FnTransEnv {
    args: init_args,
    quantifiers: init_quantifiers,
    bounds: init_bounds,
    substitutions: HashMap::new(),
  };

  // Run translation
  let fn_trans_outputs = translate_expr(&init_env, &block_to_expr(*fn_.block.clone()));

  // Convert each path into an impl block
  let impls = fn_trans_outputs
    .iter()
    .map(|outp| {
      let output_ty = &outp.output_ty;
      let args = &outp.env.args;
      let first_arg = &args[0];
      let remaining_args = &args[1..];

      let quantifiers = &outp.env.quantifiers;
      let bounds = outp
        .env
        .bounds
        .iter()
        .map(|(ty, bounds)| quote! { #ty : #(#bounds)+* })
        .collect::<Vec<_>>();

      quote! {
        impl<#(#quantifiers),*> #compute_name<#(#remaining_args),*>
          for #first_arg
        where #(#bounds),*
        {
          type Output = #output_ty;
        }
      }
    })
    .collect::<Vec<_>>();

  // Get names needed for trait declaration
  let arg_names: Vec<_> = args.iter().cloned().skip(1).collect();
  let first_name = args[0].clone();

  quote! {
    pub trait #compute_name<#(#arg_names),*> {
      type Output;
    }

    pub type #fn_name<#first_name,#(#arg_names),*> =
      <#first_name as #compute_name<#(#arg_names),*>>::Output;

    pub struct #compute_family_name;
    impl #family_trait for #compute_family_name {
      type Func<#(#args),*> = #fn_name<#(#args),*>;
    }

    impl<#first_name, #(#arg_names),*> #compute_name<#(#arg_names),*> for #first_name {
      default type Output = TyradeError;
    }

    #(#impls)*
  }
}

pub fn translate_fn(fn_: ItemFn) -> TokenStream {
  let impls = gen_impls(fn_);
  quote! { #impls }
}

pub fn translate_item(item: Item) -> TokenStream {
  match item {
    Item::Enum(enum_) => translate_enum(enum_),
    Item::Fn(fn_) => translate_fn(fn_),
    _ => {
      unimplemented!("item")
    }
  }
}
