#![allow(dead_code, unused_imports, unused_variables)]

use std::collections::{HashMap, HashSet};
use syn::{Item, ItemEnum, ItemFn, Fields, Type, TypePath, Ident, FnArg, Pat, PatIdent, Path, Stmt, Expr, BinOp, PathSegment, ExprBlock, parse2, Block};
use proc_macro2::{TokenStream, Span};
use quote::quote;
use itertools::Itertools;
use std::hash::Hash;

fn type_to_ident(ty: &Type) -> Option<Ident> {
  if let Type::Path(TypePath { path, .. }) = ty {
    Some(path.get_ident().unwrap().clone())
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

pub fn translate_enum(enum_: ItemEnum) -> TokenStream {
  let trait_name = &enum_.ident;

  let types = enum_.variants.iter().map(|variant| {
    let name = &variant.ident;
    let params = match &variant.fields {
      Fields::Unnamed(unnamed) => {
        unnamed.unnamed.iter().enumerate().map(|(i, field)| {
          let kind = type_to_ident(&field.ty).or_else(|| unimplemented!());
          (str_to_type(format!("T{}", i)), kind)
        }).collect::<Vec<_>>()
      },
      Fields::Unit => vec![],
      _ => unimplemented!()
    };

    let params_with_bounds = params.iter().map(|(id, kind)| {
      quote!{ #id: #kind }
    }).collect::<Vec<_>>();

    let just_params = params.iter().map(|(id, _)| quote!{ #id } )
      .collect::<Vec<_>>();


    let compute_trait = if just_params.len() > 0 {
      let compute_name = Ident::new(
        &format!("Compute{}", name), Span::call_site());
      let first_param = &just_params[0];
      let remaining_params = &just_params[1..];
      quote! {
        pub trait #compute_name<#(#remaining_params),*> {}
        impl<#(#just_params),*> #compute_name<#(#remaining_params),*> for #first_param {}
      }
    } else {
      quote! {}
    };

    quote!{
      pub struct #name<#(#params_with_bounds),*>(
        pub PhantomData<(#(#just_params),*)>
      );
      impl<#(#params_with_bounds),*> #trait_name for #name<#(#just_params),*> {}
      #compute_trait
    }
  }).collect::<Vec<_>>();


  quote! {
    use std::marker::PhantomData;
    pub trait #trait_name {}
    #(#types)*
  }
}

struct FnTranslator {
  // fn_name: Ident,
  // fn_params: Vec<Ident>
}

#[derive(Clone, Debug)]
struct FnTransEnv {
  args: Vec<Type>,
  quantifiers: Vec<Ident>,
  bounds: HashMap<Type, Vec<Type>>,
  substitutions: HashMap<Ident, Type>
}

fn merge_vecs<T: Clone + Eq + Hash>(v1: &Vec<T>, v2: &Vec<T>) -> Vec<T> {
  let h1 = v1.iter().collect::<HashSet<_>>();
  let h2 = v2.iter().collect::<HashSet<_>>();
  (&h1 | &h2).iter().cloned().cloned().collect::<Vec<_>>()
}

impl FnTransEnv {
  fn merge(self, other: FnTransEnv) -> FnTransEnv {
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
      let v2 = bounds.entry(k).or_insert(vec![]);
      *v2 = merge_vecs(&v, v2);
    }

    // TODO: is this correct?
    let mut substitutions = self.substitutions.clone();
    substitutions.extend(other.substitutions.clone().into_iter());

    FnTransEnv { args, quantifiers, bounds, substitutions }
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
        let merged_env = outp.into_iter().map(|o| o.env)
            .fold_first(|env1, env2| env1.merge(env2)).unwrap();
        f(merged_env, args)
      })
      .collect::<Vec<_>>()
  }
}

impl FnTranslator {
  fn translate_expr(&self, env: &FnTransEnv, expr: &Expr) -> Vec<FnTransOutput> {
    match expr {
      Expr::Match(match_) => {
        let matched_ident = if let Expr::Path(path) = &*match_.expr {
          path.path.get_ident().unwrap()
        } else {
          unimplemented!("match expr")
        };

        let match_ty = ident_to_type(matched_ident);
        let arg_idx = env.args.iter()
          .position(|arg| arg == &match_ty)
          .expect(&format!("Invalid match on {}", matched_ident));

        match_.arms.iter().map(|arm| {
          let (variant, fields) = match &arm.pat {
            Pat::Ident(PatIdent { ident, .. }) => {
              (ident.clone(), vec![])
            }
            Pat::TupleStruct(tuple_struct) => {
              let variant = tuple_struct.path.get_ident().unwrap().clone();
              let fields = tuple_struct.pat.elems.iter().map(|p| {
                if let Pat::Ident(PatIdent { ident, subpat, .. }) = &p  {
                  let kind = if let Some((_, p2)) = subpat {
                    if let Pat::Ident(PatIdent { ident, .. }) = &**p2 {
                      ident.clone()
                    } else {
                      panic!("RHS of @ must be an ident")
                    }
                  } else {
                    panic!("Match tuple struct must have @ trait annotation")
                  };

                  (ident.clone(), kind)
                } else {
                  unimplemented!("match pat ident")
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
          }).unwrap();
          env.args[arg_idx] = new_type.clone();

          // if quantifiers = [X, Y] then replace [Y] with [Z]
          env.quantifiers = env.quantifiers.into_iter()
            .filter(|ident| ident != matched_ident)
            .collect::<Vec<_>>();
          env.quantifiers.extend(field_names);

          // if bounds = {Y: Foo<X>} then rename to {Q<Z>: Foo<X>}
          let bounds = env.bounds
            .remove(&ident_to_type(&matched_ident))
            .unwrap();
          env.bounds.insert(new_type.clone(), bounds);

          // add field bounds
          for (ident, kind) in fields.iter() {
            env.bounds.insert(ident_to_type(ident), vec![ident_to_type(kind)]);
          }

          // add substitution for Y -> Q<Z>
          env.substitutions.insert(matched_ident.clone(), new_type);

          self.translate_expr(&env, &arm.body)
        }).flatten().collect::<Vec<_>>()
      },

      Expr::Path(path) => {
        let ident = path.path.get_ident().unwrap();
        let ty = env.substitutions.get(&ident).cloned()
          .unwrap_or_else(|| ident_to_type(ident));
        vec![FnTransOutput {
          env: env.clone(),
          output_ty: quote! { #ty }
        }]
      }

      Expr::Binary(binop) => {
        let left = &binop.left;
        let right = &binop.right;
        let op = match &binop.op {
          BinOp::Eq(_) => quote!{ TypeEquals },
          BinOp::And(_) => quote!{ BoolAnd },
          _ => unimplemented!("binop")
        };
        let trans_expr: Expr = parse2(quote!{ #op(#left, #right) }).unwrap();
        self.translate_expr(env, &trans_expr)
      }

      Expr::If(if_) => {
        let cond = &if_.cond;
        let then = &if_.then_branch;
        let else_ = &if_.else_branch.as_ref()
          .expect("If expression must have an 'else'").1;
        let trans_expr: Expr = parse2(quote!{ TIf(#cond, #then, #else_) }).unwrap();
        self.translate_expr(env, &trans_expr)
      }

      Expr::Block(block) => {
        if let Stmt::Expr(expr) = &block.block.stmts[0] {
          self.translate_expr(env, &expr)
        } else {
          unimplemented!("block")
        }
      }

      Expr::Call(call) => {
        let func_ident = if let Expr::Path(path) = &*call.func {
          path.path.get_ident().unwrap()
        } else {
          unimplemented!("func ident")
        };

        let args = call.args.iter()
          .map(|arg| self.translate_expr(env, arg))
          .collect::<Vec<_>>();

        FnTransOutput::merge(args, |mut env, args| {
          let first_arg: Type = parse2(args[0].clone()).unwrap();
          let bounds = env.bounds
            .entry(first_arg.clone())
            .or_insert_with(|| Vec::new());
          let compute_ident = Ident::new(
            &format!("Compute{}", func_ident), Span::call_site());
          let remaining_args = &args[1..];
          bounds.push(
            parse2(quote!{ #compute_ident<#(#remaining_args),*> }).unwrap());

          let output_ty = quote!{ #func_ident<#(#args),*> };
          FnTransOutput { output_ty, env }
        })
      }

      _ => unimplemented!("expr: {:?}", expr)
    }
  }
}


pub fn translate_fn(fn_: ItemFn) -> TokenStream {
  let name = fn_.sig.ident;
  let compute_name = Ident::new(
    &format!("Compute{}", name), Span::call_site());

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

  let init_args = args.iter()
    .map(|(arg, _)| ident_to_type(arg))
    .collect::<Vec<_>>();
  let init_quantifiers = args.iter()
    .map(|(arg, _)| (**arg).clone())
    .collect::<Vec<_>>();
  let init_bounds = args.iter()
    .filter_map(|(arg, kind)| {
      if &format!("{}", kind) == "Type" {
        None
      } else {
        Some((ident_to_type(arg), vec![ident_to_type(kind)]))
      }
    })
    .collect::<HashMap<_, _>>();

  let init_env = FnTransEnv {
    args: init_args,
    quantifiers: init_quantifiers,
    bounds: init_bounds,
    substitutions: HashMap::new()
  };

  let translator = FnTranslator {};
  let fn_trans_outputs = translator.translate_expr(
    &init_env,
    &block_to_expr(*fn_.block.clone()));

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

  let (arg_names, arg_tys): (Vec<_>, Vec<_>) =
    args.iter().cloned().skip(1).unzip();
  let (first_name, first_ty) = args[0].clone();
  quote! {
    pub trait #compute_name<#(#arg_names),*>: #first_ty { type Output; }
    pub type #name<#first_name,#(#arg_names),*> =
      <#first_name as #compute_name<#(#arg_names),*>>::Output;
    #(#impls)*
  }
}

pub fn translate_item(item: Item) -> TokenStream {
  match item {
    Item::Enum(enum_) => translate_enum(enum_),
    Item::Fn(fn_) => translate_fn(fn_),
    _ => { unimplemented!("item") }
  }
}
