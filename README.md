# Tyrade: a pure functional language for type-level programming in Rust

![ci](https://github.com/willcrichton/tyrade/workflows/ci/badge.svg)

* [Motivating example: security types](#motivating-example-security-types)
* [More complex example: session and list types](#more-complex-example-session-and-list-types)
* [How does Tyrade work?](#how-does-tyrade-work)
* [Next steps](#next-steps)

Tyrade is a proof-of-concept language showing how Rust traits enable a general-purpose type-level programming model. Its goal is to show that type-level programming is possible for useful tasks (not just writing Turing machines), and that programs can be written in a reasonable way. Here's what the language looks like:

```rust
tyrade! {
  // A type-level enum for Peano numerals, with "Z" (zero) and "S(n)" (successor).
  enum TNum {
    Z,
    S(TNum)
  }

  // A function that adds two Peano numerals together.
  fn TAdd<N1, N2>() {
    match N1 {
      Z => N2,
      S(N3) => TAdd(N3, S(N2))
    }
  }
}

fn num_tests() {
  // 1 + 1 == 2
  assert_type_eq::<S<S<Z>>, TAdd<S<Z>, S<Z>>>();
}
```

At its core, Tyrade supports recursive enums and pure recursive functions. For the main ideas behind Tyrade, continue below or consider reading my blog post on type-level programming: <https://willcrichton.net/notes/type-level-programming/>

## Motivating example: security types

Others have shown that Rust traits are [Turing-complete](https://sdleffler.github.io/RustTypeSystemTuringComplete/) and can be used for e.g. [Fizz-Buzz](https://github.com/doctorn/trait-eval). However, the direct expression of type-level programs in traits is quite obtuse, i.e. the relationship between the conceptual program and the actual traits is hard to see.

As a simple example, consider two types `HighSec` and `LowSec` representing the security of an item:

```rust
struct High;
struct Low;

struct Item<T, Sec> {
  t: T,
  _sec: PhantomData<Sec>
}
```

A simple type-level program is to compute the maximum of two security levels `S1` and `S2`. That is, if `S1 = S2 = Low`, then return `Low`, else return `High`. To encode this program in Rust traits, we turn the `MaxLevel` function into a trait, with an `impl` for each condition.

```rust
trait ComputeMaxLevel<Other> {
  type Output;
}

// These impls define the core computation
impl ComputeMaxLevel<Low>  for Low  { type Output = Low;  }
impl ComputeMaxLevel<High> for Low  { type Output = High; }
impl ComputeMaxLevel<Low>  for High { type Output = High; }
impl ComputeMaxLevel<High> for High { type Output = High; }

// The type alias gives us a more convenient way to "call" the type operator
type MaxLevel<L, R> = <L as ComputeMaxLevel<R>>::Output;

fn sec_tests() {
  // example unit tests
  assert_type_eq::<Low,  MaxLevel<Low, Low>>();
  assert_type_eq::<High, MaxLevel<Low, High>>();
}
```

The goal of Tyrade is to perform this translation automatically from a functional programming model. Using Tyrade, this program is written as:

```rust
tyrade!{
  enum Security {
    Low,
    High
  }

  fn MaxLevel<S1, S2>() {
    match S1 {
      Low => match S2 {
        Low => Low,
        High => High
      }
      High => High
    }
  }

  // In the high-level language, we can more easily see a chance for simplification.
  fn MaxLevel2<S1, S2>() {
    match S1 {
      Low => S2,
      High => High
    }
  }
}
```

This way, both the type definition and the type-level program are expressed using familiar constructs like `fn`, `enum`, and `match`.

## More complex example: session and list types

Tyrade can be used to define a framework for communication protocols, e.g. [session types](https://github.com/Munksgaard/session-types/). For example, the session types and their duals can be defined as follows:

```rust
tyrade! {
  enum SessionType {
    Close,
    Recv(Type, SessionType),
    Send(Type, SessionType),
    Choose(SessionType, SessionType),
    Offer(SessionType, SessionType),
    Label(SessionType),
    Goto(TNum)
  }

  fn Dual<S>() {
    match S {
      Close => S,
      Recv(T, S2) => Send(T, Dual(S2)),
      Send(T, S2) => Recv(T, Dual(S2)),
      Choose(S2, S3) => Offer(Dual(S2), Dual(S3)),
      Offer(S2, S3) => Choose(Dual(S2), Dual(S3)),
      Label(S2) => Label(Dual(S2)),
      Goto(N) => S
    }
  }
}

fn session_type_test() {
  // The dual of a Send is a Recv
  assert_type_eq::<
    Dual<Send<i32, Close>>,
    Recv<i32, Close>    
  >();
}
```

Tyrade provides a standard library of type-level building blocks like [booleans](https://github.com/willcrichton/tyrade/blob/master/src/tbool.rs), [numbers](https://github.com/willcrichton/tyrade/blob/master/src/tnum.rs), and [lists](https://github.com/willcrichton/tyrade/blob/master/src/tlist.rs). For example, we can use lists to implement the compile-time saving and indexing of jump points in session types.

```rust
struct Chan<Env, S>(PhantomData<(Env, S)>);

impl<Env: TList, S: SessionType> Chan<Env, Label<S>> {
  // label() pushes a type S onto the environment
  fn label(self) -> Chan<Cons<S, Env>, S> {
    Chan(PhantomData)
  }
}

impl<Env: TList, N: TNum> Chan<Env, Goto<N>>
where Env: ComputeTListNth<N> + ComputeTListSkip<N>
{
  // goto<N> gets the Nth type from the environment, removing every type
  // before then
  fn goto(self) -> Chan<TListSkip<Env, N>, TListNth<Env, N>> {
    Chan(PhantomData)
  }
}


fn session_type_test() {
  let c: Chan<
      Cons<Close, Nil>,
      Label<Goto<S<Z>>>> = Chan(PhantomData);

  // label() pushes Goto onto the Env list
  let c: Chan<
      Cons<Goto<S<Z>>, Cons<Close, Nil>>,
      Goto<S<Z>>> = c.label();

  // goto(1) replaces the session type with the type at index 1
  let _: Chan<Cons<Close, Nil>, Close> = c.goto();
}
```

## How does Tyrade work?

Consider the translation of `TAdd`. Here's the Tyrade definition:

```rust
fn TAdd<N1, N2>() {
  match N1 {
    Z => N2,
    S(N3) => TAdd(N3, S(N2))
  }
}
```

And here's the generated Rust code:

```rust
pub trait ComputeTAdd<N2> {
    type Output;
}

pub type TAdd<N1, N2> = <N1 as ComputeTAdd<N2>>::Output;

impl<N2> ComputeTAdd<N2> for Z {
    type Output = N2;
}

impl<N3, N2> ComputeTAdd<N2> for S<N3>
where
    N3: ComputeTAdd<S<N2>>
{
    type Output = TAdd<N3, S<N2>>;
}
```

At a high level, Tyrade does the following for you:
1. The compiler sets up the necessary traits and type definitions (`ComputeTAdd` and `TAdd`).
2. While compiling the operators to types, all operations are added as `where` constraints. For example, `TAdd(N3, S(N2))` creates the constraint `N3: ComputeTAdd<S<N2>>`.
3. The compiler generates a different `impl` for each match branch. In the case of multiple matches, e.g. as in `MaxLevel`, the compiler generates an impl for the cartesian product of all match branches.

See [trans.rs](https://github.com/willcrichton/tyrade/blob/master/tyrade-macro/src/trans.rs) for the details.

## Next steps

Tyrade is experimental, meaning I'm still discovering the boundaries of what's possible. There are two main areas of inquiry:

1. What type-language mechanisms does Rust's trait system permit? For example, I was not able to implement `==` since type equality in Rust doesn't quite work as we need it. Higher-kinded types would be useful as well to enable proper polymorphic type functions.

2. What application areas can benefit from a type-level programming language? Session types are the most complex example I've seen so far, but I'd be really interested to find other use cases for Tyrade.

Please let me know if you'd be interested in using or contributing to Tyrade! Email me at wcrichto@cs.stanford.edu.
