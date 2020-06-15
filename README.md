# Tyrade: a pure functional language for type-level programming in Rust

Tyrade is a proof-of-concept language showing how Rust traits enable a general purpose type-level programming model. Its goal is to show that type-level programming is possible for useful tasks (not writing Turing machines), and programs can be written in a reasonable way. Here's what the language looks like:

```rust
tyrade! {
  enum TBool {
    TTrue,
    TFalse
  }

  fn TAdd(N1: TNum, N2: TNum) -> TNum {
    match N1 {
      Z => N2,
      S(N3 @ TNum) => TAdd(N3, S(N2))
    }
  }

  fn TDivide(N1: TNum, N2: TNum) -> TNum {
    if N2 <= N1 {
      S(Z) + (N1 - N2) / N2
    } else {
      Z
    }
  }
}

fn num_tests() {
  // 1 + 1 == 2
  assert_type_eq::<S<S<Z>>, TAdd<S<Z>, S<Z>>>();
}
```

At its core, Tyrade supports recursive enum types (kinds, technically) and pure recursive functions. For the main ideas behind Tyrade, continue below or consider reading my [blog post on type-level programming](http://willcrichton.net/notes/type-level-programming/).

## Motivating example: security types

Others have shown that Rust traits are [Turing-complete](https://sdleffler.github.io/RustTypeSystemTuringComplete/) and can be used for e.g. [Fizz-Buzz](https://github.com/doctorn/trait-eval). However, the direct expression of type-level programming in traits is quite obtuse, i.e. the relationship between the conceptual program and the actual traits are obscured.

As a simple example, consider two types `HighSec` and `LowSec` representing the security of an item:

```
struct HighSec;
struct LowSec;

struct Item<T, Sec> {
  t: T,
  _sec: PhantomData<Sec>
}
```

A simple type-level program is to compute the maximum of two security levels. That is, if `S1 = S2 = Low`, then return `Low`, else return `High`. To encode this program in Rust traits, we turn the `MaxLevel` function into a trait, with an `impl` for each condition.

```rust
trait ComputeMaxLevel<Other> {
  type Output;
}

// These impls define the core computation
impl ComputeMaxLevel<LowSec>  for LowSec  { type Output = LowSec;  }
impl ComputeMaxLevel<HighSec> for LowSec  { type Output = HighSec; }
impl ComputeMaxLevel<LowSec>  for HighSec { type Output = HighSec; }
impl ComputeMaxLevel<HighSec> for HighSec { type Output = HighSec; }

// The type alias gives us a more convenient way to "call" the type operator
type MaxLevel<L, R> = <L as ComputeMaxLevel<R>>::Output;
```

The goal of Tyrade is to perform this translation automatically from a functional programming model. Using Tyrade, this program is written as:

```rust
tyrade!{
  enum Security {
    Low,
    High
  }

  fn MaxLevel(S1: Security, S2: Security) -> Security {
    match S1 {
      Low => match S2 {
        Low => Low,
        High => High
      }
      High => High
    }
  }
}
```

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

  fn Dual(S: SessionType) -> SessionType {
    match S {
      Close => S,
      Recv(T @ Type, S2 @ SessionType) => Send(T, Dual(S2)),
      Send(T @ Type, S2 @ SessionType) => Recv(T, Dual(S2)),
      Choose(S2 @ SessionType, S3 @ SessionType) => Offer(Dual(S2), Dual(S3)),
      Offer(S2 @ SessionType, S3 @ SessionType) => Choose(Dual(S2), Dual(S3)),
      Label(S2 @ SessionType) => Label(Dual(S2)),
      Goto(N @ TNum) => S
    }
  }
}

fn session_type_test() {
  assert_type_eq::<
    Recv<i32, Close>,
    Dual<Send<i32, Close>>
  >();
}
```

> Note: the `@` syntax is a hacky way to express type annotations on variables in patterns. For various technical reasons, Tyrade cannot support type inference.

Tyrade provides a standard library of type-level building blocks like [booleans](https://github.com/willcrichton/tyrade/blob/master/src/tyrade_bool.rs), [numbers](https://github.com/willcrichton/tyrade/blob/master/src/tyrade_num.rs), and [lists](https://github.com/willcrichton/tyrade/blob/master/src/tyrade_list.rs). For example, we can use lists to implement the compile-time saving and indexing of jump points in session types.

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
fn TAdd(N1: TNum, N2: TNum) -> TNum {
  match N1 {
    Z => N2,
    S(N3 @ TNum) => TAdd(N3, S(N2))
  }
}
```

And here's the generated Rust code:

```rust
pub trait ComputeTAdd<N2>: TNum {
    type Output: TNum;
}

pub type TAdd<N1, N2> = <N1 as ComputeTAdd<N2>>::Output;

impl<N2> ComputeTAdd<N2> for Z
where N2: TNum
{
    type Output = N2;
}

impl<N3, N2> ComputeTAdd<N2> for S<N3>
where
    N2: TNum,
    N3: TNum + ComputeTAdd<S<N2>>
{
    type Output = TAdd<N3, S<N2>>;
}
```

At a high level, Tyrade does the following for you:
1. The compiler sets up the necessary traits and type definitions (`ComputeTAdd` and `TAdd`).
2. While compiling the operators to types, all operations are added as `where` constraints. For example, `TAdd(N3, S(N2))` creates the constraint `N3: ComputeTAdd<S<N2>>`.
3. The compiler generates a different `impl` for each match branch. In the case of multiple matches, e.g. as in `MaxLevel`, the compiler generates an impl for the cartesian product of all match branches.

See [trans.rs](https://github.com/willcrichton/tyrade/blob/master/tyrade-macro/src/trans.rs) for the details.
