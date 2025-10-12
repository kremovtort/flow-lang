## Effects and effect rows (syntax)

This section describes only the concrete syntax for effect rows, named instances, and call-site binding. See `../drafts/features/Effects.md` for semantics and handler forms.

### Effect rows in function types
```rust
fn(A) -> @[IO] A
fn(A) -> @[IO, State<S>] A
fn(A) -> @[IO, State<S>, ..R] A         // open row with named tail R
fn(A) -> @R A                           // short form when entire row is variable R
```

Rules:
- Effect rows are written as `@[E1, E2, ..R]` where `..R` appears only at the end for open rows.
- When the entire effect row is a single variable `R: EffectRow`, use the short form `@R` after `->`.
- RefScope sugar: `@['s, IO]` ≡ `@[Scope<'s>, IO]` (see RefScopes).
- Inside `effect` declarations, operation return types without explicit rows are sugar for `@[Self]`.

### Named effect instances and qualification
Effect atoms can be named in rows to disambiguate multiple instances.

```rust
fn <S>() -> @[s1: State<S>, s2: State<S>] (S, S) {
  let x = s1::get();
  let y = s2::get();
  (x, y)
}
```

Unqualified calls `get()` are only allowed when exactly one visible `State<_>` instance is in lexical scope; otherwise use `s1::get()`.


### Call-site binding with `with { ... }`
Bind named instances or satisfy unnamed requirements by typed keys.

```rust
fn f<S>(...) -> @[s1: State<S>, s2: State<S>] ... { ... }

f(args) with {
  s1 = outer1,
  s2 = outer2,
}

fn g<S>(...) -> @[State<S>] ... { ... }
g(args) with { State<S> = s1 }
```

### Effect parameters in function heads
A function parameter of the form `name: E<T>` is an effect-instance binder. It is erased at elaboration and the bound effect is automatically added to the function's result effect row (sugar for extending the row if not already present).

Rules:
- Syntax: inside the parameter list, `name: E<T>` binds an instance name `name` for the effect `E<T>`.
- Automatic row inclusion: the return type behaves as if it had `@[name: E<T>, ..R]` appended where `R` is a fresh variable.
- The instance can be used with qualification inside the body: `name::op(...)`.
- Underscore inference at call-site (`_`) is allowed only if exactly one visible `E<T>` instance exists.

Examples:
```rust
// explicit binder; return row automatically includes a
fn f<S>(a: State<S>) -> () {
  let s = a::get();
}
// desugars as
fn f<S>(a: State<S>) -> @[a: State<S>, ..R] () { /* ... */ }

// multiple binders
fn g<S, W>(s: State<S>, w: Writer<W>) -> @() {
  // both s and w are available and included in the effect row
}
// desugars as
fn g<S, W>(s: State<S>, w: Writer<W>) -> @[s: State<S>, w: Writer<W>, ..] () { /* ... */ }
```

### Handlers (reference)
Lexical handler selection and detailed forms are specified in `../drafts/features/Effects.md`. This syntax chapter treats `handle`, `op`, and `returning` as reserved words participating in effect declarations and handler blocks.

Returning binder and dependencies:
- The type variable for the carrier is bound immediately after `returning` via `returning<Y>`.
- Block-level ambient dependency row can be declared as `handle in @[R: EffectRow] { ... }`.
- A specific effect handler may extend dependencies with `E in R1 returning<Y> C<Y> { ... }`.

Omitting `returning` in the identity case:
- If the carrier is the identity type constructor `<Y> Y`, the `returning<Y> Y` header may be omitted.
- In the same identity case, the final clause `returning (y: Y) -> Y { y }` may also be omitted.

Minimal handler block (syntax illustration):
```rust
fn runState<S, X, R>(initial: S, action: fn() -> @[State<S>, ..R] X) -> @R (X, S) {
  interpret(handle State<S> returning<Y> (Y, S) {
    op get() -> S { /* ... */ }
    op put(s: S) -> () { /* ... */ }
    returning (y: Y) -> (Y, S) { (y, /* state */ initial) }
  }, action)
}
```

Multiple effect handlers in one block:
```rust
fn runApp<R, S, W, X, E>(env: R, init: S, action: fn() -> @[Reader<R>, State<S>, Writer<W>, ..E] X) -> @E ((X, S), W) {
  interpret(
    handle in @[..] {
      Writer<W> returning<Y> (Y, W) {
        op tell(w: W) -> () { /* ... */ }
        returning (y: Y) -> (Y, W) { (y, /* w */ Monoid::empty()) }
      }

      State<S> returning<Y> (Y, S) {
        op get() -> S { /* ... */ }
        op put(s: S) -> () { /* ... */ }
        returning (y: Y) -> (Y, S) { (y, /* s */ init) }
      }

      // identity carrier: `returning` header and final clause omitted
      Reader<R> {
        op ask() -> R { /* ... */ }
      }
    }
  , action)
}
```

Carrier composition of the block result:
- If handlers in the block define carriers `C1<Y>`, `C2<Y>`, ..., `Cn<Y>` in textual order, the overall carrier is their left-to-right composition using:
```rust
type Compose<F<_>, G<_>> = <X> F<G<X>>
```
- The block result carrier is `Compose<C1, Compose<C2, ... Compose<Cn-1, Cn> ...>>` matching the order of handler declarations inside `handle`.


