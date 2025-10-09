## Generics, `where`, and quantified constraints (syntax)

Flow supports parametric polymorphism, higher-kinded parameters, and a `where { ... }` block that can include saturated predicates and local type aliases.

### Type parameters and value polymorphism
```rust
fn pair<A, B>(a: A, b: B) -> (A, B) { (a, b) }
```

### HKT parameters with `F<_>`
```rust
fn f<F<_>>(x: F<i32>) -> F<string>
```

Nested HKTs are allowed:
```rust
fn f<G<_<_>>>(x: G<Option>) -> G<Vec>
```

### `where { ... }` block (syntax only)
Contents may mix saturated predicates and local type/effect/constraint aliases usable across the whole signature.

```rust
fn pair<A, B>(a: A, b: B) -> Pair<A, B>
  where {
    type Pair<X, Y> = (X, Y),
    Eq<Pair<A, B>>,
    Show<Pair<A, B>>,
  }
```

### Traits
For trait headers, inheritance with `:<`, and instance syntax, see [Traits and instances](./Traits.md). For sort/universe rules, see `../drafts/features/Types.md`.

### Scope parameters inference
Scope parameters (RefScopes denoted with a leading `'`) may be omitted in generic parameter lists and applications when they can be inferred from references and effect rows. If the compiler cannot infer them unambiguously, a compile-time error is emitted. See `RefScopes.md`.


