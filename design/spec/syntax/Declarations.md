## Declarations

This section covers data declarations (`struct`, `enum`, GADT-like forms), type aliases, traits with associated items, effect declarations, and `impl` blocks at a syntactic level.

### Structs
```rust
struct Pair<A, B> {
  first: A,
  second: B,
}
```

Field modifiers:
- `const` on fields makes the field immutable regardless of the mutability of the outer variable.

```rust
struct ImutPair<A, B> {
  first: const A,
  second: const B,
}
```

### Enums (algebraic data types)
```rust
enum List<A> {
  Nil,
  Cons { head: A, tail: &List<A> },
}
```

### GADT-like form
```rust
enum Type_<A> {
  Char: Type_<char>,
  Int: Type_<i64>,
  <X> List(Type_<X>): Type_<List<X>>,
}
```

### Tuples (types and values)
```rust
let t: (bool, i32, string) = (false, 2: i32, "s");
let a = t._1; // tuple projections are numbered 1-based
```

### Type aliases
```rust
type Pair<X, Y> = (X, Y)
```

### Traits and instances (overview)
See the dedicated chapter: [Traits and instances](./Traits.md).

### Effects (declarations)
See `../drafts/features/Effects.md` for semantics. Syntax summary:

```rust
effect State<S> {
  op get() -> @S
  op put(s: S) -> @()
}

effect IO :< Scope<'global> {
  op println(s: String) -> @()
}
```


