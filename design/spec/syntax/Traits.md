## Traits and instances (syntax)

This chapter defines the concrete syntax for trait declarations and instance implementation blocks (`impl`). Semantics are covered in `../drafts/features/Constraints.md`.

### Trait declarations
- No `where { ... }` block in trait headers.
- Parent predicates are listed with `:<` after the trait head.
- If additional type variables are needed only for the parent list, they are bound immediately after the `trait` keyword: `trait<X> Name<Y> :< Parent<X, Y>`.

Forms:
```rust
// Simple trait with an associated type and a method
trait Ord<A> {
  type Ordering
  fn compare(a: A, b: A) -> Ordering
}

// Inheritance via :<
trait Show<A> :< Display<A>, Debug<A> {
  fn show(a: A) -> String
}

// Binding extra type variables for the parent list
trait<X> CoerceDeep<F<_>> :< Coercible<F<X>> {
  // methods / associated items...
}
```

Notes:
- Associated types are declared inside the body.
- Equality constraints are not written in the trait header; they can appear in other signatures that support `where { ... }` (e.g., functions or `impl`).

### Instance implementation blocks (`impl`)
An instance provides definitions for the methods declared by a trait for a particular type constructor or type arguments.

Forms:
```rust
impl Functor<Option> {
  fn <A>(opt: Option<A>) map<B>(f: fn(A) -> B) -> Option<B> {
    match opt {
      Some(a) => Some(f(a)),
      None => None,
    }
  }
}
```

`impl` headers may use generics and (outside trait headers) may include a `where { ... }` block if needed by the surrounding signature rules; see `Generics.md`.

### Guidelines
- Trait headers: use `:<` for parents; bind extra variables as `trait<X> ...` when needed by parents.
- Keep parent entries fully-applied (saturated) predicates.
- Definitions inside `impl` follow the ordinary function/method syntax.


