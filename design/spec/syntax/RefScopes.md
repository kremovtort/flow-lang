## RefScopes and references (syntax)

RefScopes are syntactic markers for scoped mutability. Semantic rules are described in `../drafts/features/Scoped mutability.md` and `../drafts/features/Types.md`.

### Scope parameters and references
```rust
fn <'s, A: Ord>(xs: &'s mut Vec<A>) -> @['s] () { /* ... */ }
```

### Built-in scopes
- `'global` — global scope; `IO :< Scope<'global>`.
- `'const` — scope for immutable references readable everywhere.

### Sugar in effect rows
```rust
fn <'s>() -> @['s, IO] ()  // ≡ @[Scope<'s>, IO]
```


