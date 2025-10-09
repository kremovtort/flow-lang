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

### Omitting scope type arguments and inference
- Scope type parameters may be omitted in generic parameter lists and type applications when they can be inferred from context (e.g., from `&'s T` references and effect rows `@['s, ..]`).
- If inference fails or is ambiguous, compilation must report an error.

Examples:
```rust
fn <'s>(p: &'s Point) -> @['s] () { /* ... */ }

let gp: &'global Point = /* ... */;
// 's inferred as 'global from the argument
usePoint(gp);

let p: &Point = /* no explicit scope */;
// error: cannot infer RefScope parameter 's for usePoint
usePoint(p);
```


