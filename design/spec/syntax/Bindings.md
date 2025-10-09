## Bindings and scopes

### Local bindings
```rust
let x = 10;
let y: i32 = 20;

let mut z = 0;
z = 1;
```

### Top-level bindings
Top-level immutable bindings must be annotated with an explicit type.

```rust
mod m {
  let x: i32 = 10;
}
```

### Blocks and expression position
- Blocks `{ ... }` contain statements separated by `;`.
- The last item without `;` is an expression yielding the block's value.


