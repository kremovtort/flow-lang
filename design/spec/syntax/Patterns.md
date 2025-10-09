## Patterns and `match`

### Match expressions
```rust
match value {
  // arms...
}

let x = Some(10);
let y = match x {
  Some(v) if v > 10 => v,
  Some(v) => v + 1,
  None => 0,
};
```

### Pattern forms (illustrative)
- Constructors: `C { field, ... }`, `C(arg1, arg2)`
- Tuples: `(a, b, c)`
- Literals: `0`, `true`, `"str"`
- Wildcard: `_`
- Guards: `if <boolean-expr>` following a pattern


