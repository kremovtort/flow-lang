## Modules and paths

Flow's module system is Rust-like.

### Declarations
- Declare a module with `mod`:

```rust
mod math {
  pub fn add(a: i32, b: i32) -> i32 { a + b }
}
```

- Modules may be named with an uppercase initial and may coincide with type/trait/effect names defined inside the module. Function names in a module must not duplicate type/trait names within the same module.

### Paths and `use`
- Path separator is `::`.
```rust
use math::add;
let x = add(1, 2);
```

- Import multiple symbols:
```rust
use math::{add};
```

- Optional renaming with `as` (reserved and supported syntactically):
```rust
use math::add as plus;
let y = plus(3, 4);
```

### Visibility
- `pub` marks items as publicly visible from outside the module.

### Nested modules
```rust
mod mop {
  mod inner {
    pub fn f() -> i32 { 1 }
  }

  pub fn g() -> i32 { Inner::f() }
}
```

### Capitalized module names, e.g. companion modules
```rust
struct Pair<A, B> {
  first: A,
  second: B,
}

mod Pair {
  fn new<A, B>(first: A, second: B) -> Pair<A, B> { Pair { first, second } }

  // names in declarations and their companions can't overlap
  fn <A, B>(pair: Pair<A, B>) first() -> A { pair.first } // compile-time error - `first` method is already defined in `Pair` struct

  // names of methods (infix functions) and ordinary items don't overlap since they are in different namespaces
  fn first<A, B>(pair: Pair<A, B>) -> A { pair.first } // OK
}
```
