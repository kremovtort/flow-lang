## Modules and paths

Flow's module system is Rust-like.

### Declarations
- Declare a module with `mod`:

```rust
mod Math {
  pub fn add(a: i32, b: i32) -> i32 { a + b }
}
```

- Modules may be named with an uppercase initial and may coincide with type/trait/effect names defined inside the module. Function names in a module must not duplicate type/trait names within the same module.

### Paths and `use`
- Path separator is `::`.
```rust
use Math::add;
let x = add(1, 2);
```

- Import multiple symbols:
```rust
use Math::{add};
```

- Optional renaming with `as` (reserved and supported syntactically):
```rust
use Math::add as plus;
let y = plus(3, 4);
```

### Visibility
- `pub` marks items as publicly visible from outside the module.

### Nested modules
```rust
mod Top {
  mod Inner {
    pub fn f() -> i32 { 1 }
  }

  pub fn g() -> i32 { Inner::f() }
}
```


