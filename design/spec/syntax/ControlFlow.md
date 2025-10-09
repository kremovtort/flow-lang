## Control flow

### Conditionals
```rust
if cond {
  // ...
} else if other {
  // ...
} else {
  // ...
}

let x = if cond { 1 } else { 2 };
```

### Loops
```rust
for i in 0..10 { /* ... */ }

while cond { /* ... */ }

loop {
  if done { break }
}
```

### Labels, break, continue
Labels and the use of `break`/`continue` are Rust-like.


