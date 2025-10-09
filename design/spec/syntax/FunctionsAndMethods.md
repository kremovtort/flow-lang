## Functions and methods

### Function declarations
```rust
fn add(a: i32, b: i32) -> i32 { a + b }

fn pair<A, B>(a: A, b: B) -> (A, B) { (a, b) }
```

Named arguments are supported at call sites:
```rust
let p = pair(a = true, b = 2: i32);
```

### Methods
Methods are declared similarly to functions; receiver is the first parameter positionally, with optional polymorphism markers.

```rust
// ordinary function
fn add(a: i32, b: i32) -> i32 { a + b }

// infix-style method declaration and call
fn (a: i32) add(b: i32) -> i32 { a + b }

add(1, 2);   // regular call
1.add(2);    // method-style call

// polymorphic over the left argument
fn <A>(a: A) wrap() -> Wrapper<A> { Wrapper { value: a } }

// polymorphic over the right argument
fn (n: i32) times<A>(a: A) -> List<A> { List::repeat(n, a) }

10.times("a");
```

### Module-associated methods
Module names may coincide with type names; methods grouped in such a module behave like associated functions.

```rust
struct Pair<A, B> { first: A, second: B }

mod Pair {
  fn new<A, B>(first: A, second: B) -> Pair<A, B> { Pair { first, second } }
  fn <A, B>(pair: Pair<A, B>) swap() -> Pair<B, A> {
    Pair { first: pair.second, second: pair.first }
  }
}
```


