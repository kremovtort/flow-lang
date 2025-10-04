```rust
// базовый синтаксис для чистых функций
fn pair(a: A, b: B) -> (A, B) {
  (a, b)
}

// параметрический полиморфизм
fn pair<A, B>(a: A, b: B) -> (A, B) {
  (a, b)
}

let somePair = pair(true, 2: i32);
let somePair = pair(a = true, b = 2: i32); // именованные аргументы

// rank N полиморфизм полиморфизм ограничен данными имеющими универсальное представление
struct Identity<A>(A) // UniversalRepr<Identity<A>> where { UniversalRepr<A> }
type IdentityU<A> = Identity<&A> // имеет универсальное представление

// UniversalRepr<Option<A>> where { UniversalRepr<A> }
enum Option<A> {
  Some(A),
  None,
}

type OptionU<A> = Option<&A> // имеет универсальное представление

// имеет универсальное представление только если <X> F<X> :< UniversalRepr
enum TreeF<F<_>> {
  LeafI32(F<i32>),
  LeafString(F<string>),
  Branch(l: &TreeF<F>, r: &TreeF<F>),
}

fn wrapInner(tree: TreeF<Identity>, f: fn<X>(&X) -> Option<&X>) -> TreeF<Option> {
  ...
}
```

```rust
type Handler<E> = <X> fn(@[E + ..] X) -> @X
```

## Методы
```rust
enum List<A> {
  Nil,
  Cons { head: A, tail: &List<A> }
}

// методы определяются схожим образом с тем, как они определяются в golang
// в остальном это такие же функции
fn <A>(l: List<A>) map<B>(f: fn(A) -> B) -> List<B> {
}

