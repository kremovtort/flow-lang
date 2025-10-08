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


type Handler<E> = <X> fn(@[E, ..] X) -> @X

// Методы
enum List<A> {
  Nil,
  Cons { head: A, tail: &List<A> }
}

// методы определяются схожим образом с тем, как они определяются в golang
// в остальном это такие же функции
fn <A>(l: List<A>) map<B>(f: fn(A) -> B) -> List<B> {
}

