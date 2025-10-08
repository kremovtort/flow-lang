## Enum
```rust
enum List<A> {
  Nil,
  Cons { head: A, tail: &List<A> }
}
```
## Struct
```rust
struct Pair<A, B> {
  first: A,
  second: B
}
```
## GADT
```rust
enum Type_<A> {
  Char: Type_<char>,
  Int: Type_<i64>,
  <X> List(Type_<X>): Type_<List<X>>
}
```
## Кортежи
```rust
let x: (boolean, i32, string) = (false, 2 : i32, "my string");
println!("({}, {}, {})", x._1, x._2, x._3);
```

# Модификаторы мутабельности
По умолчанию в определении типа не задаётся. Вместо этого (им)мутабельность задаётся при определении типа переменной и наследуется структурой. Но при этом иммутабельность поля можно задать при определении типа
```rust
struct Pair<A, B> {
  first: A,
  second: B,
}

let x = Pair { first = true, second = 2 : i32 };
x.first = false; // ошибка, т.к. x иммутабельно, то и его поля невозможно изменить
let mut x = Pair { first = true, second = 2 : i32 };
x.first = false; // ошибки нет

struct ImutPair<A, B> {
  first: const A,
  second: const B,
}

let mut x = ImutPair { first = true, second = 2 : i32 };
x.first = false; // ошибка компиляции, поле first иммутабельно
```

Типы могут быть параметризованы скоупами:
```rust
struct Pair<'s, A, B> {
  first: &'s A,
  second: &'s B,
}
```
