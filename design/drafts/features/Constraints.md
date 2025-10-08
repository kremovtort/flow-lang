Констрейнты это типы описывающие трейты. Трейты задаются через `trait` и всегда имеют параметры в отличии от Rust, где вводится Self тип в скоуп. В этом плане трейты в Flow больше похожи на классы типов в Haskell.

## Синтаксис
Короткий - используется оператор `:<`, который означает, что тип слева соответствует констрейнту справа, например:
```rust
trait Applicative<F<_>> :< Functor<F> { ... }
```

Если требуется ввести дополнительные переменные типов, то они вводятся через `<>` после trait:

```rust
trait<F<_>> Functor<G<_>> where {
  <A :< Coercible> Coercible<F<G<A>>>
} { ... }
```


### Quantified Constraits
Констрейнты могут быть заданы над абстрактной переменной, например
```rust
trait T<F<_>> where {
  <A :< Coercible> Coercible<F<A>>
} { ... }
```
### Вложенный where
```rust
trait T<F<_>> where {
  <A> Coercible<F<A>> where { Coercible<A> }
} { ... }
```
## Равенство типов
Равенство над типами задаётся через `==`:
```rust
trait T1<A> {
  type Person
  fn someFn() @[IO] ()
}

trait T2<A> :< T1<A> where {
  T1<A>::Person == Guest,
} {
  fn someOtherFn() @[IO] ()
}
```

## Инстансы

Инстансы трейтов задаются через `impl`
```rust
impl Functor<Option> {
  fn <A>(opt: Option<A>) map<B>(f: fn(A) -> B) -> Option<B> {
    match opt {
      Some(a) => Some(f(a)),
      None => None,
    }
  }
}
```

