Констрейнты это типы описывающие трейты. Трейты задаются через `trait` и всегда имеют параметры в отличии от Rust, где вводится Self тип в скоуп. В этом плане трейты в Flow больше похожи на классы типов в Haskell.

### Синтаксис объявления трейтов
Короткий - используется оператор `=>`, который означает, что тип слева соответствует констрейнту справа, например:
```rust
trait Applicative<F<_>> => Functor<F> { ... }
```

Если требуется ввести дополнительные переменные типов, то они вводятся через `<>` после trait:

```rust
trait<F<_>> Functor<G<_>> => Coercible<F<G<A>>> { ... }
```

### Синтаксис задания констрейнтов

Наиболее общая форма задания констрейнтов это where блок:
```rust
fn f<F<_>>(x: F<A>, f: fn(A) -> B) -> F<B> where { Functor<F> } { /* function body */ }
```
Так же есть короткий синтаксис, когда нужно применить констрейнт к одной типовой переменной:
```rust
fn f<F<_> :< Functor>(x: F<A>, f: fn(A) -> B) -> F<B> { /* function body */ }
```
Если к одной типовой переменной нужно применить несколько констрейнтов, то они задаются через кортеж:
```rust
fn f<A :< (T1, T2)>(x: A) -> A { /* function body */ }
```

### Quantified Constraits
Констрейнты могут быть заданы над абстрактной переменной, например
```rust
fn f<F<_>>(f: F<A>) -> F<A> where { <A :< Coercible> Coercible<F<A>> } { /* function body */ }
```
### Вложенный where
Where блок может быть вложенным, что как раз полезно для quantified constraints:
```rust
fn f<F<_>>(f: F<A>) -> F<A> where { <A> Coercible<F<A>> where { Coercible<A> } } { /* function body */ }
```
## Равенство типов
Равенство над типами задаётся через `==`:
```rust
trait T1<A> {
  type Person
  fn someFn() @[IO] ()
}

trait T2<A> => T1<A>, T1<A>::Person == Guest {
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

impl<F<_> :< Applicative> Functor<F> {
  fn <A>(opt: F<A>) map<B>(f: fn(A) -> B) -> F<B> {
    f.pure().apply(opt)
  }
}
// или
impl<F<_>> Functor<F> where { Applicative<F> } {
  fn <A>(opt: F<A>) map<B>(f: fn(A) -> B) -> F<B> {
    f.pure().apply(opt)
  }
}
```

так же доступно ключевое слово `where` для обозначения дополнительных ограничений для инстанса, подробнее в [Types.md](./Types.md)

