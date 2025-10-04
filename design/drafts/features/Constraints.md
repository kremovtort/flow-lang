Kind = Constraint

## Синтаксис
Короткий - используется оператор `:<`, который означает, что тип слева соответствует констрейнту справа, например:
```rust
trait Applicative<F<_> :< Functor> { ... }
```
Длинный, в более сложных случаях, где требуется наложить констрейнт на несколько переменных или иметь вложенные констрейнты:
```rust
trait<G<_<_>>, F<_>> SomeTrait<G<F>> where {
	SomeOtherTrait<F<SomeType>>,
} {
	// here is implementation
}
```
### Quantified Constraits
Констрейнты могут быть заданы над абстрактной переменной, например
```rust
trait<F<_>> ... where {
  <A :< Coercible> Coercible<F<A>>
} { ... }
```
### Вложенный where
```rust
trait<F<_>> ... where {
  <A> Coercible<F<A>> where { Coercible<A> }
} { ... }
```
## Равенство типов
Равенство над типами задаётся через `is`:
```rust
trait T1<A> {
  type Person
  fn someFn() @[IO] ()
}

trait T2<A :< T1> where {
  T1<A>::Person is Guest,
} {
  fn someOtherFn() @[IO] ()
}
```
