Одна из главных проблем при работе с иммутабельными данными – это обновление структур, особенно если они глубоко вложены. Для решения этой проблемы используются оптики.

В случае языка Flow я хочу сделать оптики first-class citizen.

Простой случай обновления структуры (только линзы):
```rust
struct Person {
  name: String,
  age: i32,
  address: Address,
}

struct Address {
  street: String,
  city: String,
  state: String,
  zip: String,
}

fn main() -> @[IO] () {
  let person = Person {
    name = "John",
    age = 30,
    address = Address {
      street = "123 Main St",
      city = "Anytown",
      state = "CA",
      zip = "12345",
    },
  };

  let newPerson = person.set(#address/#street, "456 Main St");

  println!("New person: {:?}", newPerson);
}
```

Случай с использованием линзы + призмы:

```rust
struct Person {
  name: String,
  age: i32,
  address: Option<Address>,
}

struct Address {
  street: String,
  city: String,
  state: String,
  zip: String,
}

fn main() -> @[IO] () {
  let person = Person {
    name = "John",
    age = 30,
    address = Some(Address {
      street = "123 Main St",
      city = "Anytown",
      state = "CA",
      zip = "12345",
    }),
  };

  let newPerson = person.set(#address/#Some/#street, "456 Main St");

  println!("New person: {:?}", newPerson);
  let newOtherPerson = person
    .updated(#address/#Some/#city, |city| city ++ " City");
  println!("New other person: {:?}", newOtherPerson);
}
```

Иерархия оптик:
```mermaid
classDiagram
  Fold <|-- FoldAffine
  Fold <|-- FoldNonEmpty

  FoldAffine <|-- Getter
  FoldNonEmpty <|-- Getter

  Fold <|-- Traverse
  Setter <|-- Traverse

  FoldAffine <|-- TraverseAffine
  FoldNonEmpty <|-- TraverseNonEmpty

  Traverse <|-- TraverseAffine
  Traverse <|-- TraverseNonEmpty

  Getter <|-- Lens
  TraverseAffine <|-- Lens
  TraverseNonEmpty <|-- Lens

  TraverseAffine <|-- Prism
  Review <|-- Prism

  Lens <|-- Iso
  Prism <|-- Iso

  class Setter {
    + sets(f: fn(A) -> B, s: S) T
  }

  class Fold {
    + folding< F<_>: Foldable >(s: S) F A
  }

  class FoldAffine {
    + foldingAffine(s: S) Option A
  }

  class FoldNonEmpty {
    + foldingNonEmpty< F: FoldableNonEmpty >(s: S) F A
  }

  class Getter {
    + to(s: S) A
  }

  class Traverse {
    + traverse(f: fn(A) -> @B, s: S) @T
  }

  class TraverseAffine {
  }

  class TraverseNonEmpty {
  }

  class Review {
  }

  class Lens {
    + get(s: S) A
    + set(s: S, b: B) T
  }

  class Prism {
    + build(b: B) T
    + matching(s: S) Either T A
  }

  class Iso {}
```
