# Система типов Flow

## Вселенные (Universes)

Flow имеет четыре основные вселенные с бесконечной иерархией уровней:

- **Type₀, Type₁, Type₂, ...** — обычные типы данных
- **Constraint₀, Constraint₁, Constraint₂, ...** — ограничения (traits/type classes)
- **Effect₀, Effect₁, Effect₂, ...** — алгебраические эффекты
- **RefScope₀, RefScope₁, RefScope₂, ...** — области видимости ссылок (scoped mutability)

### Правила иерархии

```
Type₀ : Type₁ : Type₂ : ...
Constraint₀ : Constraint₁ : Constraint₂ : ...
Effect₀ : Effect₁ : Effect₂ : ...
RefScope₀ : RefScope₁ : RefScope₂ : ...
```

Уровни нужны для поддержки параметризации вселенных сами собой (избежание парадокса Рассела).

### Связь между вселенными

RefScope можно использовать как Effect через builtin конструктор:
```rust
Scope : fn(RefScope₀) -> Effect₀ : Effect₁
```

Это позволяет записывать scopes в effect rows: `@['s, IO]` где `'s : RefScope₀`.

## Sorts (типы типов)

Sort описывает "тип типа" и включает:
- Конкретные вселенные: `Type₀`, `Constraint₁`, etc.
- Функциональные типы: `fn(Sort₁, ..., Sortₙ) -> Sort`

### Правила для функциональных типов

Для `fn(A₁: Sort₁, ..., Aₙ: Sortₙ) -> B: Sort_B`:

**Уровень:** `max(level(Sort₁), ..., level(Sortₙ), level(Sort_B)) + 1`

**Вселенная определяется по codomain:**
- Если `B : Type_n` → результат в `Type_(max_level + 1)`
- Если `B : Constraint_n` → результат в `Constraint_(max_level + 1)`
- Если `B : Effect_n` → результат в `Effect_(max_level + 1)`

### Кортежи

```
(A₁, ..., Aₙ) : U_(max(level(A₁), ..., level(Aₙ)))
```

Все компоненты кортежа должны принадлежать одной и той же вселенной U ∈ {Type, Constraint, Effect, RefScope}.

### Phase distinction: разделение type-level и term-level

Flow использует **predicative** систему типов, где квантификация поднимает уровень:
```
<A: Type₀> Type₀ : Type₁
```

**На term-level (значения программы):**

Разрешено:
- Значения любых типов: `value : T` где `T : Type_n` для любого n
- Полиморфные функции: `identity : <A> fn(A) -> A : Type₁`

Запрещено (отсутствие зависимых типов):
- Типы как значения: `fn(...) -> Type_n` ❌
- Типы в аргументах: `fn(t: Type_n) -> ...` ❌
- Вселенные (Type_k, Constraint_k, Effect_k, RefScope_k) не являются допустимыми типами значений

**На type-level:**
- Можно свободно оперировать со вселенными: `fn(Type) -> Type` ✓
- Type constructors, constraints, effects работают на type-level

## Примеры типизации

```rust
// Type constructors (type-level функции)
Vec : fn(Type) -> Type : Type₁
Option : fn(Type) -> Type : Type₁
Map : fn(Type, Type) -> Type : Type₁

// HKT
Functor : fn(fn(Type) -> Type) -> Constraint : Constraint₂

// Constraints
Eq : fn(Type) -> Constraint : Constraint₁
Monoid : fn(Type) -> Constraint : Constraint₁

// Effects
IO : Effect₀ : Effect₁
State : fn(Type) -> Effect : Effect₁

// RefScopes (специальный синтаксис с ')
'global : RefScope₀ : RefScope₁  // builtin
'const : RefScope₀ : RefScope₁   // builtin
Scope : fn(RefScope₀) -> Effect₀ : Effect₁  // конструктор для использования в effect rows

// Quantified constraints (first-class)
<X :< FromJSON> FromJSON<F<X>> : Constraint₁

// Type alias для quantified constraint
type FromJSON1<F<_>> = <X :< FromJSON> FromJSON<F<X>>
FromJSON1 : fn(fn(Type) -> Type) -> Constraint : Constraint₁

// Полиморфные функции (value-level с forall, predicative)
identity : <A> fn(A) -> A : Type₁              // квантификация поднимает в Type₁
pair : <A, B> fn(A, B) -> (A, B) : Type₁
map : <A, B> fn(Vec<A>, fn(A) -> B) -> Vec<B> : Type₁
```

## Синтаксис функциональных типов

### Различие между forall и type constructors

**Forall (универсальный квантор):**
```rust
<A> Type           // для всех A, возвращает Type
<A> fn(A) -> A     // полиморфная функция (identity)
```

**Type constructors (type-level функции):**
```rust
fn(Type) -> Type              // type constructor (Vec, Option)
fn(fn(Type) -> Type) -> Constraint  // HKT constructor (Functor)
```

**Value-level функции:**
```rust
fn(i32, i32) -> i32     // обычная функция
fn() -> @IO ()        // IO action без аргументов
```

### Ключевые правила

1. **`<...>`** используется только для forall (универсальной квантификации)
2. **`fn(...) -> ...`** используется для всех функций (type-level и value-level)
3. **Различие type-level vs value-level** определяется во время elaboration:
   - В Surface AST: `fn(A) -> B` парсится одинаково
   - Различие определяется через:
     - Позицию в синтаксисе (где ожидается type vs value)
     - Type inference (проверка что A и B - вселенные или типы)
   - В Core AST: явно различаются как разные конструкторы

### Полная форма с явными sorts и constraints

```rust
<X: Type0, E: Effect0> fn(X) -> @E X where { Eq<X>, Show<X> }
```

### Синтаксис constraints

В `<>` используется синтаксис `:` для указания sort и `:<` для указания constraints:

```rust
// Короткий синтаксис в <>
<A :< Eq> fn(A, A) -> Bool

// Длинный синтаксис с where
<A> fn(A, A) -> Bool where { Eq<A> }

// Смешанный
<A :< Eq> fn(A) -> A where { Show<A> }

// Явное указание sort
<A: Type0> fn(A) -> A
<F: fn(Type) -> Type> fn(F<i32>) -> F<String>
```

### Корректность формы where

В `where { ... }` каждый элемент должен быть насыщённым (fully-applied) выражением вселенной `Constraintₙ` и не быть функциональным типом на верхнем уровне.

Допустимые формы:
- Прямое применение трейта: `C<T1, ..., Tm>`
- Применение алиаса/конструктора ограничения: `Alias<...>`
- Квантифицированные ограничения: `<A: ...> D<...>`

Недопустимые формы:
- «Сырые» конструкторы без аргументов: `C`, `Alias`
- Функции/конструкторы предикатов: `fn(...) -> Constraint` (на верхнем уровне)

Примеры (ok):
```rust
where { Functor<F> }
where { Eq<X>, Show<X> }
where { FromJSON1<F> }                    // alias applied
where { <A: Type0> Coercible<F<A>> }      // quantified constraint
where { CC<X> }                           // if CC: fn(Type) -> Constraint
```

Примеры (ошибка):
```rust
where { Functor }                         // constructor, not a predicate
where { FromJSON1 }                       // alias not applied
where { fn(fn(Type) -> Type) -> Constraint } // function, not a predicate
```

Уровни:
- В `where` допустимы `Constraintₙ` любых уровней (в т.ч. за счёт квантификации).
- Общий уровень типа функции поднимается с учётом уровней всех ограничений из `where` по общим правилам.

Правила умолчания для sort в параметрах `<>`:
- Если sort не указан, по умолчанию используется `Type`.
- Для идентификаторов с префиксом `'` (RefScope) sort равен `RefScope`.

### Локальные type-алиасы в where

В блоке `where { ... }` вместе с ограничениями можно объявлять локальные алиасы, которые используются во всей сигнатуре функции (в том числе в аргументах, результате, effect row и самих ограничениях). Алиасы видимы вперёд (forward reference).

Синтаксис объявлений внутри `where`:
```rust
where {
  type Pair<X, Y> = (X, Y),
  type FX<F<_>> = F<X>,                // alias к Constraint, если F: fn(Type)->Constraint
  type MyEff = State<S>,               // alias к Effect
  Eq<Pair<A, B>>,                      // насыщённые предикаты остаются допустимы
  CC<X>
}
```

Правила:
- Алиас может принадлежать любой вселенной: `Type`, `Constraint`, `Effect` (и может использоваться там, где ожидается соответствующая вселенная).
- Параметры алиаса задаются как в общих правилах `<>` (сорт по умолчанию `Type`, для `'a` — `RefScope`; HKT-параметры через `F<_>`).
- Алиасы в `where` не рекурсивны (запрещены прямые и взаимные рекурсивные ссылки между локальными алиасами).
- Локальные алиасы могут затенять глобальные имена/алиасы в своей области.
- В блоке `where` допускается смешивание деклараций алиасов и насыщённых предикатов, элементы разделяются запятой.

Примеры использования:
```rust
<A, B> fn(a: A, b: B) -> Pair<A, B>
  where {
    type Pair<X, Y> = (X, Y),
    Eq<Pair<A, B>>,
    Show<Pair<A, B>>
  }

<X, S> fn(x: X) -> @[MyEff, ..] ()
  where {
    type MyEff = State<S>
  }

<F<_>, X> fn(x: X) -> X
  where {
    type FX<F<_>> = F<X>,          // Constraint alias
    FX<FromJSON>                   // использован как предикат
  }
```

Ошибки:
```rust
where { type T = T }                // рекурсия запрещена
where { type E = IO, E<X> }         // если E — Effect, то E<X> недопустимо (Effect не функция)
```

### HKT параметры

Для Higher-Kinded Types используется специальный синтаксис `F<_>`:

```rust
// HKT параметр (по умолчанию _ это Type)
<F<_>> fn(F<i32>) -> F<String>

// С явным указанием universe параметра
<F<_: Effect>> fn(F) -> F

// Вложенные HKT
<G<_<_>>> fn(G<Option>) -> G<Vec>

// Эквивалентные записи с явным sort
<F<_>> Type  ≡  <F: fn(Type) -> Type> Type
<F<_: Effect>> Type  ≡  <F: fn(Effect) -> Type> Type
```

### Effect syntax

```rust
// Конкретные эффекты
fn(A) -> @[IO] A
fn(A) -> @[IO, State<S>] A

// Переменная эффекта (shorthand)
fn(A) -> @A  ≡  <E: Effect> fn(A) -> @E A

// Открытый список эффектов (shorthand)
fn(A) -> @[IO, ..] A  ≡  <E: Effect> fn(A) -> @[IO, E] A

// Явная форма с эффектом как параметром
<E: Effect> fn(A) -> @E A
<R: Effect> fn(A) -> @[IO, State<S>, R] A
```

### Effect rows: сахар и нормализация

Сахар и развёртывание:
- `'s` в effect row эквивалентно `Scope<'s>`: `@['s, IO] ≡ @[Scope<'s>, IO]`.
- Внутри объявления `effect E { ... }` действует правило: `@T ≡ @[Self, ..] T`.
- Шортхенд переменной эффекта вне `effect`: `@A ≡ <E: Effect> @E A`.

Канонизация effect row:
1. Развернуть сахар (`'s` → `Scope<'s>`, внутреннее `@T` → `@[Self, ..] T`).
2. Превратить список эффектов в множество атомов плюс, опционально, хвост `..ρ`.
3. Удалить дубликаты атомов.
4. Выполнить редукцию по наследованию: если `A :< B`, то `B` избыточен и удаляется.
5. Отсортировать атомы стабильным лексикографическим порядком по развёрнутому виду.
6. Эквивалентность строк эффектов определяется их канонической формой.

### Лексический выбор хэндлера и ключ мономорфизации (спецификация)

- Выбор хэндлера делается лексически во время elaboration: операция `E.op`
  резолвится к хэндлеру, предоставленному ближайшим `handle/impose/interpose`.
- Внутренняя компиляция передаёт окружение хэндлеров как неявный аргумент; вызовы `E.op`
  компилируются в прямые обращения к соответствующим слотам `Handler<E>` без рантайм-поиска.
- Совместимость строк эффектов проверяется через entailment; переменные effect row непрозрачны.
- Ключ мономорфизации (в будущем): каноническая строка эффектов `@[A1,..,An]` после
  дедупликации и редукции по наследованию служит ключом специализации.

### Именованные инстансы эффектов

- В строке эффектов допускаются именованные экземпляры одного и того же эффекта:
  `@[s1: E<T>, s2: E<T>]` — сахар для `E<T>#s1`, `E<T>#s2`.
- Канонизация: дубликаты удаляются только для идентичных атомов (одно и то же имя экземпляра);
  разные имена не схлопываются.
- Разрешение вызовов:
  - Квалифицированный вызов `s1::op(...)` однозначно обращается к экземпляру `E#s1`.
  - Неквалифицированный вызов `op(...)` допустим, если в области видимости ровно один экземпляр `E`.
    Иначе — ошибка разрешения, требуется квалификация.
- Теневание лексическое: ближайший экземпляр скрывает внешние для неквалифицированных вызовов;
  квалификация обходит теневание.
- Наследование/entailment применяются к экземплярам: `E#i :< B` означает, что присутствие `E#i`
  удовлетворяет требованию `B` через проекцию словаря `Handler<E> -> Handler<B>`.

### Параметры-инстансы эффектов и вывод

- В сигнатуре функции можно объявлять параметр-инстанс `a: E<T>` — это не term-level значение,
  а биндер для выбранной инстанции эффекта. Он используется в effect row как имя (`@[a: E<T>, ..]`)
  и в квалифицированных вызовах (`a::op(...)`). При elaboration стирается и передаётся как слот `Handler<E>`.
- Вызов функции:
  - `f(s1, ...)` — явная привязка `a = s1`.
  - `f(_, ...)` — вывод: допустим только если в текущем лексическом скоупе ровно одна видимая инстанция `E<T>`.
    Иначе — ошибка неоднозначности, требуется явное имя.
- Снятие конкретной инстансы внутри функции (если есть несколько в row) требует квалификации по имени
  в соответствующем операторе (например, `interpret[b](...)`).

### Привязка инстансов при вызове: `with { ... }`

- Синтаксис: `f(args) with { s1 = outer1, s2 = outer2, State<S> = s }`
- Правила:
  - Слева допустимы:
    - имена инстансов из сигнатуры функции (например, `s1: E<T>` → `s1 = name`),
    - типизированные ключи для неименованных требований (`E<T> = name`).
  - Порядок вызова: `f::<TArgs>(positionalArgs) with { ... }`.
  - Лишние/пропущенные привязки — ошибка; каждая требуемая инстанция должна быть связана.
  - Блок `with` стирается при elaboration и превращается в передачу соответствующих словарей хэндлеров.

Совместимость при использовании:
- `@[A1, .., An, ..σ]` совместим с требуемым `@[B1, .., Bk, ..ρ]`, если для каждого `Bi` есть `Aj` такой, что `Aj = Bi` или `Aj :< Bi`. Если у требуемого row нет хвоста `..ρ`, лишних атомов быть не должно.

### RefScope syntax

RefScope идентифицируется префиксом `'` и используется для scoped mutability:

```rust
// RefScope параметры
<'s> Type              // scope параметр
<'s, 's2> Type         // несколько scopes

// Ссылки со scope
&'s Type               // иммутабельная ссылка в scope 's
&'s mut Type           // мутабельная ссылка в scope 's

// Функции со scopes
<'s, A: Ord> fn(&'s mut Vec<A>) -> @['s] ()

// Вложенные scopes
<'outer, A, X> fn(
  mutex: Mutex<A>,
  f: <'inner> fn(&'inner mut A) -> @['outer, 'inner, IO, ..] X
) -> @['outer, IO, ..] X

// Builtin scopes
'global : RefScope₀    // глобальный scope, IO :< Scope<'global>
'const : RefScope₀     // scope для иммутабельных объектов, чистый код

// RefScope как Effect (автоматическое преобразование)
@['s, IO]  ≡  @[Scope<'s>, IO]

// RefScope в результате type-level функций
trait ExtractScope<T> {
  type Scope : RefScope₀
}

type ScopeOf<T> = ExtractScope<T>::Scope : fn(Type₀) -> RefScope₀ : RefScope₁
```

### Bottom type

```rust
// Bottom type
! : Type0

// Elimination rule:
// A value of type `!` can be coerced to any `T : Type_n`
```

### Builtin RefScopes

**`'global`** — глобальный scope всей программы:
- Используется для глобальных мутабельных ссылок
- `IO :< Scope<'global>` (IO наследует от Scope<'global>)
- Доступ к `&'global mut` требует `@[IO]` или `@['global]`

**`'const`** — scope для иммутабельных ссылок:
- Ссылки `&'const T` можно разыменовывать в любом scope
- Чистый код может работать с `&'const T` без эффектов
- `const` поля в структурах неявно используют `'const`

### Erasure: RefScope are type-only

RefScope существуют только на уровне типов и полностью стираются при компиляции:

```rust
// No runtime representation for RefScope
<'s> fn(&'s T) -> U   // `'s` is a phantom in types, not a value
@['s, IO]             // sugar for @[Scope<'s>, IO] is elaboration-time only
```

Безопасность обеспечивается исключительно типчекером (фантомные параметры, ограничения в сигнатурах); рантайм не содержит «меток» RefScope.

### Общая форма функционального типа

```rust
<'s, X: Type0, E: Effect0, CC: fn(Type) -> Constraint> fn(&'s Person<X>) -> @['s, E] Person<X>
  where { Eq<X>, Show<X>, CC<X> }
```

## Universe полиморфизм

Universe уровни могут быть полиморфными и выводиться автоматически:

```rust
// В коде пишем без явного уровня
fn(Type) -> Type

// Система выводит полиморфный тип, работающий на любом уровне:
// fn(Type_n) -> Type_n : Type_(n+1) для любого n
```

Вывод уровней происходит во время type checking.

## Quantified Constraints

Quantified constraints являются first-class сущностями:

```rust
// Quantified constraint
<A :< Coercible> Coercible<F<A>> : Constraint₁

// Можно создавать алиасы
type FromJSON1<F<_>> = <X :< FromJSON> FromJSON<F<X>>

// Использование
trait Traversable<F<_>> where {
  Functor<F>,
  <A :< FromJSON> FromJSON<F<A>>  // quantified constraint в where
}
```

Вложенная форма with where внутри:
```rust
<A> Coercible<F<A>> where { Coercible<A> }
// эквивалентно
<A :< Coercible> Coercible<F<A>>
```

## Наследование Constraints и Effects

Constraints и Effects могут наследоваться друг от друга используя синтаксис `=>`.

### Наследование Constraints

```rust
// Constraint наследует другие constraints
trait Ord<A> => Eq<A> {
  fn compare(a: A, b: A) -> Ordering
}

// Ord : fn(Type) -> Constraint : Constraint₁
// где для любого T: если Ord<T> то автоматически Eq<T>

// Множественное наследование
trait Show<A> => Display<A>, Debug<A> {
  fn show(a: A) -> String
}
```

### Почему в Flow — предикативные universes (в отличие от Haskell)

- **Мотивация**
  - Чёткая стратификация уровней исключает парадоксы (Girard) и упрощает формальные гарантии при наличии HKT, quantified constraints, эффектов и `RefScope`.
  - Улучшает phase distinction: исключает неявные «инъекции» `Constraint`/`Effect`/`RefScope` в `Type`, делает elaboration и нормализацию предсказуемыми.
  - Проще канонизация и субтайпинг effect rows (редукция по `:<`, дедупликация) — без смешения вселенных.
  - Локализует «поднятие уровней»: квантификация даёт +1, что делает уровни явными и контролируемыми.

- **Компромиссы**
  - Иногда требуется больше аннотаций (уровень/сорт), т.к. квантификация предикативна и поднимает уровень.
  - Некоторые «удобные» идиомы impredicative полиморфизма (в духе свободного применения конструкторов ограничений) запрещены; требуют насыщения аргументами или явной квантификации.

- **Практические следствия в Flow**
  - **Quantified constraints**: `<A> D<...>` имеют `Constraintₙ` с подъёмом уровня; в `where` разрешены только насыщённые предикаты (не функции вида `fn(...) -> Constraint`).
  - **HKT и sorts**: `fn(Type) -> Type`, `fn(Effect) -> Type` и т.д. — сорт обязателен или выводится по правилам (по умолчанию `Type`, для `'a` — `RefScope`).
  - **Tuples**: элементы только из одной вселенной; нет смешения `Constraint`/`Effect` в `Type`.
  - **Effects**: `RefScope` → `Effect` через `Scope<'s>`; строки эффектов нормализуются и сравниваются по канонической форме.

- **Контраст с Haskell**
  - Haskell сохраняет impredicative вселенную (`Type :: Type`) ради совместимости и эргономики вывода; Dependent Haskell минимизирует стратификацию.
  - Flow выбирает предикативность для ясной семантики и управляемости при совмещении HKT, эффектов, `RefScope` и first-class ограничений.

### Наследование Effects

```rust
// Effect наследует другие effects
effect IO => Scope<'global> {
  // IO операции требуют доступа к глобальному scope
  op println(s: String) -> @()
}

// Effect с параметрами наследует базовый effect
effect State<S> => Stateful {
  op get() -> @S
  op put(s: S) -> @()
}

// Множественное наследование
effect IO => FileIO, Error<IOError> {
  op readFile(path: String) -> @String
}
// Global inheritance relation implied by the design
IO => Error<IOError>, AsyncError
```

### Наследование в type checking

Когда `A => B`, то:
- Для Constraint: если требуется `B<T>`, можно предоставить `A<T>`
- Для Effect: код с эффектом `@[A]` может вызываться где ожидается `@[B, ..]`

```rust
// Пример с constraints
fn f<A :< Ord>(a: A, b: A) -> Bool {
  a == b  // OK: Ord<A> => Eq<A>, поэтому A автоматически Eq
}

// Пример с effects
fn doIO() -> @[IO] () {
  doGlobalMutation().do // OK: IO => Scope<'global>
}

fn doGlobalMutation() -> @['global] () { ... }
```

### Правила наследования

1. **Транзитивность:** Если `A => B` и `B => C`, то `A => C`
2. **Рефлексивность:** `A => A` всегда истинно
3. **Параметры:** Наследование инвариантно к параметрам:
   - `State<S> => Stateful` НЕ означает `State<T> => Stateful` для произвольного T
   - Параметры должны совпадать точно

## Структура AST

### Два уровня представления

1. **Surface AST** — поверхностный синтаксис, результат парсинга
   - Сохраняет синтаксический сахар
   - Неполная информация о типах
   - Не различает type-level и value-level функции синтаксически
   - Близок к коду пользователя

2. **Core AST** — десахаренное представление после elaboration
   - Все shorthands развёрнуты
   - Все типы явно аннотированы
   - Уровни вселенных выведены
   - Type-level и value-level функции явно различаются
   - Упрощённая структура для type checking и codegen

**Elaboration (Surface → Core):**
- Разрешение имён и scope resolution
- Desugaring синтаксического сахара
- Type inference и определение type-level vs value-level
- Вывод universe уровней
- Развёртывание constraints

### Примеры трансформаций Surface → Core

```rust
// Surface: fn(A) -> @A
// Core: <E: Effect₀> fn(A) -> @E A

// Surface: fn(Type) -> Type
// Core: fn(Type_n) -> Type_n : Type_(n+1)  // с переменной уровня n

// Surface: <A :< Eq> fn(A) -> A
// Core: <A: Type₀> fn(A) -> A where { Eq<A> }

// Surface: <F<_>> Type
// Core: <F: fn(Type₀) -> Type₀> Type
```
