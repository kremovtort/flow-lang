## Effects specification: principles

- Lexical handler selection: operations resolve to the nearest handler in source, no runtime search.
- Inheritance `A :< B` is entailment: rows with `A` satisfy requirements for `B`; no automatic method lifting.
- Effect rows: canonicalize (desugar, set semantics, remove supers via `:<`, sort); compatibility via entailment.
- Named instances and qualification: `@[s1: E<T>, s2: E<T>]`, calls `s1::op(...)`; unqualified only if unique.
- Instance parameters and `_` inference: `a: E<T>` is a binder (erased); `_` allowed only if exactly one `E<T>` visible.
- Call-site binding: `f(args) with { s1 = outer1, E<T> = name }` (erased to dictionary passing on elaboration).
- Monomorphization key (future): canonical effect row after reduction and deduplication.

### Runtime primitives used in this document

- DelimitedCell<T>: handler-local, snapshot/restore across delimited continuations; pushLocal/popLocal for lexical scopes; set/get for updates.
- DelimitedStack<R>: handler-local stack for Reader-like scoping; multi-shot safe via continuation snapshots.
- Stream<A>: abstract lazy stream with `empty`, `singleton`, `++`, and `decompose(): Option<(A, Stream<A>)>`.

### Delimited control syntax
- Spawn (fibers)

- Standard spawn (fiber-safe rows only):
  ```rust
  fn <X, R: EffectRow>(act: @[R] X) -> @['global] Fiber<X>
    where { FiberSafe<R> }
  ```
  FiberSafe<R> holds if every effect in R is fiber-safe (e.g., `:< Scope<'global>` or scope-agnostic).

## Handler blocks

Lexical handler blocks let you define one or more effect handlers that may share state.

Syntax:
```rust
handle in R {
  E1 returning C1 { /* ops for E1; handler-local state available here */ }
  E2 returning C2 { /* ops for E2; can call E1 ops and vice versa */ }
}
```

Rules:
- Each `Ei returning Ci` defines a handler with its own prompt and carrier `Ci<Y>`.
- The block runs in dependency row `R`; inside ops, code executes under `@[R, ..]`.
- Handlers in the same block may call each other; delimited control forms must be in tail-position
  with respect to their own effect.
- Optional per-effect dependency extension: `E in R1 returning C` means the handler depends on
  `R ∪ R1`.

### Example: shared-state multi-effect handler block

```rust
type Handler<
  Es: EffectRow, // effects that handler interprets
  Ds: EffectRow, // effects that handler depends on
  C: fn(Type) -> Type, // carrier type
>

type Compose<F<_>, G<_>> = <X> F<G<X>>;

fn runApp<R, S, W: Monoid, X>(
  env: R,
  init: S,
  action: @[Reader<R>, State<S>, Writer<W>, ..] X,
) -> @[..] ((X, S), W) {
  action.interpret(
    handle { // returning<Y> ((Y, S), W)
      // Writer using shared writerCell
      Writer<W> returning<Y> (Y, W) {
        let mut writerCell = DelimitedCell::new(Monoid::empty());

        op tell(w: W) -> @() { writerCell.set(writerCell.get() ++ w) }

        op listen<Y>(sub: @Y) -> @(Y, W) {
          let parent = writerCell.get();
          writerCell.pushLocal(Monoid::empty());
          let y = sub.do;
          let local = writerCell.get();
          writerCell.popLocal();
          writerCell.set(parent ++ local);
          (y, local)
        }

        op censor<Y>(sub: @Y, f: fn(W) -> W) -> @Y {
          let parent = writerCell.get();
          writerCell.pushLocal(Monoid::empty());
          let y = sub.do;
          let local = writerCell.get();
          writerCell.popLocal();
          writerCell.set(parent ++ f(local));
          y
        }

        op pass<Y>(sub: @(Y, fn(W) -> W)) -> @Y {
          let parent = writerCell.get();
          writerCell.pushLocal(Monoid::empty());
          let (y, k) = sub.do;
          let local = writerCell.get();
          writerCell.popLocal();
          writerCell.set(parent ++ k(local));
          y
        }

        returning (y: Y) -> (Y, W) { (y, writerCell.get()) }
      }

      // State using shared stateCell
      State<S> returning<Y> (Y, S) {
        let mut stateCell = DelimitedCell::new(init);

        op get() -> @S { stateCell.get() }
        op put(s: S) -> @() { stateCell.set(s) }

        returning (y: Y) -> (Y, S) { (y, stateCell.get()) }
      }

      // Reader using shared envCell
      Reader<R> {
        let mut envCell = DelimitedCell::new(env);

        op ask() -> @R { envCell.get() }
        op local<Y>(f: fn(R) -> R, sub: @Y) -> @Y {
          envCell.pushLocal(f(envCell.get()));
          let y = sub.do;
          envCell.popLocal();
          y
        }
      }
    }
  )
}
```

- Block forms inside `op` bodies:
  - `shift { resume => /* compute carrier */ }`
  - `control { resume => /* compute carrier */ }`
  - `control0 { resume => /* compute carrier */ }`
- Tail-position only: after these blocks, no further code in the `op` body executes.
- Typing: each form returns `@!` (non-returning); `.do` is not required.
- Semantics (short): `shift` captures up to handler prompt; `control` reinstalls continuation; `control0` does not.

```rust
pub effect ConsoleOutput {
  pub op print(str: string) -> @()
}

pub effect Reader<A> {
  pub op ask() -> @A
  pub op local<X>(f: fn(A) -> A, action: @X) -> @X
}


pub effect State<S> {
  pub op get() -> @S
  pub op put(S) -> @()
}

pub effect Writer<W> {
  pub op tell(W) -> @()
  pub op listen<X>(@X) -> @(X, W)
  pub op censor<X>(@X, fn(W) -> W) -> @X
  pub op pass<X>(@(X, fn(W) -> W)) -> @X
}

pub effect Error<E> {
  pub op throw(E) -> @!
  pub op <X>(@X) catch(E -> @X) -> @X
}

fn runState<S, X>(initial: S, action: @[State<S>, ..] X) -> @[..] (X, S) {
  action.interpret(<Y> handle State<S> returning (Y, S) {
    // Note: composition with NonDet requires per-branch snapshots.
    // Aggregation of branch states (if needed) should be expressed explicitly
    // at the call site or via a higher-level combinator, not inside this handler.

    let mut state = initial;

    op get() -> @S {
      state
    }

    op put(s: S) @{
      state = s;
    }

    returning (y: Y) -> (Y, S) {
      (y, state)
    }
  })
}

fn runReader<R, X>(env: R, action: @[Reader<R>, ..] X) -> @[..] X {
  let handler = |e| handle Reader<R> {
    op ask() -> @R {
      e
    }

    op local<Y>(f: fn(R) -> R, localAction: @Y) -> @Y {
      // replaceInterpreter takes a function that accepts the old effects environment,
      // which can be used to call effects from the previous environment (with the old interpreter)
      localAction.replaceInterpreter(|_oldEffectsEnv| handler(f(e))).do
    }
  };

  action.interpret(handler(env)).do
}
// Note: this stackless variant is simple but problematic in multi-shot settings:
// local rebinds are not automatically restored on resume; prefer DelimitedStack below.

// Alternative stack-based interpreter. DelimitedStack is a runtime structure capturing
// the current environment as a stack to support nested locals and multi-shot control.
// API sketch: DelimitedStack::new(R) -> DelimitedStack<R>, top() -> R, push(R) -> (), pop() -> ()
fn runReaderWithDelimitedStack<R, X>(env: R, action: @[Reader<R>, ..] X) -> @[..] X {
  let handler = handle Reader<R> {
    // Create DelimitedStack inside the handler, making it handler-local.
    // For multi-shot control, the captured continuation must snapshot this state.
    let mut delimitedStack = DelimitedStack::new(env);
    op ask() -> @R {
      delimitedStack.top()
    }

    op local<Y>(f: fn(R) -> R, localAction: @Y) -> @Y {
      // Push new environment; for multi-shot, the continuation must capture
      // the DelimitedStack snapshot so that each resume sees a consistent top.
      delimitedStack.push(f(delimitedStack.top()));
      let y = localAction.do;
      delimitedStack.pop();
      y
    }
  };

  action.interpret(handler).do
}

// Runtime primitive: DelimitedCell
// Concept: handler-local cell with snapshot/restore across delimited continuations.
// Guarantees:
// - Multi-shot safe: each resume sees an independent snapshot of the cell state
// - O(1) push/pop local scopes; copy-on-write on set/modify
// API sketch:
//   struct DelimitedCell<T> { /* opaque, runtime-provided */ }
//   impl<T> DelimitedCell<T> {
//     fn new(init: T) -> DelimitedCell<T>
//     fn get() -> @T
//     fn set(t: T) -> @()
//     fn pushLocal(t: T) -> @()   // begin lexical/delimited scope with value t
//     fn popLocal() -> @()        // end scope, restore previous snapshot
//   }

// Reader on top of DelimitedCell
fn runReaderWithDelimitedCell<R, X>(env: R, action: @[Reader<R>, ..] X) -> @[..] X {
  let handler = handle Reader<R> {
    let mut cell = DelimitedCell::new(env);

    op ask() -> @R {
      cell.get()
    }

    op local<Y>(f: fn(R) -> R, localAction: @Y) -> @Y {
      cell.pushLocal(f(cell.get()));
      let y = localAction.do;
      cell.popLocal();
      y
    }
  };

  action.interpret(handler).do
}

// Canonical Writer on top of DelimitedCell (requires Monoid<W>; ++ is Monoid::append)
fn runWriter<W, X>(action: @[Writer<W>, ..] X) -> @[..] (X, W) {
  let handler = handle Writer<W> {
    let mut cell = DelimitedCell::new(Monoid::empty());

    op tell(w: W) -> @() {
      cell.set(cell.get() ++ w);
    }

    op listen<Y>(sub: @Y) -> @(Y, W) {
      let parent = cell.get();
      cell.pushLocal(Monoid::empty());
      let y = sub.do;
      let local = cell.get();
      cell.popLocal();
      cell.set(parent ++ local);
      (y, local)
    }

    op censor<Y>(sub: @Y, f: fn(W) -> W) -> @Y {
      let parent = cell.get();
      cell.pushLocal(Monoid::empty());
      let y = sub.do;
      let local = cell.get();
      cell.popLocal();
      cell.set(parent ++ f(local));
      y
    }

    op pass<Y>(sub: @(Y, fn(W) -> W)) -> @Y {
      let parent = cell.get();
      cell.pushLocal(Monoid::empty());
      let pair = sub.do;
      let (y, k) = pair;
      let local = cell.get();
      cell.popLocal();
      cell.set(parent ++ k(local));
      y
    }

    returning (y: Y) -> (Y, W) {
      (y, cell.get())
    }
  };

  action.interpret(handler).do
}

// State on top of DelimitedCell with per-branch snapshots
fn runStateDelimited<S, X>(initial: S, action: @[State<S>, ..] X) -> @[..] (X, S) {
  let handler = handle State<S> {
    let mut cell = DelimitedCell::new(initial);

    op get() -> @S { cell.get() }

    op put(s: S) -> @() { cell.set(s) }

    returning (y: Y) -> (Y, S) { (y, cell.get()) }
  };

  action.interpret(handler).do
}

// Resource effect sketch (bracket) with two policies
pub effect Resource {
  pub op bracket<A, B>(@A, fn(A) -> @B, fn(A) -> @()) -> @B
}

// Handler-local resource env carried by DelimitedCell; runtime ensures finalizers
// run on normal completion, abort or async cancellation. Each policy defines how
// capture/resume/spawn interact with resource handles.
// Note: safety against handle leaks is enforced by types only (phantom RefScope);
// there is no runtime RefScope metadata.

// Scoped policy: share handle across delimited branches within a fiber;
// finalize when last branch exits; do not transfer on spawn.
fn runResourceScoped<X>(action: @[Resource, ..] X) -> @[..] X {
  let handler = handle Resource {
    let mut env = DelimitedCell::new(ResourceEnv::empty());

    op bracket<A, B>(acq: @A, useWith: fn(A) -> @B, rel: fn(A) -> @()) -> @B {
      env.pushLocal(env.get().beginScope());
      let a = acq.do;
      env.set(env.get().registerFinalizer(|| rel(a).do));
      let b = useWith(a).do;
      env.set(env.get().runFinalizers());
      env.popLocal();
      b
    }

    returning (y: Y) -> Y { y }
  };

  action.interpret(handler).do
}

// Per-fiber policy: branches in the same fiber share; new fibers acquire independently;
// finalize on fiber exit for fiber-owned handles.
fn runResourcePerFiber<X>(action: @[Resource, ..] X) -> @[..] X {
  let handler = handle Resource {
    let mut env = DelimitedCell::new(ResourceEnv::emptyPerFiber());

    op bracket<A, B>(acq: @A, useWith: fn(A) -> @B, rel: fn(A) -> @()) -> @B {
      env.pushLocal(env.get().beginFiberScope());
      let a = acq.do;
      env.set(env.get().registerFiberFinalizer(|| rel(a).do));
      let b = useWith(a).do;
      env.set(env.get().runFiberFinalizersIfNeeded());
      env.popLocal();
      b
    }

    returning (y: Y) -> Y { y }
  };

  action.interpret(handler).do
}

fn runError<E, X>(action: @[Error<E>, ..] X) -> @[..] Result<E, X> {
  let handler = handle Error<E> {
    op throw(e: E) @{
      abort(Err(e)).do;
	  }

	  op <Y>(action1: @Y) catch(errorHandler: E -> @Y) -> @Y {
      recover(action1, |res| match res {
        Err(e) => errorHandler(e).do,
        Ok(a) => a,
      }).do
	  }

	  returning (y: Y) -> Result<E, Y> {
      Ok(y)
	  }
	}
  ;

  action.interpret(handler).do
}
```

```rust
enum List<A> {
  Nil,
  Cons { head: A, tail: &List<A> },
}

pub effect NonDet {
  op empty() -> @!
  op choose<F<_> :< Foldable, X>(F<X>) -> @X
}

// depth-first non-determinism
fn runNonDetListDF<A>(action: @[NonDet, ..] A) -> @[..] Stream<A> {
  let handler = handle NonDet {
    op empty() -> @! {
      abort(Stream::empty()).do
    }

    op choose<F<_> :< Foldable, X>(elems: F<X>) -> @X {
      // delimited control; we also need control and control0,
      // think how to ensure type-safety of their usage
      shift { resume =>
        let mut res = Stream::empty();
        for elem in elems.iterator() {
          res = res ++ resume(elem).do;
        }
        res
      }
    }

    returning<Y>(y: Y) -> Stream<Y> {
      Stream::singleton(y)
    }
  }

  action.interpret(handler).do
}

// breadth-first non-determinism
fn runNonDetListBF<A>(action: @[NonDet, ..] A) -> @[..] Stream<A> {
  // Breadth-first via explicit queue using control to capture continuation.
  let handler = handle NonDet returning<X> Stream<X> {
    // BFS queue of pending branches; each element is a thunk producing Stream<X>
    let mut queue: Queue<@Stream<X>> = Queue::empty();

    op empty() -> @! { abort(Stream::empty()).do }

    op choose<F<_> :< Foldable, X>(elems: F<X>) -> @X {
      // Capture the current continuation and enqueue all choices
      control { resume =>
        for elem in elems.iterator() {
          queue.push(resume(elem));
        }
        // Dequeue in FIFO order and concatenate lazily
        let mut acc = Stream::empty();
        while let Some(k) = queue.pop() {
          acc = acc ++ k.do;
        }
        acc
      }
    }

    returning<Y>(y: Y) -> Stream<Y> { Stream::singleton(y) }
  };

  action.interpret(handler).do
}
```

// Handler selection and compilation model (spec excerpt)
// - Handler selection is lexical: an operation E.op resolves to the E-handler provided
//   by the nearest enclosing handle/impose/interpose in source structure, not by runtime search.
// - Elaboration passes the required handlers as an implicit environment Env<[E1,..,En]>,
//   and each E.op compiles to a direct call into the corresponding Handler<E> slot.
// - Effect row compatibility uses entailment only; effect rows remain opaque at runtime.
// - Monomorphization key (future work): the canonical effect row @[A1,..,An] after
//   deduplication and inheritance reduction serves as the specialization key.

## Named effect instances and qualification (spec excerpt)

- Effect atoms can be named to distinguish multiple instances of the same effect in scope.
- Syntax in effect rows: `@[s1: State<S>, s2: State<S>]` (sugar for `State<S>#s1`, `State<S>#s2`).
- Qualification in calls: `s1::get()` refers to the named instance; unqualified `get()` is allowed
  only if there is exactly one `State<_>` instance visible in the lexical scope.
- Canonicalization: duplicates are removed only for identical atoms (same effect and same instance name);
  different instances never collapse.
- Shadowing: the nearest (lexically) instance shadows outer ones for unqualified calls; qualified
  calls `s1::...` bypass shadowing.
- Entailment and inheritance apply per instance: if `E#i :< B`, then a row containing `E#i` satisfies
  a requirement for `B` via projection of `Handler<E>` to `Handler<B>`.
- impose/interpose may target either the nearest instance by effect type or a concrete named instance.

```rust
// Example with two State instances and qualified calls
fn <S>() -> @[s1: State<S>, s2: State<S>] (S, S) {
  let x = s1::get().do;
  let y = s2::get().do;
  (x, y)
}

// Instance-parameter and underscore inference at call-site
fn f(a: A, g: @[a, b: A] X) -> @[a] X {
  // handle only b; a remains in the row
  g.interpret(handler) with { b = b }.do
}

// explicit binding
f(s1, g).do

// underscore inference: allowed only if exactly one visible A-instance exists
f(_, g).do

// with-binding syntax for instances at call site
fn f<S>(...) -> @[s1: State<S>, s2: State<S>] ... { ... }

f(x, y) with {
  s1 = outer1,
  s2 = outer2,
}

// binding unnamed requirement via typed key
fn g<S>(...) -> @[State<S>] ... { ... }
g(z) with { State<S> = s1 }
```

```rust
pub effect Provider<E: Effect> {
  pub op <X>(@[E, ..] X) -> @[..] X
}
```

```rust
fn f(g: @[a: A, b: A] X) -> @[a] X {
  g.interpret(handler).do
}

f(g).do
```

```rust
fn f(a: A, g: @[a, b] X) -> @[a] X {
  g.interpret(handler)[b].do
}

f(_, g).do // аргумент a тут выводится из доступных эффектов в лексическом контексте
```
