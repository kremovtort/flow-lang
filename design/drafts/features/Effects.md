## Effects specification: principles

- Lexical handler selection: operations resolve to the nearest handler in source, no runtime search.
- Inheritance `A => B` is entailment: rows with `A` satisfy requirements for `B`; no automatic method lifting.
- Effect rows: canonicalize (desugar, set semantics, remove supers via `=>`, sort); compatibility via entailment.
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
  fn <X, R: EffectRow>(act: fn() -> @[..R] X) -> @['global, ..R] Fiber<X>
    where { FiberSafe<R> }
  ```
  FiberSafe<R> holds if every effect in R is fiber-safe (e.g., `=> Scope<'global>` or scope-agnostic).

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
  action: fn() -> @[Reader<R>, State<S>, Writer<W>, ..] X,
) -> @[..] ((X, S), W) {
  interpret(
    action,
    handle { // returning<Y> ((Y, S), W)
      // Writer using shared writerCell
      Writer<W> returning<Y> (Y, W) {
        let mut writerCell = DelimitedCell::new(Monoid::empty());

        op tell(w: W) { writerCell.set(writerCell.get() ++ w) }

        op listen<Y, R>(sub: fn() -> @R Y) -> @R (Y, W) {
          let parent = writerCell.get();
          writerCell.pushLocal(Monoid::empty());
          let y = sub();
          let local = writerCell.get();
          writerCell.popLocal();
          writerCell.set(parent ++ local);
          (y, local)
        }

        op censor<Y, R>(sub: fn() -> @R Y, f: fn(W) -> W) -> @R Y {
          let parent = writerCell.get();
          writerCell.pushLocal(Monoid::empty());
          let y = sub();
          let local = writerCell.get();
          writerCell.popLocal();
          writerCell.set(parent ++ f(local));
          y
        }

        op pass<Y, R>(sub: fn() -> @R (Y, fn(W) -> W)) -> @R Y {
          let parent = writerCell.get();
          writerCell.pushLocal(Monoid::empty());
          let (y, k) = sub();
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
        op local<Y, ER>(f: fn(R) -> R, sub: fn() -> @ER Y) -> @ER Y {
          envCell.pushLocal(f(envCell.get()));
          let y = sub();
          envCell.popLocal();
          y
        }
      }
    },
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

### Built-in effects for delimited control (op-scope only)

These effects are implicitly in scope inside any `op` body of a handler. They are not available outside `op`.

```rust
// Abort with payload E
pub effect Abort<E> {
  pub op recover<X, R>(action: fn() -> @R X, handler: fn(E) -> @R X) -> @R X
  pub op abort(E) -> @!
}

// One-shot prompt tied to the op-local scope 's and the fully applied carrier Ret
// SingleShot<Self> marks that multiple resumes are not allowed; a second resume panics at runtime.
pub effect Prompt1<'s, Ret> => 's where { SingleShot<Self> } {
  pub op control0<A, R>(k: fn(
    reenter: fn(fn() -> @R Ret) -> @R Ret,
    resume:  fn(A) -> @R Ret
  ) -> @R Ret) -> @R !

  pub op control1<A>(k: fn(
    resume: fn(A) -> @R Ret
  ) -> @R Ret) -> @R !

  pub op shift1<A>(k: fn(
    resume: fn(A) -> @R Ret
  ) -> @R Ret) -> @!
}

// Multi-shot prompt tied to the op-local scope 's and the fully applied carrier Ret
pub effect ControlN<'s, Ret> => 's {
  pub op control0N<A, R>(k: fn(
    reenter: fn(fn() -> @R Ret) -> @R Ret,
    resume:  fn(A) -> @R Ret
  ) -> @R Ret) -> @R !

  pub op controlN<A, R>(k: fn(
    resume: fn(A) -> @R Ret
  ) -> @R Ret) -> @R !

  pub op shiftN<A, R>(k: fn(
    resume: fn(A) -> @R Ret
  ) -> @R Ret) -> @R !
}

// Optional aliases (sugar):
// control  ≡ controlN
// shift    ≡ shiftN
// control0 ≡ control1
```

```rust
pub effect ConsoleOutput {
  pub op print(str: string) -> @()
}

pub effect Reader<A> {
  pub op ask() -> @A
  pub op local<X, R>(f: fn(A) -> A, action: fn() -> @R X) -> @R X
}


pub effect State<S> {
  pub op get() -> @S
  pub op put(S) -> @()
}

pub effect Writer<W> {
  pub op tell(W) -> @()
  pub op listen<X>(fn() -> X) -> (X, W)
  pub op censor<X>(fn() -> X, fn(W) -> W) -> X
  pub op pass<X>(fn() -> (X, fn(W) -> W)) -> X
}

pub effect Error<E> {
  pub op throw(E) -> @!
  pub op <X, R>(fn() -> @R X) catch(fn(E) -> @R X) -> @R X
}

fn runState<S, X, R>(initial: S, action: fn() -> @[State<S>, ..R] X) -> @R (X, S) {
  interpret(<Y> handle State<S> returning (Y, S) {
    // Note: composition with NonDet requires per-branch snapshots.
    // Aggregation of branch states (if needed) should be expressed explicitly
    // at the call site or via a higher-level combinator, not inside this handler.

    let mut state = initial;

    op get() -> S {
      state
    }

    op put(s: S) {
      state = s;
    }

    returning (y: Y) -> (Y, S) {
      (y, state)
    }
  }, action)
}

fn runReader<R, X, E>(env: R, action: fn() -> @[Reader<R>, ..E] X) -> @E X {
  let handler = |e| handle Reader<R> {
    op ask() -> R {
      e
    }

    op local<Y, Es>(f: fn(R) -> R, action: fn() -> @Es Y) -> @Es Y {
      // replaceInterpreter takes a function that accepts the old effects environment,
      // which can be used to call effects from the previous environment (with the old interpreter)
      action.replaceInterpreter(|_oldEffectsEnv| handler(f(e)))
    }
  };

  interpret(handler(env), action)
}
// Note: this stackless variant is simple but problematic in multi-shot settings:
// local rebinds are not automatically restored on resume; prefer DelimitedStack below.

// Alternative stack-based interpreter. DelimitedStack is a runtime structure capturing
// the current environment as a stack to support nested locals and multi-shot control.
// API sketch: DelimitedStack::new(R) -> DelimitedStack<R>, top() -> R, push(R) -> (), pop() -> ()
fn runReaderWithDelimitedStack<R, X, E>(env: R, action: fn() -> @[Reader<R>, ..E] X) -> @E X {
  let handler = handle Reader<R> {
    // Create DelimitedStack inside the handler, making it handler-local.
    // For multi-shot control, the captured continuation must snapshot this state.
    let mut delimitedStack = DelimitedStack::new(env);
    op ask() -> R {
      delimitedStack.top()
    }

    op local<Y, E>(f: fn(R) -> R, action: fn() -> @E Y) -> @E Y {
      // Push new environment; for multi-shot, the continuation must capture
      // the DelimitedStack snapshot so that each resume sees a consistent top.
      delimitedStack.push(f(delimitedStack.top()));
      let y = action();
      delimitedStack.pop();
      y
    }
  };

  interpret(handler, action)
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
fn runReaderWithDelimitedCell<R, X, E>(env: R, action: fn() -> @[Reader<R>, ..E] X) -> @E X {
  let handler = handle Reader<R> {
    let mut cell = DelimitedCell::new(env);

    op ask() -> R {
      cell.get()
    }

    op local<Y, E>(f: fn(R) -> R, action: fn() -> @E Y) -> @E Y {
      cell.pushLocal(f(cell.get()));
      let y = action();
      cell.popLocal();
      y
    }
  };

  interpret(handler, action)
}

// Canonical Writer on top of DelimitedCell (requires Monoid<W>; ++ is Monoid::append)
fn runWriter<W, X, E>(action: fn() -> @[Writer<W>, ..E] X) -> @E (X, W) {
  let handler = handle Writer<W> {
    let mut cell = DelimitedCell::new(Monoid::empty());

    op tell(w: W) -> () {
      cell.set(cell.get() ++ w);
    }

    op listen<Y>(sub: fn() -> Y) -> (Y, W) {
      let parent = cell.get();
      cell.pushLocal(Monoid::empty());
      let y = sub();
      let local = cell.get();
      cell.popLocal();
      cell.set(parent ++ local);
      (y, local)
    }

    op censor<Y>(sub: fn() -> Y, f: fn(W) -> W) -> Y {
      let parent = cell.get();
      cell.pushLocal(Monoid::empty());
      let y = sub();
      let local = cell.get();
      cell.popLocal();
      cell.set(parent ++ f(local));
      y
    }

    op pass<Y>(sub: fn() -> (Y, fn(W) -> W)) -> Y {
      let parent = cell.get();
      cell.pushLocal(Monoid::empty());
      let pair = sub();
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

  interpret(handler, action)
}

// State on top of DelimitedCell with per-branch snapshots
fn runStateDelimited<S, X, E>(initial: S, action: fn() -> @[State<S>, ..E] X) -> @E (X, S) {
  let handler = handle State<S> {
    let mut cell = DelimitedCell::new(initial);

    op get() -> S { cell.get() }

    op put(s: S) -> () { cell.set(s) }

    returning (y: Y) -> (Y, S) { (y, cell.get()) }
  };

  interpret(handler, action)
}

// Resource effect sketch (bracket) with two policies
pub effect Resource {
  pub op bracket<A, B>(fn() -> A, fn(A) -> fn() -> B, fn(A) -> fn() -> ()) -> fn() -> B
}

// Handler-local resource env carried by DelimitedCell; runtime ensures finalizers
// run on normal completion, abort or async cancellation. Each policy defines how
// capture/resume/spawn interact with resource handles.
// Note: safety against handle leaks is enforced by types only (phantom RefScope);
// there is no runtime RefScope metadata.

// Scoped policy: share handle across delimited branches within a fiber;
// finalize when last branch exits; do not transfer on spawn.
fn runResourceScoped<X, E>(action: fn() -> @[Resource, ..E] X) -> @E X {
  let handler = handle Resource {
    let mut env = DelimitedCell::new(ResourceEnv::empty());

    op bracket<A, B>(acq: fn() -> A, useWith: fn(A) -> fn() -> B, rel: fn(A) -> fn() -> ()) -> fn() -> B {
      env.pushLocal(env.get().beginScope());
      let a = acq();
      env.set(env.get().registerFinalizer(|| rel(a)()));
      let b = useWith(a);
      env.set(env.get().runFinalizers());
      env.popLocal();
      b
    }

    returning (y: Y) -> Y { y }
  };

  interpret(handler, action)
}

// Per-fiber policy: branches in the same fiber share; new fibers acquire independently;
// finalize on fiber exit for fiber-owned handles.
fn runResourcePerFiber<X, E>(action: fn() -> @[Resource, ..E] X) -> @E X {
  let handler = handle Resource {
    let mut env = DelimitedCell::new(ResourceEnv::emptyPerFiber());

    op bracket<A, B>(acq: fn() -> A, useWith: fn(A) -> fn() -> B, rel: fn(A) -> fn() -> ()) -> fn() -> B {
      env.pushLocal(env.get().beginFiberScope());
      let a = acq();
      env.set(env.get().registerFiberFinalizer(|| rel(a)()));
      let b = useWith(a);
      env.set(env.get().runFiberFinalizersIfNeeded());
      env.popLocal();
      b
    }

    returning (y: Y) -> Y { y }
  };

  interpret(handler, action)
}

fn runError<E, X, R>(action: fn() -> @[Error<E>, ..R] X) -> @R Result<E, X> {
  let handler = handle Error<E> {
    op throw(e: E) {
      abort(Err(e));
	  }

	  op <Y, R>(action1: fn() -> @R Y) catch(errorHandler: fn(E) -> @R Y) -> @R Y {
      recover(action1(), |res| match res {
        Err(e) => errorHandler(e),
        Ok(a) => a,
      })
	  }

	  returning (y: Y) -> Result<E, Y> {
      Ok(y)
	  }
	}
  ;

  interpret(handler, action)
}
```

```rust
enum List<A> {
  Nil,
  Cons { head: A, tail: &List<A> },
}

pub effect NonDet {
  op empty() -> !
  op choose<F<_> :< Foldable, X>(F<X>) -> X
}

// depth-first non-determinism
fn runNonDetListDF<A, E>(action: fn() -> @[NonDet, ..E] A) -> @E Stream<A> {
  let handler = handle NonDet {
    op empty() -> ! {
      abort(Stream::empty())
    }

    op choose<F<_> :< Foldable, X>(elems: F<X>) -> X {
      // delimited control; we also need control and control0,
      // think how to ensure type-safety of their usage
      shift(|resume| {
        let mut res = Stream::empty();
        for elem in elems.iterator() {
          res = res ++ resume(elem);
        }
        res
      })
    }

    returning<Y>(y: Y) -> Stream<Y> {
      Stream::singleton(y)
    }
  }

  interpret(handler, action)
}

// breadth-first non-determinism
fn runNonDetListBF<A, E>(action: fn() -> @[NonDet, ..E] A) -> @E Stream<A> {
  // Breadth-first via explicit queue using control to capture continuation.
  let handler = handle NonDet returning<X> Stream<X> {
    // BFS queue of pending branches; each element is a thunk producing Stream<X>
    let mut queue: Queue<fn() -> @[Self] Stream<X>> = Queue::empty();

    op empty() -> @! { abort(Stream::empty()) }

    op choose<F<_> :< Foldable, X>(elems: F<X>) -> X {
      // Capture the current continuation and enqueue all choices
      control(|resume| {
        for elem in elems.iterator() {
          queue.push(resume(elem));
        }
        // Dequeue in FIFO order and concatenate lazily
        let mut acc = Stream::empty();
        while let Some(k) = queue.pop() {
          acc = acc ++ k();
        }
        acc
      })
    }

    returning<Y>(y: Y) -> Stream<Y> { Stream::singleton(y) }
  };

  interpret(handler, action)
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
- Entailment and inheritance apply per instance: if `E#i => B`, then a row containing `E#i` satisfies
  a requirement for `B` via projection of `Handler<E>` to `Handler<B>`.
- impose/interpose may target either the nearest instance by effect type or a concrete named instance.

```rust
// Example with two State instances and qualified calls
fn <S>() -> @[s1: State<S>, s2: State<S>] (S, S) {
  let x = s1::get();
  let y = s2::get();
  (x, y)
}

// Instance-parameter and underscore inference at call-site
fn f(a: A, g: @[a, b: A] X) -> @[a] X {
  // handle only b; a remains in the row
  interpret(handler, g) with { b = b }
}

// explicit binding
f(s1, g)

// underscore inference: allowed only if exactly one visible A-instance exists
f(_, g)

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
  interpret(handler, g)
}

f(g)
```

```rust
fn f(a: A, g: @[a, b] X) -> @[a] X {
  interpret(handler, g)
}

f(_, g) // аргумент a тут выводится из доступных эффектов в лексическом контексте
```

## Static effects - эффекты, реализация которых известна на этапе компиляции, либо же эффекты, реализация которых не требуется (пустые эффекты)

```rust
pub effect Concurrency {}

mod Concurrency {
  fn spawn<Es :< FiberSafe>(f: fn() -> @Es ()) -> @Es () {
    // some magical staff here
  }
}
```
