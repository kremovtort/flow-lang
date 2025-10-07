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
  pub op censor<A>(@A, fn(W) -> Option<W>) -> @A
}

// есть своего рода наследование эффектов
pub effect WriterListener<W, Acc> :< Writer<W>  {
  pub op listen<A>(@A) -> @(A, Acc)
}

pub effect Error<E> {
  pub op throw(E) -> @!
  pub op <X>(@X) catch(E -> @X) -> @X
}

fn runState<S, X>(initial: S, action: @[State<S>, ..] X) -> @[..] (X, S) {
  action.interpret(<Y> handle State<S> returning (Y, S) {
    // How to make State branch together with NonDet?
    // Probably we need a way to combine states from successful branches,
    // e.g. fn(S, S) -> S, but how to express that in the effects system?
    // Alternatively, specify bind: <A, B> fn(@A, fn(A) -> @B) -> @B for any effect,
    // but that moves us to monadic design.

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
  let handler = |e: R| <Y> handle Reader<R> returning Y {
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

// Alternative stack-based interpreter. DelimitedStack is a runtime structure capturing
// the current environment as a stack to support nested locals and multi-shot control.
// API sketch: DelimitedStack::new(R) -> DelimitedStack<R>, top() -> R, push(R) -> (), pop() -> ()
fn runReaderWithDelimitedStack<R, X>(env: R, action: @[Reader<R>, ..] X) -> @[..] X {
  let handler = <Y> handle Reader<R> returning Y {
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
  let handler = <Y> handle Reader<R> returning Y {
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

// Generic Writer+Listener on top of DelimitedCell with user-supplied aggregation
// Accumulator model: `acc` aggregates events; `filters` is a stack of censor functions.
struct WriterState<W, Acc> {
  acc: Acc,
  filters: List<fn(W) -> Option<W>>,
}

fn runWriterListenerGeneric<W, Acc, X>(
  action: @[WriterListener<W, Acc>, ..] X,
  emptyAcc: Acc,
  wToAcc: fn(W) -> Acc,
  combine: fn(Acc, Acc) -> Acc,
) -> @[..] (X, Acc) {
  let handler = <Y> handle WriterListener<W, Acc> returning (Y, Acc) {
    let mut cell = DelimitedCell::new(WriterState { acc = emptyAcc, filters = Nil });

    op tell(w: W) -> @() {
      // Apply filters (inner-most first). Drop event if any filter returns None.
      let st = cell.get();
      let mut current: Option<W> = Some(w);
      for f in st.filters.iterator() {
        current = match current {
          None => None,
          Some(x) => f(x),
        };
      }
      match current {
        None => (),
        Some(w2) => {
          let acc1 = combine(st.acc, wToAcc(w2));
          cell.set(WriterState { acc = acc1, filters = st.filters });
        }
      }
    }

    op censor<Y>(sub: @Y, f: fn(W) -> Option<W>) -> @Y {
      // Temporarily extend filters, preserving the running accumulator.
      let before = cell.get();
      let extendedFilters = addFilter(f, before.filters); // conceptual: push f on top
      cell.set(WriterState { acc = before.acc, filters = extendedFilters });
      let y = sub.do;
      let after = cell.get();
      cell.set(WriterState { acc = after.acc, filters = before.filters });
      y
    }

    op listen<Y>(sub: @Y) -> @(Y, Acc) {
      // Isolate sub-accumulator, then merge it back and also return it.
      let parent = cell.get();
      cell.set(WriterState { acc = emptyAcc, filters = parent.filters });
      let y = sub.do;
      let localAcc = cell.get().acc;
      cell.set(WriterState { acc = combine(parent.acc, localAcc), filters = parent.filters });
      (y, localAcc)
    }

    returning (y: Y) -> (Y, Acc) {
      (y, cell.get().acc)
    }
  };

  action.interpret(handler).do
}

// State on top of DelimitedCell with per-branch snapshots
fn runStateDelimited<S, X>(initial: S, action: @[State<S>, ..] X) -> @[..] (X, S) {
  let handler = <Y> handle State<S> returning (Y, S) {
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
  let handler = <Y> handle Resource returning Y {
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
  let handler = <Y> handle Resource returning Y {
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
  let handler = handle Error<E> returning Result<E, Y> {
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
  let handler = <X> handle NonDet returning Stream<X> {
    op empty() -> @! {
      abort(Stream::empty()).do
    }

    op choose<F<_> :< Foldable, X>(elems: F<X>) -> @X {
      // delimited control; we also need control and control0,
      // think how to ensure type-safety of their usage
      shift(|resume| {
        let mut res = Stream::empty();
        for elem in elems.iterator() {
          res = res ++ resume(elem).do;
        }
        res
      }).do
    }

    returning<Y>(y: Y) -> @Stream<Y> {
      Stream::singleton(y)
    }
  }

  action.interpret(handler).do
}

// breadth-first non-determinism
fn runNonDetListBF<A>(action: @[NonDet, ..] A) -> @[..] Stream<A> {
  let handler = <X> handle NonDet returning Stream<X> {
    // как оно должно быть реализовано? через переустановку хэндлера?
    ...
  }

  action.interpret(handler).do
}
```
