```rust
pub effect ConsoleOutput {
  pub op print(str: string) @()
}

pub effect Reader<A> {
  pub op ask() -> @A
  pub op local<X>(f: fn(A) -> A, action: @X) @X
}


pub effect State<S> {
  pub op get() -> @S
  pub op set(S) -> @()
}

pub effect Writer<W> {
  pub op tell(W) -> @()
  pub op censor<A>(@A, fn(W) -> Option<W>) -> @A
}

// есть своего рода наследование эффектов
pub effect WriterListener<W> :< Writer<W>  {
  type Acc
  op listen<A>(@A) -> @(A, Acc)
}

pub effect Error<E> {
  pub op throw<X>(E) -> @X
  pub op <X>(@X) catch(E -> @X) -> @X
}

fn runState<S, X>(initial: S, action: @[State<S> + ..] X) -> @[..] X {
  action.interpret(<Y> handle State<S> returning (Y, S) { tag =>
    let mut state = initial;
    
    op get() -> @S {
      tag.resume(state)
    }
    
    op put(s: S) @{
      state = s;
      tag.resume()
    }
    
    returning (y: Y) -> (Y, S) {
      (y, s)
    }
  })
}

fn runReader<R, X>(env: R, action: @[Reader<R> + ..] X) -> @[..] X {
  let handler = |e| <Y> handle Reader<R> returning Y { tag =>
    op ask() -> @R {
      tag.resume(e);
    }
    
    op local<Y>(f: fn(R) -> R, localAction: @Y) -> @Y {
      tag.resume(localAction.replaceInterpreter(|_| handler(f(e))).do);
    }
  };
  
  action.interpret(handler(env))
}

fn runError<E, X>(action: @[Error<E> + ..] X) -> @[..] Result<E, X> {
  let handler = handle Error<E> returning Result<E, Y> { tag =>
	  op throw(e: E) @{
		tag.abort(Err(e));
	  }
	  
	  op <Y>(action1: @Y) catch(errorHandler: E -> @Y) -> @Y {
		tag.recover(action1, |res| match res {
		  Err(e) => resume errorHandler(e).do,
		  Success(a) => resume a,
		})
	  }
	  
	  returning (y: Y) -> Result<E, Y> {
		Success(y)
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
  op empty<X>() -> @X
  op choose() -> @boolean
}

fn runNonDetList<A>(action: @[NonDet + ..] A) -> @[..] List<A> {
  let handler = <X> handle NonDet returning List<X> {
    op empty<Y>() -> @Y {
      abort Nil;
    }
    
    op choose() -> @boolean {
      let a = resume true;
      let b = resume false;
      a ++ b
    }
  }
}
```
