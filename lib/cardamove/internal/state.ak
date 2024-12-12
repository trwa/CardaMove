// =====================================================================================================================
// State Monad
// ---------------------------------------------------------------------------------------------------------------------
// data State s a = State { runState :: s -> (a, s) }
// https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-State-Lazy.html#v:state
// =====================================================================================================================

pub type State<s, a> =
  fn(s) -> (a, s)

pub fn get() -> State<s, s> {
  state(fn(s) { (s, s) })
}

pub fn put(s: s) -> State<s, Void> {
  state(fn(_) { (Void, s) })
}

pub fn state(f: fn(s) -> (a, s)) -> State<s, a> {
  f
}

pub fn modify(f: fn(s) -> s) -> State<s, Void> {
  state(fn(s) { (Void, f(s)) })
}

pub fn run_state(m: State<s, a>, s: s) -> (a, s) {
  m(s)
}

pub fn eval_state(m: State<s, a>, s: s) -> a {
  let (x, _) = run_state(m, s)
  x
}

pub fn exec_state(m: State<s, a>, s: s) -> s {
  let (_, s1) = run_state(m, s)
  s1
}

pub fn map_state(f: fn(a, s) -> (b, s), m: State<s, a>) -> State<s, b> {
  fn(s) {
    let (x, s1) = run_state(m, s)
    f(x, s1)
  }
}

pub fn with_state(f: fn(s) -> s, m: State<s, a>) -> State<s, a> {
  state_then(modify(f), m)
}

// fmap :: Functor f => (a -> b) -> f a -> f b
pub fn fmap(f: fn(a) -> b, m: State<s, a>) -> State<s, b> {
  map_state(fn(x, s) { (f(x), s) }, m)
}

// pure :: Applicative f => a -> f a
pub fn pure(x: a) -> State<s, a> {
  state(fn(s) { (x, s) })
}

// (<*>) :: Applicative f => f (a -> b) -> f a -> f b
//fn ap(mf: State<s, fn(a) -> b>, ma: State<s, a>) -> State<s, b> {
//  state(
//    fn(s) {
//      let (f, s1) = run_state(mf, s)
//      let (x, s2) = run_state(ma, s1)
//      (f(x), s2)
//    },
//  )
//}

// return :: Monad m => a -> m a
pub fn return(x: a) -> State<s, a> {
  pure(x)
}

// (>>=) :: Monad m => m a -> (a -> m b) -> m b
pub fn bind(m: State<s, a>, f: fn(a) -> State<s, b>) -> State<s, b> {
  state(
    fn(s) {
      let (x, s1) = run_state(m, s)
      run_state(f(x), s1)
    },
  )
}

// (>>) :: Monad m => m a -> m b -> m b
pub fn state_then(ma: State<s, a>, mb: State<s, b>) -> State<s, b> {
  bind(ma, fn(_) { mb })
}
