// =====================================================================================================================
// RunState
// ---------------------------------------------------------------------------------------------------------------------
// Aiken data types cannot hold functions, so we need to use a type alias.
// In Haskell, this would have been properly:
// data State s a = State { runState :: s -> (a, s) }
// Here, what we can actually represent is the type of runState.
// TODO: Find out if RunState can actually be a Monad instance (prove the three monad laws).
// =====================================================================================================================

pub type RunState<s, a> =
  fn(s) -> (a, s)

pub fn run_state(m: RunState<s, a>, s: s) -> (a, s) {
  m(s)
}

pub fn eval_state(m: RunState<s, a>, s: s) -> a {
  let (x, _) = run_state(m, s)
  x
}

pub fn exec_state(m: RunState<s, a>, s: s) -> s {
  let (_, s1) = run_state(m, s)
  s1
}

pub fn map_state(f: fn(a, s) -> (b, s), m: RunState<s, a>) -> RunState<s, b> {
  fn(s) {
    let (x, s1) = run_state(m, s)
    f(x, s1)
  }
}

// =====================================================================================================================
// Functor instance for RunState.
// =====================================================================================================================

// fmap :: Functor f => (a -> b) -> f a -> f b
pub fn fmap(f: fn(a) -> b, m: RunState<s, a>) -> RunState<s, b> {
  map_state(fn(x, s) { (f(x), s) }, m)
}

// =====================================================================================================================
// Applicative instance for RunState.
// =====================================================================================================================

// pure :: Applicative f => a -> f a
pub fn pure(x: a) -> RunState<s, a> {
  fn(s) { (x, s) }
}

// (<*>) :: Applicative f => f (a -> b) -> f a -> f b
pub fn ap(mf: RunState<s, fn(a) -> b>, ma: RunState<s, a>) -> RunState<s, b> {
  fn(s) {
    let (f, s1) = run_state(mf, s)
    let (x, s2) = run_state(ma, s1)
    (f(x), s2)
  }
}

// =====================================================================================================================
// Quick (and dirty) monadic instance for RunState.
// =====================================================================================================================

// return :: Monad m => a -> m a
pub fn return(x: a) -> RunState<s, a> {
  pure(x)
}

// (>>=) :: Monad m => m a -> (a -> m b) -> m b
pub fn bind(m: RunState<s, a>, f: fn(a) -> RunState<s, b>) -> RunState<s, b> {
  fn(s) {
    let (x, s1) = run_state(m, s)
    run_state(f(x), s1)
  }
}
