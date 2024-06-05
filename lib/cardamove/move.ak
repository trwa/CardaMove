use aiken/dict.{Dict}
use cardamove/prelude.{Unit}
use cardamove/state.{RunState, put, state}

// =====================================================================================================================
// Move Global Storage Operators
// ---
// https://move-language.github.io/move/global-storage-operators.html
// =====================================================================================================================

pub type Address =
  Int

pub type Signer =
  Address

pub type Storage<t> {
  internal: Dict<Address, t>,
}

pub type FailState<s, t> =
  RunState<s, Option<t>>

fn compare(lhs: Address, rhs: Address) -> Ordering {
  if lhs < rhs {
    Less
  } else if lhs > rhs {
    Greater
  } else {
    Equal
  }
}

fn has_key(storage: Storage<t>, address: Address) -> Bool {
  dict.has_key(storage.internal, address)
}

fn get(storage: Storage<t>, address: Address) -> Option<t> {
  dict.get(storage.internal, address)
}

fn insert(storage: Storage<t>, address: Address, value: t) -> Storage<t> {
  Storage { internal: dict.insert(storage.internal, address, value, compare) }
}

fn delete(storage: Storage<t>, address: Address) -> Storage<t> {
  Storage { internal: dict.delete(storage.internal, address) }
}

pub fn from_list(list: List<(Address, t)>) -> Storage<t> {
  let internal = dict.from_list(list, compare)
  Storage { internal }
}

pub fn assert(condition: Bool) -> FailState<g, Unit> {
  if condition {
    state.return(Some(Unit))
  } else {
    state.return(None)
  }
}

pub fn move_to(
  address: Address,
  value: t,
  gets: fn(g) -> Storage<t>,
  puts: fn(g, Storage<t>) -> g,
) -> FailState<g, Unit> {
  let s0: g <- state.bind(state.get())
  let storage = gets(s0)
  if has_key(storage, address) {
    state.return(None)
  } else {
    let s1 = puts(s0, insert(storage, address, value))
    let _ <- state.bind(put(s1))
    state.return(Some(Unit))
  }
}

pub fn move_from(
  address: Address,
  gets: fn(g) -> Storage<t>,
  puts: fn(g, Storage<t>) -> g,
) -> FailState<g, t> {
  let s0: g <- state.bind(state.get())
  let storage = gets(s0)
  when get(storage, address) is {
    None -> state.return(None)
    some -> {
      let s1 = puts(s0, delete(storage, address))
      let _ <- state.bind(put(s1))
      state.return(some)
    }
  }
}

pub fn borrow_global_mut(
  address: Address,
  gets: fn(g) -> Storage<t>,
  puts: fn(g, Storage<t>) -> g,
  comp: fn(t) -> FailState<g, t>,
) -> FailState<g, Unit> {
  let res0: t <- bind(move_from(address, gets, puts))
  let res1: t <- bind(comp(res0))
  let _ <- bind(move_to(address, res1, gets, puts))
  return(Unit)
}

// =====================================================================================================================
// FailState is a Monad
// =====================================================================================================================

pub fn bind(m: FailState<s, a>, f: fn(a) -> FailState<s, b>) -> FailState<s, b> {
  let s <- state.bind(state.get())
  let x <- state.bind(m)
  when x is {
    Some(v) -> f(v)
    None -> state.state(fn(_) { (None, s) })
  }
}

pub fn return(a: a) -> FailState<s, a> {
  state.return(Some(a))
}
