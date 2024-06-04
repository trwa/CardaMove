use aiken/dict.{Dict}
use cardamove/prelude.{Unit}
use cardamove/state.{RunState, bind, get, put, return}

// =====================================================================================================================
// Move Global Storage Operators
// ---
// https://move-language.github.io/move/global-storage-operators.html
// =====================================================================================================================

pub type Address =
  Int

pub type Signer =
  Address

pub type Storage<t> =
  Dict<Address, t>

fn compare(lhs: Address, rhs: Address) -> Ordering {
  if lhs < rhs {
    Less
  } else if lhs > rhs {
    Greater
  } else {
    Equal
  }
}

pub fn assert(condition: Bool) -> RunState<g, Option<Unit>> {
  if condition {
    return(Some(Unit))
  } else {
    return(None)
  }
}

pub fn move_to(
  address: Address,
  value: t,
  gets: fn(g) -> Storage<t>,
  puts: fn(g, Storage<t>) -> g,
) -> RunState<g, Option<Unit>> {
  let s0: g <- bind(get())
  let storage = gets(s0)
  if dict.has_key(storage, address) {
    return(None)
  } else {
    let s1 = puts(s0, dict.insert(storage, address, value, compare))
    let _ <- bind(put(s1))
    return(Some(Unit))
  }
}

pub fn move_from(
  address: Address,
  gets: fn(g) -> Storage<t>,
  puts: fn(g, Storage<t>) -> g,
) -> RunState<g, Option<t>> {
  let s0: g <- bind(get())
  let storage = gets(s0)
  when dict.get(storage, address) is {
    None -> return(None)
    some -> {
      let s1 = puts(s0, dict.delete(storage, address))
      let _ <- bind(put(s1))
      return(some)
    }
  }
}

pub fn borrow(
  address: Address,
  gets: fn(g) -> Storage<t>,
  puts: fn(g, Storage<t>) -> g,
  comp: fn(t) -> RunState<g, Option<t>>,
) -> RunState<g, Option<Unit>> {
  let s0: g <- bind(get())
  let storage = gets(s0)
  when dict.get(storage, address) is {
    None -> return(None)
    Some(x) -> {
      let my: Option<t> <- bind(comp(x))
      when my is {
        None -> return(None)
        Some(y) -> {
          let s2 = puts(s0, dict.insert(storage, address, y, compare))
          let _ <- bind(put(s2))
          return(Some(Unit))
        }
      }
    }
  }
}
