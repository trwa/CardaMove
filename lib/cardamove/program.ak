use cardamove/internal/state
use cardamove/internal/storage
use cardamove/primitive.{Mv_Address, Mv_Bool, Mv_Reference, Mv_Unit}

pub type Mv_Program<s, t> =
  state.State<s, Option<t>>

pub fn bind(
  m: Mv_Program<s, a>,
  k: fn(a) -> Mv_Program<s, b>,
) -> Mv_Program<s, b> {
  let s <- state.bind(state.get())
  let o <- state.bind(m)
  when o is {
    Some(x) -> k(x)
    None -> state.state(fn(_) { (None, s) })
  }
}

pub fn return(a: a) -> Mv_Program<s, a> {
  state.return(Some(a))
}

fn halt() -> Mv_Program<s, a> {
  state.return(None)
}

fn run(m: Mv_Program<g, Mv_Unit>, s: g) -> Option<g> {
  let (assert, expected) = state.run_state(m, s)
  when assert is {
    Some(_) -> Some(expected)
    None -> None
  }
}

pub fn check(m: Mv_Program<g, Mv_Unit>, before: g, after: g) -> Bool {
  when m |> run(before) is {
    None -> False
    Some(expected) -> expected == after
  }
}

pub fn assert(condition: Bool) -> Mv_Program<g, Mv_Unit> {
  if condition {
    return(Void)
  } else {
    halt()
  }
}

pub fn move_to(
  address: Mv_Address,
  value: t,
  select: fn(g) -> storage.Storage<t>,
  update: fn(g, storage.Storage<t>) -> g,
) -> Mv_Program<g, Mv_Unit> {
  let s0: g <- state.bind(state.get())
  let storage = select(s0)
  if storage.has_key(storage, address) {
    halt()
  } else {
    let s1 = update(s0, storage.insert(storage, address, value))
    let _ <- state.bind(state.put(s1))
    return(Void)
  }
}

pub fn move_from(
  address: Mv_Address,
  select: fn(g) -> storage.Storage<t>,
  update: fn(g, storage.Storage<t>) -> g,
) -> Mv_Program<g, t> {
  let s0: g <- state.bind(state.get())
  let storage = select(s0)
  when storage.get(storage, address) is {
    None -> halt()
    Some(value) -> {
      let s1 = update(s0, storage.delete(storage, address))
      let _ <- state.bind(state.put(s1))
      return(value)
    }
  }
}

pub fn borrow_global(
  address: Mv_Address,
  select: fn(g) -> storage.Storage<t>,
  _update: fn(g, storage.Storage<t>) -> g,
) -> Mv_Program<g, Mv_Reference<t>> {
  let s: g <- state.bind(state.get())
  let o = storage.get(select(s), address)
  when o is {
    None -> halt()
    Some(value) -> return(Mv_Reference { address, value })
  }
}

pub fn assign(
  reference: Mv_Reference<t>,
  value: t,
  select: fn(g) -> storage.Storage<t>,
  update: fn(g, storage.Storage<t>) -> g,
) -> Mv_Program<g, Mv_Reference<t>> {
  let s0: g <- state.bind(state.get())
  let storage = select(s0)
  let s1 = update(s0, storage.insert(storage, reference.address, value))
  let _ <- state.bind(state.put(s1))
  return(Mv_Reference { address: reference.address, value })
}

pub fn exists(
  address: Mv_Address,
  select: fn(g) -> storage.Storage<t>,
  _update: fn(g, storage.Storage<t>) -> g,
) -> Mv_Program<g, Mv_Bool> {
  let s: g <- state.bind(state.get())
  let storage = select(s)
  return(storage.has_key(storage, address))
}
