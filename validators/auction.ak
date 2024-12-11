use aiken/collection/dict.{Dict}
use cardamove/prelude.{Coin, gt}
use cardano/transaction.{InlineDatum, OutputReference, Spend, Transaction}

// =====================================================================================================================
// State Monad
// ---------------------------------------------------------------------------------------------------------------------
// data State s a = State { runState :: s -> (a, s) }
// https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-State-Lazy.html#v:state
// =====================================================================================================================

type State<s, a> =
  fn(s) -> (a, s)

fn state_get() -> State<s, s> {
  state_state(fn(s) { (s, s) })
}

fn state_put(s: s) -> State<s, Void> {
  state_state(fn(_) { (Void, s) })
}

fn state_state(f: fn(s) -> (a, s)) -> State<s, a> {
  f
}

fn state_modify(f: fn(s) -> s) -> State<s, Void> {
  state_state(fn(s) { (Void, f(s)) })
}

fn state_run_state(m: State<s, a>, s: s) -> (a, s) {
  m(s)
}

fn state_eval_state(m: State<s, a>, s: s) -> a {
  let (x, _) = state_run_state(m, s)
  x
}

fn state_exec_state(m: State<s, a>, s: s) -> s {
  let (_, s1) = state_run_state(m, s)
  s1
}

fn state_map_state(f: fn(a, s) -> (b, s), m: State<s, a>) -> State<s, b> {
  fn(s) {
    let (x, s1) = state_run_state(m, s)
    f(x, s1)
  }
}

fn state_with_state(f: fn(s) -> s, m: State<s, a>) -> State<s, a> {
  state_then(state_modify(f), m)
}

// fmap :: Functor f => (a -> b) -> f a -> f b
fn state_fmap(f: fn(a) -> b, m: State<s, a>) -> State<s, b> {
  state_map_state(fn(x, s) { (f(x), s) }, m)
}

// pure :: Applicative f => a -> f a
fn state_pure(x: a) -> State<s, a> {
  state_state(fn(s) { (x, s) })
}

// (<*>) :: Applicative f => f (a -> b) -> f a -> f b
//fn state_ap(mf: State<s, fn(a) -> b>, ma: State<s, a>) -> State<s, b> {
//  state_state(
//    fn(s) {
//      let (f, s1) = state_run_state(mf, s)
//      let (x, s2) = state_run_state(ma, s1)
//      (f(x), s2)
//    },
//  )
//}

// return :: Monad m => a -> m a
fn state_return(x: a) -> State<s, a> {
  state_pure(x)
}

// (>>=) :: Monad m => m a -> (a -> m b) -> m b
fn state_bind(m: State<s, a>, f: fn(a) -> State<s, b>) -> State<s, b> {
  state_state(
    fn(s) {
      let (x, s1) = state_run_state(m, s)
      state_run_state(f(x), s1)
    },
  )
}

// (>>) :: Monad m => m a -> m b -> m b
fn state_then(ma: State<s, a>, mb: State<s, b>) -> State<s, b> {
  state_bind(ma, fn(_) { mb })
}

// =====================================================================================================================
// Storage (Single Type)
// ---------------------------------------------------------------------------------------------------------------------
// https://move-language.github.io/move/global-storage-operators.html
// =====================================================================================================================

type Address =
  ByteArray

type Storage<t> =
  Dict<Address, t>

fn storage_has_key(storage: Storage<t>, address: Address) -> Bool {
  dict.has_key(storage, address)
}

fn storage_get(storage: Storage<t>, address: Address) -> Option<t> {
  dict.get(storage, address)
}

fn storage_insert(storage: Storage<t>, address: Address, value: t) -> Storage<t> {
  dict.insert(storage, address, value)
}

fn storage_delete(storage: Storage<t>, address: Address) -> Storage<t> {
  dict.delete(storage, address)
}

fn storage_from_pairs(pairs: Pairs<Address, t>) -> Storage<t> {
  dict.from_pairs(pairs)
}

// =====================================================================================================================
// Reference (TODO: Must be changed to use Path)
// =====================================================================================================================

type Reference<t> {
  address: Address,
  value: t,
}

fn reference_dereference(a: Reference<t>) -> t {
  a.value
}

// =====================================================================================================================
// Program
// =====================================================================================================================

type Program<s, t> =
  State<s, Option<t>>

fn program_bind(m: Program<s, a>, k: fn(a) -> Program<s, b>) -> Program<s, b> {
  let s <- state_bind(state_get())
  let o <- state_bind(m)
  when o is {
    Some(x) -> k(x)
    None -> state_state(fn(_) { (None, s) })
  }
}

fn program_return(a: a) -> Program<s, a> {
  state_return(Some(a))
}

fn program_assert(condition: Bool) -> Program<g, Void> {
  if condition {
    state_return(Some(Void))
  } else {
    state_return(None)
  }
}

fn program_move_to(
  address: Address,
  value: t,
  select: fn(g) -> Storage<t>,
  update: fn(g, Storage<t>) -> g,
) -> Program<g, Void> {
  let s0: g <- state_bind(state_get())
  let storage = select(s0)
  if storage_has_key(storage, address) {
    state_return(None)
  } else {
    let s1 = update(s0, storage_insert(storage, address, value))
    let _ <- state_bind(state_put(s1))
    program_return(Void)
  }
}

fn program_move_from(
  address: Address,
  select: fn(g) -> Storage<t>,
  update: fn(g, Storage<t>) -> g,
) -> Program<g, t> {
  let s0: g <- state_bind(state_get())
  let storage = select(s0)
  when storage_get(storage, address) is {
    None -> state_return(None)
    Some(value) -> {
      let s1 = update(s0, storage_delete(storage, address))
      let _ <- state_bind(state_put(s1))
      program_return(value)
    }
  }
}

fn program_borrow_global(
  address: Address,
  select: fn(g) -> Storage<t>,
  _update: fn(g, Storage<t>) -> g,
) -> Program<g, Reference<t>> {
  let s: g <- state_bind(state_get())
  let o = storage_get(select(s), address)
  when o is {
    None -> state_return(None)
    Some(value) -> program_return(Reference { address, value })
  }
}

fn program_assign(
  reference: Reference<t>,
  value: t,
  select: fn(g) -> Storage<t>,
  update: fn(g, Storage<t>) -> g,
) -> Program<g, Reference<t>> {
  let s0: g <- state_bind(state_get())
  let storage = select(s0)
  let s1 = update(s0, storage_insert(storage, reference.address, value))
  let _ <- state_bind(state_put(s1))
  program_return(Reference { address: reference.address, value })
}

fn program_exists(
  address: Address,
  select: fn(g) -> Storage<t>,
  _update: fn(g, Storage<t>) -> g,
) -> Program<g, Bool> {
  let s: g <- state_bind(state_get())
  let storage = select(s)
  program_return(storage_has_key(storage, address))
}

// =====================================================================================================================
// Auction Smart contract
// =====================================================================================================================

pub type Auction {
  auctioneer: Address,
  top_bidder: Address,
  expired: Bool,
}

pub type Bid {
  coins: Coin,
}

type GlobalStorage {
  auction: Storage<Auction>,
  bid: Storage<Bid>,
  coin: Storage<Coin>,
}

pub type SerializedGlobalStorage {
  auction: Pairs<Address, Auction>,
  bid: Pairs<Address, Bid>,
  coin: Pairs<Address, Coin>,
}

fn global_storage_parse(gs: SerializedGlobalStorage) -> GlobalStorage {
  GlobalStorage {
    auction: storage_from_pairs(gs.auction),
    bid: storage_from_pairs(gs.bid),
    coin: storage_from_pairs(gs.coin),
  }
}

fn select_auction(g: GlobalStorage) -> Storage<Auction> {
  g.auction
}

fn update_auction(g: GlobalStorage, s: Storage<Auction>) -> GlobalStorage {
  GlobalStorage { ..g, auction: s }
}

fn select_bid(g: GlobalStorage) -> Storage<Bid> {
  g.bid
}

fn update_bid(g: GlobalStorage, s: Storage<Bid>) -> GlobalStorage {
  GlobalStorage { ..g, bid: s }
}

fn select_coin(g: GlobalStorage) -> Storage<Coin> {
  g.coin
}

fn update_coin(g: GlobalStorage, s: Storage<Coin>) -> GlobalStorage {
  GlobalStorage { ..g, coin: s }
}

// =====================================================================================================================
// Entry points
// =====================================================================================================================

fn start(auctioneer: Address, base: Coin) -> Program<GlobalStorage, Void> {
  let auction = Auction { auctioneer, top_bidder: auctioneer, expired: False }
  let _: Void <-
    program_bind(
      program_move_to(auctioneer, auction, select_auction, update_auction),
    )
  let _: Void <-
    program_bind(
      program_move_to(auctioneer, Bid { coins: base }, select_bid, update_bid),
    )
  program_return(Void)
}

fn bid(
  bidder: Address,
  auctioneer: Address,
  coins: Coin,
) -> Program<GlobalStorage, Void> {
  let auction: Reference<Auction> <-
    program_bind(
      program_borrow_global(auctioneer, select_auction, update_auction),
    )
  let Bid { coins: top_bid } <-
    program_bind(program_move_from(bidder, select_bid, update_bid))
  let _: Void <-
    program_bind(
      program_assert(reference_dereference(auction).expired == False),
    )
  let _: Void <- program_bind(program_assert(gt(coins, top_bid)))
  let _: Void <-
    program_bind(
      program_move_to(
        reference_dereference(auction).top_bidder,
        coins,
        select_coin,
        update_coin,
      ),
    )
  let _: Reference<Auction> <-
    program_bind(
      program_assign(
        auction,
        Auction { ..reference_dereference(auction), top_bidder: bidder },
        select_auction,
        update_auction,
      ),
    )
  let _: Void <-
    program_bind(program_move_to(bidder, Bid { coins }, select_bid, update_bid))
  program_return(Void)
}

fn end(auctioneer: Address) -> Program<GlobalStorage, Void> {
  let auction: Reference<Auction> <-
    program_bind(
      program_borrow_global(auctioneer, select_auction, update_auction),
    )
  let _: Void <-
    program_bind(
      program_assert(reference_dereference(auction).auctioneer == auctioneer),
    )
  let auction: Reference<Auction> <-
    program_bind(
      program_assign(
        auction,
        Auction { ..reference_dereference(auction), expired: True },
        select_auction,
        update_auction,
      ),
    )
  let Bid { coins: top_bid } <-
    program_bind(
      program_move_from(
        reference_dereference(auction).top_bidder,
        select_bid,
        update_bid,
      ),
    )
  let _: Void <-
    program_bind(program_move_to(auctioneer, top_bid, select_coin, update_coin))
  program_return(Void)
}

// =====================================================================================================================
// Validator
// =====================================================================================================================

pub type EntryPoint {
  EntryPointStart { auctioneer: Address, base: Coin }
  EntryPointBid { bidder: Address, auctioneer: Address, coins: Coin }
  EntryPointEnd { auctioneer: Address }
}

validator auction {
  spend(
    datum: Option<SerializedGlobalStorage>,
    redeemer: EntryPoint,
    input: OutputReference,
    self: Transaction,
  ) {
    expect Some(input_serialized_global_storage) = datum
    let before: GlobalStorage =
      global_storage_parse(input_serialized_global_storage)

    expect [output] = self.outputs
    expect InlineDatum(data) = output.datum
    expect output_serialized_global_storage: SerializedGlobalStorage = data
    let after: GlobalStorage =
      global_storage_parse(output_serialized_global_storage)
    when redeemer is {
      EntryPointStart { auctioneer, base } -> {
        let (assert, expected) =
          start(auctioneer, base) |> state_run_state(before)
        when assert is {
          None -> False
          _ -> expected == after
        }
      }
      EntryPointBid { bidder, auctioneer, coins } -> {
        let (assert, expected) =
          bid(bidder, auctioneer, coins) |> state_run_state(before)
        when assert is {
          None -> False
          _ -> expected == after
        }
      }
      EntryPointEnd { auctioneer } -> {
        let (assert, expected) = end(auctioneer) |> state_run_state(before)
        when assert is {
          None -> False
          _ -> expected == after
        }
      }
    }
  }

  else(_) {
    fail
  }
}
