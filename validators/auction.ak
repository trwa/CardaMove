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
fn state_ap(mf: State<s, fn(a) -> b>, ma: State<s, a>) -> State<s, b> {
  state_state(
    fn(s) {
      let (f, s1) = state_run_state(mf, s)
      let (x, s2) = state_run_state(ma, s1)
      (f(x), s2)
    },
  )
}

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

type Signer =
  Address

type Storage<t> {
  impl: Dict<Address, t>,
}

fn storage_has_key(storage: Storage<t>, address: Address) -> Bool {
  dict.has_key(storage.impl, address)
}

fn storage_get(storage: Storage<t>, address: Address) -> Option<t> {
  dict.get(storage.internal, address)
}

fn storage_insert(storage: Storage<t>, address: Address, value: t) -> Storage<t> {
  Storage { impl: dict.insert(storage.internal, address, value) }
}

fn storage_delete(storage: Storage<t>, address: Address) -> Storage<t> {
  Storage { impl: dict.delete(storage.internal, address) }
}

fn storage_from_pairs(pairs: Pairs<Address, t>) -> Storage<t> {
  Storage { impl: dict.from_pairs(pairs) }
}

// =====================================================================================================================
// Reference (TODO: Must be changed to use Path)
// =====================================================================================================================

opaque type Reference<t> {
  address: Address,
  value: t,
}

pub fn reference_dereference(a: Reference<t>) -> t {
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

fn program_halt() -> Program<s, a> {
  state_return(None)
}

fn program_assert(condition: Bool) -> Program<g, Void> {
  if condition {
    state_return(Some(Void))
  } else {
    state_return(None)
  }
}

pub fn program_move_to(
  address: Address,
  value: t,
  get_storage: fn(g) -> Storage<t>,
  set_storage: fn(g, Storage<t>) -> g,
) -> Program<g, Void> {
  let s0: g <- state_bind(state_get())
  let storage = get_storage(s0)
  if storage_has_key(storage, address) {
    program_halt()
  } else {
    let s1 = set_storage(s0, storage_insert(storage, address, value))
    let _ <- state_bind(state_put(s1))
    program_return(Void)
  }
}

pub fn program_move_from(
  address: Address,
  get_storage: fn(g) -> Storage<t>,
  set_storage: fn(g, Storage<t>) -> g,
) -> Program<g, t> {
  let s0: g <- state_bind(state_get())
  let storage = get_storage(s0)
  when storage_get(storage, address) is {
    None -> program_halt()
    Some(value) -> {
      let s1 = set_storage(s0, storage_delete(storage, address))
      let _ <- state_bind(state_put(s1))
      program_return(value)
    }
  }
}

pub fn program_borrow_global(
  address: Address,
  get_storage: fn(g) -> Storage<t>,
  _set_storage: fn(g, Storage<t>) -> g,
) -> Program<g, Reference<t>> {
  let s: g <- state_bind(state_get())
  let o = storage_get(get_storage(s), address)
  when o is {
    None -> program_halt()
    Some(value) -> program_return(Reference { address, value })
  }
}

pub fn program_assign(
  reference: Reference<t>,
  value: t,
  get_storage: fn(g) -> Storage<t>,
  set_storage: fn(g, Storage<t>) -> g,
) -> Program<g, Reference<t>> {
  let s0: g <- state_bind(state_get())
  let storage = get_storage(s0)
  let s1 = set_storage(s0, storage_insert(storage, reference.address, value))
  let _ <- state_bind(state_put(s1))
  program_return(Reference { address: reference.address, value })
}

pub fn program_exists(
  address: Address,
  get_storage: fn(g) -> Storage<t>,
) -> Program<g, Bool> {
  let s: g <- state_bind(state_get())
  let storage = get_storage(s)
  program_return(storage_has_key(storage, address))
}

// =====================================================================================================================
// Auction Smart contract
// =====================================================================================================================

// =====================================================================================================================
// All the types that have the 'key' ability and are defined at module level
// =====================================================================================================================

type Auction {
  auctioneer: Address,
  top_bidder: Address,
  expired: Bool,
}

type Bid {
  coins: Coin,
}

// =====================================================================================================================
// Global Storage
// ---
// We **must** generate the record selectors and updaters for the global storage.
// =====================================================================================================================

type GlobalStorage {
  auction: Storage<Auction>,
  bid: Storage<Bid>,
  coin: Storage<Coin>,
}

fn get_storage_auction(g: GlobalStorage) -> Storage<Auction> {
  g.auction
}

fn set_storage_auction(g: GlobalStorage, s: Storage<Auction>) -> GlobalStorage {
  GlobalStorage { ..g, auction: s }
}

fn get_storage_bid(g: GlobalStorage) -> Storage<Bid> {
  g.bid
}

fn set_storage_bid(g: GlobalStorage, s: Storage<Bid>) -> GlobalStorage {
  GlobalStorage { ..g, bid: s }
}

fn get_storage_coin(g: GlobalStorage) -> Storage<Coin> {
  g.coin
}

fn set_storage_coin(g: GlobalStorage, s: Storage<Coin>) -> GlobalStorage {
  GlobalStorage { ..g, coin: s }
}

// =====================================================================================================================
// Entry points
// =====================================================================================================================

fn start(auctioneer: Signer, base: Coin) -> Program<GlobalStorage, Void> {
  let auction = Auction { auctioneer, top_bidder: auctioneer, expired: False }
  let _: Void <-
    program_bind(
      program_move_to(
        auctioneer,
        auction,
        get_storage_auction,
        set_storage_auction,
      ),
    )
  let _: Void <-
    program_bind(
      program_move_to(
        auctioneer,
        Bid { coins: base },
        get_storage_bid,
        set_storage_bid,
      ),
    )
  program_return(Void)
}

fn bid(
  bidder: Signer,
  auctioneer: Address,
  coins: Coin,
) -> Program<GlobalStorage, Void> {
  let auction: Reference<Auction> <-
    program_bind(
      program_borrow_global(
        auctioneer,
        get_storage_auction,
        set_storage_auction,
      ),
    )
  let Bid { coins: top_bid } <-
    program_bind(program_move_from(bidder, get_storage_bid, set_storage_bid))
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
        get_storage_coin,
        set_storage_coin,
      ),
    )
  let _: Reference<Auction> <-
    program_bind(
      program_assign(
        auction,
        Auction { ..reference_dereference(auction), top_bidder: bidder },
        get_storage_auction,
        set_storage_auction,
      ),
    )
  let _: Void <-
    program_bind(
      program_move_to(bidder, Bid { coins }, get_storage_bid, set_storage_bid),
    )
  program_return(Void)
}

fn end(auctioneer: Signer) -> Program<GlobalStorage, Void> {
  let auction: Reference<Auction> <-
    bind(
      borrow_global_mut(auctioneer, get_storage_auction, set_storage_auction),
    )
  let _: Void <- bind(assert(deref_mut(auction).auctioneer == auctioneer))
  let auction: Reference<Auction> <-
    bind(
      assign_mut(
        auction,
        Auction { ..deref_mut(auction), expired: True },
        get_storage_auction,
        set_storage_auction,
      ),
    )
  let Bid { coins: top_bid } <-
    bind(
      move_from(deref_mut(auction).top_bidder, get_storage_bid, set_storage_bid),
    )
  let _: Void <-
    bind(move_to(auctioneer, top_bid, get_storage_coin, set_storage_coin))
  return(Void)
}

// =====================================================================================================================
// Validator
// =====================================================================================================================

type StorageSnapshot {
  auction: Pairs<Address, Auction>,
  bid: Pairs<Address, Bid>,
  coin: Pairs<Address, Coin>,
}

type Dispatch {
  CheckStart { auctioneer: Signer, base: Coin }
  CheckBid { bidder: Signer, auctioneer: Address, coins: Coin }
  CheckEnd { auctioneer: Signer }
}

fn parse(dump: StorageSnapshot) -> GlobalStorage {
  GlobalStorage {
    auction: storage_from_pairs(dump.auction),
    bid: storage_from_pairs(dump.bid),
    coin: storage_from_pairs(dump.coin),
  }
}

validator auction {
  spend(
    output_datum: Option<StorageSnapshot>,
    redeemer: Dispatch,
    input_reference: OutputReference,
    self: Transaction,
  ) {
    // IO boundaries
    expect InlineDatum(data) = input_reference.output.datum
    expect input_datum: StorageSnapshot = data

    // Normal code starts here
    let before: GlobalStorage = parse(input_datum)
    let after: GlobalStorage = parse(output_datum)
    when redeemer is {
      CheckStart { auctioneer, base } -> {
        let (assert, expected) =
          start(auctioneer, base) |> state_run_state(before)
        when assert is {
          None -> False
          _ -> expected == after
        }
      }
      CheckBid { bidder, auctioneer, coins } -> {
        let (assert, expected) =
          bid(bidder, auctioneer, coins) |> state_run_state(before)
        when assert is {
          None -> False
          _ -> expected == after
        }
      }
      CheckEnd { auctioneer } -> {
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
