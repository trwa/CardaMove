use aiken/transaction.{ScriptContext, Spend}
use cardamove/move.{
  Address, FailState, Signer, Storage, assert, assign, bind, borrow, from_list,
  move_from, move_to, return,
}
use cardamove/prelude.{Coin, Unit, gt}
use cardamove/state.{run_state}

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

type Global {
  auction: Storage<Auction>,
  bid: Storage<Bid>,
  coin: Storage<Coin>,
}

fn get_storage_auction(g: Global) -> Storage<Auction> {
  g.auction
}

fn set_storage_auction(g: Global, s: Storage<Auction>) -> Global {
  Global { ..g, auction: s }
}

fn get_storage_bid(g: Global) -> Storage<Bid> {
  g.bid
}

fn set_storage_bid(g: Global, s: Storage<Bid>) -> Global {
  Global { ..g, bid: s }
}

fn get_storage_coin(g: Global) -> Storage<Coin> {
  g.coin
}

fn set_storage_coin(g: Global, s: Storage<Coin>) -> Global {
  Global { ..g, coin: s }
}

// =====================================================================================================================
// Simulate functions
// =====================================================================================================================

fn sim_start(auctioneer: Signer, base: Coin) -> FailState<Global, Unit> {
  let _: Unit <-
    bind(
      move_to(
        auctioneer,
        Auction { auctioneer, top_bidder: auctioneer, expired: False },
        get_storage_auction,
        set_storage_auction,
      ),
    )
  let _: Unit <-
    bind(
      move_to(auctioneer, Bid { coins: base }, get_storage_bid, set_storage_bid),
    )
  return(Unit)
}

fn sim_bid(
  bidder: Signer,
  auctioneer: Address,
  coins: Coin,
) -> FailState<Global, Unit> {
  let ass0: Auction <-
    bind(borrow(auctioneer, get_storage_auction, set_storage_auction))
  let bid: Bid <- bind(move_from(bidder, get_storage_bid, set_storage_bid))
  let _: Unit <- bind(assert(ass0.expired == False))
  let _: Unit <- bind(assert(gt(coins, bid.coins)))
  let _: Unit <-
    bind(move_to(ass0.top_bidder, coins, get_storage_coin, set_storage_coin))
  // It is important to **bind** the new value to a fresh variable
  // Because in more complex scenarios, we might need to use the new value
  let ass1 = Auction { ..ass0, top_bidder: bidder }
  let _: Unit <-
    bind(assign(auctioneer, ass1, get_storage_auction, set_storage_auction))
  let _: Unit <-
    bind(move_to(bidder, Bid { coins }, get_storage_bid, set_storage_bid))
  return(Unit)
}

fn sim_end(auctioneer: Signer) -> FailState<Global, Unit> {
  let ass0: Auction <-
    bind(borrow(auctioneer, get_storage_auction, set_storage_auction))
  let _: Unit <- bind(assert(ass0.auctioneer == auctioneer))
  let ass1 = Auction { ..ass0, expired: True }
  let _: Unit <-
    bind(assign(auctioneer, ass1, get_storage_auction, set_storage_auction))
  let bid: Bid <-
    bind(move_from(ass1.top_bidder, get_storage_bid, set_storage_bid))
  let _: Unit <-
    bind(move_to(auctioneer, bid.coins, get_storage_coin, set_storage_coin))
  return(Unit)
}

// =====================================================================================================================
// Check functions
// =====================================================================================================================

fn check_start(
  before: Global,
  auctioneer: Signer,
  base: Coin,
  after: Global,
) -> Bool {
  let (assert, expected) = run_state(sim_start(auctioneer, base), before)
  when assert is {
    None -> False
    _ -> expected == after
  }
}

fn check_bid(
  before: Global,
  bidder: Signer,
  auctioneer: Address,
  coins: Coin,
  after: Global,
) -> Bool {
  let (assert, expected) = run_state(sim_bid(bidder, auctioneer, coins), before)
  when assert is {
    None -> False
    _ -> expected == after
  }
}

fn check_end(before: Global, auctioneer: Address, after: Global) -> Bool {
  let (assert, expected) = run_state(sim_end(auctioneer), before)
  when assert is {
    None -> False
    _ -> expected == after
  }
}

// =====================================================================================================================
// State/Dispatch
// =====================================================================================================================

type State {
  auction: List<(Address, Auction)>,
  bid: List<(Address, Bid)>,
  coin: List<(Address, Coin)>,
}

type Dispatch {
  CheckStart { auctioneer: Signer, base: Coin }
  CheckBid { bidder: Signer, auctioneer: Address, coins: Coin }
  CheckEnd { auctioneer: Signer }
}

// =====================================================================================================================
// Datum/Redeemer
// =====================================================================================================================

type Datum {
  state: State,
}

type Redeemer {
  state: State,
  dispatch: Dispatch,
}

// =====================================================================================================================
// Validator
// =====================================================================================================================

validator {
  fn run(datum: Datum, redeemer: Redeemer, context: ScriptContext) -> Bool {
    let before =
      Global {
        auction: from_list(datum.state.auction),
        bid: from_list(datum.state.bid),
        coin: from_list(datum.state.coin),
      }
    let after =
      Global {
        auction: from_list(redeemer.state.auction),
        bid: from_list(redeemer.state.bid),
        coin: from_list(redeemer.state.coin),
      }
    let dispatch = redeemer.dispatch
    let purpose = context.purpose
    when purpose is {
      Spend(_) ->
        when dispatch is {
          CheckStart { auctioneer, base } ->
            check_start(before, auctioneer, base, after)
          CheckBid { bidder, auctioneer, coins } ->
            check_bid(before, bidder, auctioneer, coins, after)
          CheckEnd { auctioneer } -> check_end(before, auctioneer, after)
        }
      _ -> False
    }
  }
}
