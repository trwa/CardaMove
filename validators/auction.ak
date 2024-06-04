use aiken/transaction.{ScriptContext}
use cardamove/move.{
  Address, FailState, Signer, Storage, assert, from_list, guard, move_from,
  move_to, success,
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

fn gets_auction(g: Global) -> Storage<Auction> {
  g.auction
}

fn puts_auction(g: Global, s: Storage<Auction>) -> Global {
  Global { ..g, auction: s }
}

fn gets_bid(g: Global) -> Storage<Bid> {
  g.bid
}

fn puts_bid(g: Global, s: Storage<Bid>) -> Global {
  Global { ..g, bid: s }
}

fn gets_coin(g: Global) -> Storage<Coin> {
  g.coin
}

fn puts_coin(g: Global, s: Storage<Coin>) -> Global {
  Global { ..g, coin: s }
}

// =====================================================================================================================
// Simulate functions
// =====================================================================================================================

fn sim_start(auctioneer: Signer, base: Coin) -> FailState<Global, Unit> {
  let _: Unit <-
    guard(
      move_to(
        auctioneer,
        Auction { auctioneer, top_bidder: auctioneer, expired: False },
        gets_auction,
        puts_auction,
      ),
    )
  let _: Unit <-
    guard(move_to(auctioneer, Bid { coins: base }, gets_bid, puts_bid))
  success(Unit)
}

fn sim_bid(
  bidder: Signer,
  auctioneer: Address,
  coins: Coin,
) -> FailState<Global, Unit> {
  let auction: Auction <-
    guard(move_from(auctioneer, gets_auction, puts_auction))
  let bid: Bid <- guard(move_from(bidder, gets_bid, puts_bid))
  let _: Unit <- guard(assert(auction.expired == False))
  let _: Unit <- guard(assert(gt(coins, bid.coins)))
  let _: Unit <- guard(move_to(auction.top_bidder, coins, gets_coin, puts_coin))
  let _: Unit <-
    guard(
      move_to(
        auctioneer,
        Auction { ..auction, top_bidder: bidder },
        gets_auction,
        puts_auction,
      ),
    )
  let _: Unit <- guard(move_to(bidder, Bid { coins }, gets_bid, puts_bid))
  success(Unit)
}

fn sim_end(auctioneer: Address) -> FailState<Global, Unit> {
  let auction: Auction <-
    guard(move_from(auctioneer, gets_auction, puts_auction))
  let _: Unit <- guard(assert(auction.expired == False))
  let _: Unit <-
    guard(
      move_to(
        auctioneer,
        Auction { ..auction, expired: True },
        gets_auction,
        puts_auction,
      ),
    )
  success(Unit)
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
    _ -> after == expected
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
  fn run(datum: Datum, redeemer: Redeemer, _context: ScriptContext) -> Bool {
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
    when dispatch is {
      CheckStart { auctioneer, base } ->
        check_start(before, auctioneer, base, after)
      CheckBid { bidder, auctioneer, coins } ->
        check_bid(before, bidder, auctioneer, coins, after)
      CheckEnd { auctioneer } -> check_end(before, auctioneer, after)
    }
  }
}
