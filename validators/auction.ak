use aiken/transaction.{ScriptContext}
use cardamove/coin.{Coin, gt}
use cardamove/move.{Address, Signer}

type Auction {
  auctioneer: Address,
  top_bidder: Address,
  expired: Bool,
}

type Bid {
  coins: Coin,
}

type GlobalState {
  auction: Auction,
  top_bid: Bid,
}

type Dispatch {
  CallStart { auctioneer: Signer, base: Coin }
  CallBid { bidder: Signer, auctioneer: Address, coins: Coin }
  CallEnd { auctioneer: Signer }
}

// Encodes what was **after** the transaction (in Move)
type Datum {
  state_after: GlobalState,
}

// Encodes what was **before** the transaction (in Move) and what function was called to modify it
type Redeemer {
  // TODO: Maybe this should be taken elsewhere...
  state_before: GlobalState,
  dispatch: Dispatch,
}

fn start(
  _state_before: GlobalState,
  auctioneer: Signer,
  base: Coin,
  state_after: GlobalState,
) -> Bool {
  let check_auctioneer = state_after.auction.auctioneer == auctioneer
  let check_top_bidder = state_after.auction.top_bidder == auctioneer
  let check_expired = state_after.auction.expired == False
  let check_bid = state_after.top_bid.coins == base

  check_auctioneer && check_top_bidder && check_expired && check_bid
}

fn bid(
  state_before: GlobalState,
  bidder: Signer,
  _auctioneer: Address,
  coins: Coin,
  state_after: GlobalState,
) -> Bool {
  // Here we take values from the state before the transaction...
  let auction = state_before.auction
  let top_bid = state_before.top_bid
  // ... and check them.
  let check_expired = auction.expired == False
  let check_top_bid = gt(coins, top_bid.coins)

  // TODO: How do we model this? Should we add actual accounts to the global state?
  // coins::deposit(auction.top_bidder, top_bid.coins)
  // ---
  // Here we take values from the state after the transaction...
  let top_bidder1 = state_after.auction.top_bidder
  let top_bid1 = state_after.top_bid
  // ... and check them.
  let check_top_bidder1 = top_bidder1 == bidder
  let check_top_bid1 = top_bid1.coins == coins

  check_expired && check_top_bid && check_top_bidder1 && check_top_bid1
  // TODO: Same as above
  // move_to(bidder, coins)
}

fn end(
  _state_before: GlobalState,
  _auctioneer: Address,
  _state_after: GlobalState,
) -> Bool {
  // TODO: Do this with Hammad
  False
}

validator {
  fn run(datum: Datum, redeemer: Redeemer, _context: ScriptContext) -> Bool {
    let state_before = redeemer.state_before
    let state_after = datum.state_after
    let dispatch = redeemer.dispatch
    when dispatch is {
      CallStart { auctioneer, base } ->
        start(state_before, auctioneer, base, state_after)
      CallBid { bidder, auctioneer, coins } ->
        bid(state_before, bidder, auctioneer, coins, state_after)
      CallEnd { auctioneer } -> end(state_before, auctioneer, state_after)
    }
  }
}
