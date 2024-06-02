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
  bid: Bid,
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
  let check_bid = state_after.bid.coins == base

  check_auctioneer && check_top_bidder && check_expired && check_bid
}

fn bid(
  _state_before: GlobalState,
  _bidder: Signer,
  _auctioneer: Address,
  _coins: Coin,
  _state_after: GlobalState,
) -> Bool {
  // TODO: Do this with Hammad
  False
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
