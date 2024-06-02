use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{VerificationKey}
use cardamove/util.{Value, gt}

// =====================================================================================================================
// Types of our framework's prelude
// =====================================================================================================================

// TODO: provide with our framework
// integrate the Value type from aiken/transaction/value
// into our auction contract code. This type allows us to handle
// multi-asset values, which is suitable for representing the coin values
// in our auction contract
type Coin =
  Value

// =====================================================================================================================
// Types encoding the state of the smart contract
// =====================================================================================================================

type Auction {
  auctioneer: VerificationKey,
  top_bidder: VerificationKey,
  expired: Bool,
}

type Bid {
  coin: Coin,
}

// =====================================================================================================================
// Datum, Redeemer
// We keep naming them like that only because it seems to be a convention in Aiken
// =====================================================================================================================

type Datum {
  auction: Auction,
  bid: Bid,
}

type Redeemer {
  StartFunc { auctioneer: VerificationKey, base: Coin }
  BidFunc { bidder: VerificationKey, auctioneer: VerificationKey, coins: Coin }
  EndFunc { auctioneer: VerificationKey }
}

// =====================================================================================================================
// Contract
// =====================================================================================================================

validator {
  fn run(datum: Datum, redeemer: Redeemer, context: ScriptContext) -> Bool {
    when redeemer is {
      StartFunc { auctioneer, base } ->
        start_auction(auctioneer, base, datum, context)
      BidFunc { bidder, auctioneer, coins } ->
        place_bid(bidder, auctioneer, coins, datum, context)
      EndFunc { auctioneer } -> end_auction(auctioneer, datum, context)
    }
  }
}

//fn zero() -> Value {
// Value { lovelace: 0 }
// assuming lovelace is the main unit in Value
//}

// Function to compare if one Value is greater than another
//fn gt(value1: Value, value2: Value) -> Bool {
// value1.lovelace > value2.lovelace
//}

fn start_auction(
  auctioneer: VerificationKey,
  base: Coin,
  datum: Datum,
  ctx: ScriptContext,
) -> Bool {
  // Ensure the auctioneer is the same and the auction has not started
  let is_auctioneer = datum.auction.auctioneer == auctioneer
  let top_bidder = datum.auction.top_bidder == auctioneer
  let expired = !datum.auction.expired
  let check_bid = datum.bid.coin == base
  //let valid_initial_amount = gt(base, zero())
  is_auctioneer && top_bidder && expired && check_bid
  //&& valid_initial_amount
}

fn place_bid(
  bidder: VerificationKey,
  auctioneer: VerificationKey,
  coins: Coin,
  datum: Datum,
  ctx: ScriptContext,
) -> Bool {
  // Ensure the auction is not expired and the new bid is higher than the current top bid
  let auction_not_expired = !datum.auction.expired
  let higher_bid = gt(coins, datum.bid.coin)
  // Validate bidder is not the same as the top bidder
  let valid_bidder = datum.auction.top_bidder != bidder

  auction_not_expired && higher_bid && valid_bidder
}

fn end_auction(
  auctioneer: VerificationKey,
  datum: Datum,
  ctx: ScriptContext,
) -> Bool {
  // Ensure only the auctioneer can end the auction and the auction is not expired
  let is_auctioneer = datum.auction.auctioneer == auctioneer
  let auction_not_expired = !datum.auction.expired

  is_auctioneer && auction_not_expired
}

// =====================================================================================================================
// Entry points
// =====================================================================================================================

fn start(auctioneer: VerificationKey, base: Coin) -> Datum {
  let auction = Auction { auctioneer, top_bidder: auctioneer, expired: False }
  let bid = Bid { coin: base }
  Datum { auction, bid }
}

fn bid(
  acc: VerificationKey,
  auctioneer: VerificationKey,
  coins: Coin,
  datum: Datum,
  context: ScriptContext,
) -> Datum {
  if place_bid(acc, auctioneer, coins, datum, context) {
    let new_datum =
      Datum {
        auction: Auction {
          auctioneer: datum.auction.auctioneer,
          top_bidder: acc,
          expired: datum.auction.expired,
        },
        bid: Bid { coin: coins },
      }
    new_datum
  } else {
    datum
  }
}

fn end(
  auctioneer: VerificationKey,
  datum: Datum,
  context: ScriptContext,
) -> Datum {
  if end_auction(auctioneer, datum, context) {
    let new_datum =
      Datum {
        auction: Auction {
          auctioneer: datum.auction.auctioneer,
          top_bidder: datum.auction.top_bidder,
          expired: True,
        },
        bid: datum.bid,
      }
    new_datum
  } else {
    datum
  }
}
