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

type Datum {
  auction: Auction,
  bid: Bid,
}

type Redeemer {
  RedeemerStart { auctioneer: Signer, base: Coin }
  RedeemerBid { bidder: Signer, auctioneer: Address, coins: Coin }
  RedeemerEnd { auctioneer: Signer }
}

fn start(
  _datum: Datum,
  _auctioneer: Signer,
  _base: Coin,
  _context: ScriptContext,
) -> Bool {
  // TODO: Use code from Hammad
  False
}

fn bid(
  datum: Datum,
  bidder: Signer,
  auctioneer: Address,
  coins: Coin,
  context: ScriptContext,
) -> Bool {
  // Ensure the auction is not expired and the new bid is higher than the current top bid
  let auction_not_expired = !datum.auction.expired
  let higher_bid = gt(coins, datum.bid.coins)
  // Validate bidder is not the same as the top bidder
  let valid_bidder = datum.auction.top_bidder != bidder

  auction_not_expired && higher_bid && valid_bidder
}

fn end(_datum: Datum, _auctioneer: Address, _context: ScriptContext) -> Bool {
  // TODO: Use code from Hammad
  False
}

validator {
  fn run(datum: Datum, redeemer: Redeemer, context: ScriptContext) -> Bool {
    when redeemer is {
      RedeemerStart { auctioneer, base } ->
        start(datum, auctioneer, base, context)
      RedeemerBid { bidder, auctioneer, coins } ->
        bid(datum, bidder, auctioneer, coins, context)
      RedeemerEnd { auctioneer } -> end(datum, auctioneer, context)
    }
  }
}
