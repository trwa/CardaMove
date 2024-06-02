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
  RedeemStart { auctioneer: Signer, base: Coin }
  RedeemBid { bidder: Signer, auctioneer: Address, coins: Coin }
  RedeemEnd { auctioneer: Signer }
}

fn start(
  auctioneer: Address,
  base: Coin,
  datum: Datum,
  ctx: ScriptContext,
) -> Bool {
  // TODO: Use code from Hammad
  False
}

fn bid(
  bidder: Address,
  auctioneer: Address,
  coins: Coin,
  datum: Datum,
  ctx: ScriptContext,
) -> Bool {
  // Ensure the auction is not expired and the new bid is higher than the current top bid
  let auction_not_expired = !datum.auction.expired
  let higher_bid = gt(coins, datum.bid.coins)
  // Validate bidder is not the same as the top bidder
  let valid_bidder = datum.auction.top_bidder != bidder

  auction_not_expired && higher_bid && valid_bidder
}

fn end(auctioneer: Address, datum: Datum, ctx: ScriptContext) -> Bool {
  // TODO: Use code from Hammad
  False
}

validator {
  fn run(datum: Datum, redeemer: Redeemer, context: ScriptContext) -> Bool {
    when redeemer is {
      RedeemStart { auctioneer, base } ->
        start(auctioneer, base, datum, context)
      RedeemBid { bidder, auctioneer, coins } ->
        bid(bidder, auctioneer, coins, datum, context)
      RedeemEnd { auctioneer } -> end(auctioneer, datum, context)
    }
  }
}
