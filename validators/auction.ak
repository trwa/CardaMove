use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{VerificationKey}

// =====================================================================================================================
// Types of our framework's prelude
// =====================================================================================================================

// TODO: provide with our framework
type Coin =
  Coin

// TODO: provide with our framework
type Address =
  ByteArray

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
  StartFunc { auctioneer: ByteArray, base: Coin }
  BidFunc { bidder: ByteArray, auctioneer: ByteArray, coins: Coin }
  EndFunc { auctioneer: ByteArray }
}

// =====================================================================================================================
// Contract
// =====================================================================================================================

validator {
  fn run(datum: Datum, redeemer: Redeemer, context: ScriptContext) -> Bool {
    when redeemer is {
      StartFunc { auctioneer, base } -> True
      BidFunc { bidder, auctioneer, coins } -> True
      EndFunc { auctioneer } -> True
    }
  }
}
