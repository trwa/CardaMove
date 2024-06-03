use aiken/transaction.{ScriptContext}
use cardamove/coin.{Coin, gt}
use cardamove/move.{Address, Signer}
use cardamove/state.{RunState, bind, get, put, return, run_state}

// =====================================================================================================================
// Types defined in the Move code
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
// Implicit global state
// =====================================================================================================================

type GlobalState {
  auction: Auction,
  top_bid: Bid,
}

// =====================================================================================================================
// Function calls
// =====================================================================================================================

type Dispatch {
  CheckStart { auctioneer: Signer, base: Coin }
  CheckBid { bidder: Signer, auctioneer: Address, coins: Coin }
  CheckEnd { auctioneer: Signer }
}

// =====================================================================================================================
// Datum contains the state after the transaction
// =====================================================================================================================

// Encodes what was **after** the transaction (in Move)
type Datum {
  state_after: GlobalState,
}

// =====================================================================================================================
// Redeemer contains the state before the transaction and the function that was called
// =====================================================================================================================

// Encodes what was **before** the transaction (in Move) and what function was called to modify it
type Redeemer {
  // TODO: Maybe this should be taken from elsewhere, here it seems not safe at all...
  state_before: GlobalState,
  dispatch: Dispatch,
}

// =====================================================================================================================
// Validator functions
// =====================================================================================================================

fn check_start(
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

fn sim_bid(
  bidder: Signer,
  _auctioneer: Address,
  coins: Coin,
) -> RunState<GlobalState, Bool> {
  let s0: GlobalState <- bind(get())
  let assert_expired = s0.auction.expired == False
  let assert_top_bid = gt(coins, s0.top_bid.coins)
  // TODO: How do we model this? 
  // Should we add accounts to the global state?
  // coins::deposit(auction.top_bidder, top_bid.coins)
  let _ <-
    bind(
      put(
        GlobalState {
          auction: Auction {
            auctioneer: s0.auction.auctioneer,
            top_bidder: bidder,
            expired: s0.auction.expired,
          },
          top_bid: Bid { coins },
        },
      ),
    )
  // TODO: Same as above.
  // move_to(bidder, coins)
  return(assert_expired && assert_top_bid)
}

fn check_bid(
  state_before: GlobalState,
  bidder: Signer,
  auctioneer: Address,
  coins: Coin,
  state_after: GlobalState,
) -> Bool {
  let expected = sim_bid(bidder, auctioneer, coins)
  let (assert, state) = run_state(expected, state_before)
  assert && state == state_after
}

fn check_end(
  state_before: GlobalState,
  auctioneer: Address,
  state_after: GlobalState,
) -> Bool {
  let check_auctioneer = state_before.auction.auctioneer == auctioneer
  let auction_expired = state_after.auction.expired == True
  check_auctioneer && auction_expired
}

validator {
  fn run(datum: Datum, redeemer: Redeemer, _context: ScriptContext) -> Bool {
    let state_before = redeemer.state_before
    let state_after = datum.state_after
    let dispatch = redeemer.dispatch
    when dispatch is {
      CheckStart { auctioneer, base } ->
        check_start(state_before, auctioneer, base, state_after)
      CheckBid { bidder, auctioneer, coins } ->
        check_bid(state_before, bidder, auctioneer, coins, state_after)
      CheckEnd { auctioneer } ->
        check_end(state_before, auctioneer, state_after)
    }
  }
}
