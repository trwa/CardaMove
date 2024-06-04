use aiken/dict
use aiken/option.{and_then}
use aiken/transaction.{ScriptContext}
use cardamove/move.{Address, Signer, Storage, assert, move_from, move_to}
use cardamove/prelude.{Coin, Unit, gt}
use cardamove/state.{RunState, bind, get, put, return, run_state, state}

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
  Datum
}

//state_after: Global,

// =====================================================================================================================
// Redeemer contains the state before the transaction and the function that was called
// =====================================================================================================================

// Encodes what was **before** the transaction (in Move) and what function was called to modify it
type Redeemer {
  // TODO: Maybe this should be taken from elsewhere, here it seems not safe at all...
  //state_before: Global,
  dispatch: Dispatch,
}

// =====================================================================================================================
// Validator functions
// =====================================================================================================================

fn check_start(
  _state_before: Global,
  auctioneer: Signer,
  base: Coin,
  state_after: Global,
) -> Bool {
  //let check_auctioneer = state_after.auction.auctioneer == auctioneer
  //let check_top_bidder = state_after.auction.top_bidder == auctioneer
  //let check_expired = state_after.auction.expired == False
  //let check_bid = state_after.bid.coins == base

  //check_auctioneer && check_top_bidder && check_expired && check_bid
  True
}

fn sim_bid(
  bidder: Signer,
  auctioneer: Address,
  coins: Coin,
) -> RunState<Global, Option<Unit>> {
  let m0: Option<Auction> <-
    bind(move_from(auctioneer, gets_auction, puts_auction))
  when m0 is {
    Some(auction) -> {
      let m1: Option<Bid> <- bind(move_from(bidder, gets_bid, puts_bid))
      when m1 is {
        Some(bid) -> {
          let assert_expired <- bind(assert(auction.expired == False))
          when assert_expired is {
            Some(_) -> {
              let assert_top_bid <- bind(assert(gt(coins, bid.coins)))
              when assert_top_bid is {
                Some(_) -> {
                  let m2: Option<Unit> <-
                    bind(
                      move_to(auction.top_bidder, coins, gets_coin, puts_coin),
                    )
                  when m2 is {
                    Some(_) -> {
                      let m3: Option<Unit> <-
                        bind(
                          move_to(
                            auctioneer,
                            Auction { ..auction, top_bidder: bidder },
                            gets_auction,
                            puts_auction,
                          ),
                        )
                      when m3 is {
                        Some(_) -> {
                          let m4: Option<Unit> <-
                            bind(
                              move_to(bidder, Bid { coins }, gets_bid, puts_bid),
                            )
                          when m4 is {
                            Some(_) -> return(Some(Unit))
                            None -> return(None)
                          }
                        }
                        None -> return(None)
                      }
                    }
                    None -> return(None)
                  }
                }
                None -> return(None)
              }
            }
            None -> return(None)
          }
        }
        None -> return(None)
      }
    }
    None -> return(None)
  }
}

// =====================================================================================================================
//
// =====================================================================================================================

fn check_bid(
  state_before: Global,
  bidder: Signer,
  auctioneer: Address,
  coins: Coin,
  state_after: Global,
) -> Bool {
  let (assert, expected) =
    run_state(sim_bid(bidder, auctioneer, coins), state_before)
  when assert is {
    None -> False
    _ -> expected == state_after
  }
}

fn check_end(
  state_before: Global,
  auctioneer: Address,
  state_after: Global,
) -> Bool {
  let check_auctioneer = True
  // state_before.auction.auctioneer == auctioneer
  let auction_expired = True
  //state_after.auction.expired == True
  check_auctioneer && auction_expired
}

validator {
  fn run(datum: Datum, redeemer: Redeemer, _context: ScriptContext) -> Bool {
    let state_before =
      Global { auction: dict.new(), bid: dict.new(), coin: dict.new() }
    //redeemer.state_before
    //let state_after = datum.state_after
    let state_after =
      Global { auction: dict.new(), bid: dict.new(), coin: dict.new() }
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
