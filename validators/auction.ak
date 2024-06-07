use aiken/transaction.{InlineDatum, ScriptContext, Spend}
use cardamove/move.{
  Address, GuardState, Ref, Signer, Storage, assert, assign, bind, borrow,
  from_list, move_from, move_to, return, value,
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

type GlobalStorage {
  auction: Storage<Auction>,
  bid: Storage<Bid>,
  coin: Storage<Coin>,
}

fn get_storage_auction(g: GlobalStorage) -> Storage<Auction> {
  g.auction
}

fn set_storage_auction(g: GlobalStorage, s: Storage<Auction>) -> GlobalStorage {
  GlobalStorage { ..g, auction: s }
}

fn get_storage_bid(g: GlobalStorage) -> Storage<Bid> {
  g.bid
}

fn set_storage_bid(g: GlobalStorage, s: Storage<Bid>) -> GlobalStorage {
  GlobalStorage { ..g, bid: s }
}

fn get_storage_coin(g: GlobalStorage) -> Storage<Coin> {
  g.coin
}

fn set_storage_coin(g: GlobalStorage, s: Storage<Coin>) -> GlobalStorage {
  GlobalStorage { ..g, coin: s }
}

// =====================================================================================================================
// Start
// =====================================================================================================================

fn start(auctioneer: Signer, base: Coin) -> GuardState<GlobalStorage, Unit> {
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

// =====================================================================================================================
// Bid
// =====================================================================================================================

fn bid(
  bidder: Signer,
  auctioneer: Address,
  coins: Coin,
) -> GuardState<GlobalStorage, Unit> {
  let auction: Ref<Auction> <-
    bind(borrow(auctioneer, get_storage_auction, set_storage_auction))
  let bid: Bid <- bind(move_from(bidder, get_storage_bid, set_storage_bid))
  let _: Unit <- bind(assert(value(auction).expired == False))
  let _: Unit <- bind(assert(gt(coins, bid.coins)))
  let _: Unit <-
    bind(
      move_to(
        value(auction).top_bidder,
        coins,
        get_storage_coin,
        set_storage_coin,
      ),
    )
  let _: Ref<Auction> <-
    bind(
      assign(
        auction,
        Auction { ..value(auction), top_bidder: bidder },
        get_storage_auction,
        set_storage_auction,
      ),
    )
  let _: Unit <-
    bind(move_to(bidder, Bid { coins }, get_storage_bid, set_storage_bid))
  return(Unit)
}

// =====================================================================================================================
// End
// =====================================================================================================================

fn end(auctioneer: Signer) -> GuardState<GlobalStorage, Unit> {
  let auction: Ref<Auction> <-
    bind(borrow(auctioneer, get_storage_auction, set_storage_auction))
  let _: Unit <- bind(assert(value(auction).auctioneer == auctioneer))
  let auction: Ref<Auction> <-
    bind(
      assign(
        auction,
        Auction { ..value(auction), expired: True },
        get_storage_auction,
        set_storage_auction,
      ),
    )
  let bid: Bid <-
    bind(move_from(value(auction).top_bidder, get_storage_bid, set_storage_bid))
  let _: Unit <-
    bind(move_to(auctioneer, bid.coins, get_storage_coin, set_storage_coin))
  return(Unit)
}

// =====================================================================================================================
// Check functions
// =====================================================================================================================

fn check_start(
  before: GlobalStorage,
  auctioneer: Signer,
  base: Coin,
  after: GlobalStorage,
) -> Bool {
  let (assert, expected) = run_state(start(auctioneer, base), before)
  when assert is {
    None -> False
    _ -> expected == after
  }
}

fn check_bid(
  before: GlobalStorage,
  bidder: Signer,
  auctioneer: Address,
  coins: Coin,
  after: GlobalStorage,
) -> Bool {
  let (assert, expected) = run_state(bid(bidder, auctioneer, coins), before)
  when assert is {
    None -> False
    _ -> expected == after
  }
}

fn check_end(
  before: GlobalStorage,
  auctioneer: Address,
  after: GlobalStorage,
) -> Bool {
  let (assert, expected) = run_state(end(auctioneer), before)
  when assert is {
    None -> False
    _ -> expected == after
  }
}

// =====================================================================================================================
// Validator
// =====================================================================================================================

type SerializedGlobalStorage {
  auction: List<(Address, Auction)>,
  bid: List<(Address, Bid)>,
  coin: List<(Address, Coin)>,
}

type Dispatch {
  CheckStart { auctioneer: Signer, base: Coin }
  CheckBid { bidder: Signer, auctioneer: Address, coins: Coin }
  CheckEnd { auctioneer: Signer }
}

fn parse(dump: SerializedGlobalStorage) -> GlobalStorage {
  GlobalStorage {
    auction: from_list(dump.auction),
    bid: from_list(dump.bid),
    coin: from_list(dump.coin),
  }
}

validator {
  fn run(
    output_datum: SerializedGlobalStorage,
    redeemer: Dispatch,
    context: ScriptContext,
  ) -> Bool {
    when context.purpose is {
      Spend(output) ->
        when context.transaction.inputs |> transaction.find_input(output) is {
          Some(input) ->
            when input.output.datum is {
              InlineDatum(data) -> {
                //! Unsafe conversion here
                expect input_datum: SerializedGlobalStorage = data

                // Normal code starts here
                let before: GlobalStorage = parse(input_datum)
                let after: GlobalStorage = parse(output_datum)
                when redeemer is {
                  CheckStart { auctioneer, base } ->
                    check_start(before, auctioneer, base, after)
                  CheckBid { bidder, auctioneer, coins } ->
                    check_bid(before, bidder, auctioneer, coins, after)
                  CheckEnd { auctioneer } ->
                    check_end(before, auctioneer, after)
                }
              }
              _ -> False
            }
          _ -> False
        }
      _ -> False
    }
  }
}
