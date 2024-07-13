use aiken/list.{filter_map}
use aiken/transaction.{InlineDatum, ScriptContext, Spend}
use cardamove/move/prelude.{Coin, Unit, gt}
use cardamove/move/state.{run_state}
use cardamove/move/storage.{
  Address, GuardState, RefMut, Signer, Storage, assert, assign_mut, bind,
  borrow_global_mut, deref_mut, from_list, move_from, move_to, return,
}

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
// NEW
// =====================================================================================================================

type LocalStorage {
  auction: Option<Auction>,
  bid: Option<Bid>,
  coin: Option<Coin>,
}

fn aggregate_storage(storage: List<(Address, LocalStorage)>) -> GlobalStorage {
  let auctions =
    filter_map(
      storage,
      fn(pair) {
        when pair.2nd.auction is {
          Some(x) -> Some((pair.1st, x))
          None -> None
        }
      },
    )

  let bids =
    filter_map(
      storage,
      fn(pair) {
        when pair.2nd.bid is {
          Some(x) -> Some((pair.1st, x))
          None -> None
        }
      },
    )

  let coins =
    filter_map(
      storage,
      fn(pair) {
        when pair.2nd.coin is {
          Some(x) -> Some((pair.1st, x))
          None -> None
        }
      },
    )

  GlobalStorage {
    auction: from_list(auctions),
    bid: from_list(bids),
    coin: from_list(coins),
  }
}

// =====================================================================================================================
// Start
// =====================================================================================================================

fn start(auctioneer: Signer, base: Coin) -> GuardState<GlobalStorage, Unit> {
  let auction = Auction { auctioneer, top_bidder: auctioneer, expired: False }
  let _: Unit <-
    bind(move_to(auctioneer, auction, get_storage_auction, set_storage_auction))
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
  let auction: RefMut<Auction> <-
    bind(
      borrow_global_mut(auctioneer, get_storage_auction, set_storage_auction),
    )
  let Bid { coins: top_bid } <-
    bind(move_from(bidder, get_storage_bid, set_storage_bid))
  let _: Unit <- bind(assert(deref_mut(auction).expired == False))
  let _: Unit <- bind(assert(gt(coins, top_bid)))
  let _: Unit <-
    bind(
      move_to(
        deref_mut(auction).top_bidder,
        coins,
        get_storage_coin,
        set_storage_coin,
      ),
    )
  let _: RefMut<Auction> <-
    bind(
      assign_mut(
        auction,
        Auction { ..deref_mut(auction), top_bidder: bidder },
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
  let auction: RefMut<Auction> <-
    bind(
      borrow_global_mut(auctioneer, get_storage_auction, set_storage_auction),
    )
  let _: Unit <- bind(assert(deref_mut(auction).auctioneer == auctioneer))
  let auction: RefMut<Auction> <-
    bind(
      assign_mut(
        auction,
        Auction { ..deref_mut(auction), expired: True },
        get_storage_auction,
        set_storage_auction,
      ),
    )
  let Bid { coins: top_bid } <-
    bind(
      move_from(deref_mut(auction).top_bidder, get_storage_bid, set_storage_bid),
    )
  let _: Unit <-
    bind(move_to(auctioneer, top_bid, get_storage_coin, set_storage_coin))
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
  let (assert, expected) = start(auctioneer, base) |> run_state(before)
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
  let (assert, expected) = bid(bidder, auctioneer, coins) |> run_state(before)
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
  let (assert, expected) = end(auctioneer) |> run_state(before)
  when assert is {
    None -> False
    _ -> expected == after
  }
}

// =====================================================================================================================
// Validator
// =====================================================================================================================

type StorageSnapshot {
  auction: List<(Address, Auction)>,
  bid: List<(Address, Bid)>,
  coin: List<(Address, Coin)>,
}

type Dispatch {
  CheckStart { auctioneer: Signer, base: Coin }
  CheckBid { bidder: Signer, auctioneer: Address, coins: Coin }
  CheckEnd { auctioneer: Signer }
}

fn parse(dump: StorageSnapshot) -> GlobalStorage {
  GlobalStorage {
    auction: from_list(dump.auction),
    bid: from_list(dump.bid),
    coin: from_list(dump.coin),
  }
}

validator {
  fn run(
    output_datum: StorageSnapshot,
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
                expect input_datum: StorageSnapshot = data

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
