use aiken/list.{filter_map}
use aiken/transaction.{ScriptContext}
use cardamove/cardano/contract.{
  ContractDatum, ContractRedeemer, contract_validate,
}
use cardamove/move/prelude.{Coin, Unit, gt}
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
// Local/Global Storage
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
// Script
// =====================================================================================================================

type Dispatch {
  CheckStart { auctioneer: Signer, base: Coin }
  CheckBid { bidder: Signer, auctioneer: Address, coins: Coin }
  CheckEnd { auctioneer: Signer }
}

fn script(funcall: Dispatch) -> GuardState<GlobalStorage, Unit> {
  when funcall is {
    CheckStart { auctioneer, base } -> start(auctioneer, base)
    CheckBid { bidder, auctioneer, coins } -> bid(bidder, auctioneer, coins)
    CheckEnd { auctioneer } -> end(auctioneer)
  }
}

// =====================================================================================================================
// Validator
// =====================================================================================================================

validator {
  fn run(
    datum: ContractDatum<LocalStorage>,
    redeemer: ContractRedeemer<Dispatch>,
    context: ScriptContext,
  ) -> Bool {
    contract_validate(datum, redeemer, context, script, aggregate_storage)
  }
}
