use aiken/list.{filter_map}
use aiken/transaction.{ScriptContext}
use cardamove/cardano/contract.{
  ContractDatum, ContractRedeemer, contract_validate,
}
use cardamove/move/prelude.{Unit}
use cardamove/move/storage.{
  Address, GuardState, RefMut, Signer, Storage, assert, assign_mut, bind,
  borrow_global_mut, deref_mut, from_list, move_from, move_to, return,
}

// =====================================================================================================================
// Types
// =====================================================================================================================

type Auction {
  auctioneer: Address,
  top_bidder: Address,
  expired: Bool,
}

type Bid {
  coins: Coin,
}

type Coin {
  ada: Int,
}

fn gt(lhs: Coin, rhs: Coin) -> Bool {
  lhs.ada > rhs.ada
}

// =====================================================================================================================
// Storage
// =====================================================================================================================

type StorageUtxo {
  auction: Option<Auction>,
  bid: Option<Bid>,
  coin: Option<Coin>,
}

type StorageGlobal {
  auction: Storage<Auction>,
  bid: Storage<Bid>,
  coin: Storage<Coin>,
}

fn storage_get_auction(g: StorageGlobal) -> Storage<Auction> {
  g.auction
}

fn storage_set_auction(g: StorageGlobal, s: Storage<Auction>) -> StorageGlobal {
  StorageGlobal { ..g, auction: s }
}

fn storage_get_bid(g: StorageGlobal) -> Storage<Bid> {
  g.bid
}

fn storage_set_bid(g: StorageGlobal, s: Storage<Bid>) -> StorageGlobal {
  StorageGlobal { ..g, bid: s }
}

fn storage_get_coin(g: StorageGlobal) -> Storage<Coin> {
  g.coin
}

fn storage_set_coin(g: StorageGlobal, s: Storage<Coin>) -> StorageGlobal {
  StorageGlobal { ..g, coin: s }
}

fn storage_aggregate(storage: List<(Address, StorageUtxo)>) -> StorageGlobal {
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

  StorageGlobal {
    auction: from_list(auctions),
    bid: from_list(bids),
    coin: from_list(coins),
  }
}

// =====================================================================================================================
// Start
// =====================================================================================================================

fn start(auctioneer: Signer, base: Coin) -> GuardState<StorageGlobal, Unit> {
  let auction = Auction { auctioneer, top_bidder: auctioneer, expired: False }
  let _: Unit <-
    bind(move_to(auctioneer, auction, storage_get_auction, storage_set_auction))
  let _: Unit <-
    bind(
      move_to(auctioneer, Bid { coins: base }, storage_get_bid, storage_set_bid),
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
) -> GuardState<StorageGlobal, Unit> {
  let auction: RefMut<Auction> <-
    bind(
      borrow_global_mut(auctioneer, storage_get_auction, storage_set_auction),
    )
  let Bid { coins: top_bid } <-
    bind(move_from(bidder, storage_get_bid, storage_set_bid))
  let _: Unit <- bind(assert(deref_mut(auction).expired == False))
  let _: Unit <- bind(assert(gt(coins, top_bid)))
  let _: Unit <-
    bind(
      move_to(
        deref_mut(auction).top_bidder,
        coins,
        storage_get_coin,
        storage_set_coin,
      ),
    )
  let _: RefMut<Auction> <-
    bind(
      assign_mut(
        auction,
        Auction { ..deref_mut(auction), top_bidder: bidder },
        storage_get_auction,
        storage_set_auction,
      ),
    )
  let _: Unit <-
    bind(move_to(bidder, Bid { coins }, storage_get_bid, storage_set_bid))
  return(Unit)
}

// =====================================================================================================================
// End
// =====================================================================================================================

fn end(auctioneer: Signer) -> GuardState<StorageGlobal, Unit> {
  let auction: RefMut<Auction> <-
    bind(
      borrow_global_mut(auctioneer, storage_get_auction, storage_set_auction),
    )
  let _: Unit <- bind(assert(deref_mut(auction).auctioneer == auctioneer))
  let auction: RefMut<Auction> <-
    bind(
      assign_mut(
        auction,
        Auction { ..deref_mut(auction), expired: True },
        storage_get_auction,
        storage_set_auction,
      ),
    )
  let Bid { coins: top_bid } <-
    bind(
      move_from(deref_mut(auction).top_bidder, storage_get_bid, storage_set_bid),
    )
  let _: Unit <-
    bind(move_to(auctioneer, top_bid, storage_get_coin, storage_set_coin))
  return(Unit)
}

// =====================================================================================================================
// Script
// =====================================================================================================================

type CallFunction {
  CallStart { auctioneer: Signer, base: Coin }
  CallBid { bidder: Signer, auctioneer: Address, coins: Coin }
  CallEnd { auctioneer: Signer }
}

fn script(dispatch: CallFunction) -> GuardState<StorageGlobal, Unit> {
  when dispatch is {
    CallStart { auctioneer, base } -> start(auctioneer, base)
    CallBid { bidder, auctioneer, coins } -> bid(bidder, auctioneer, coins)
    CallEnd { auctioneer } -> end(auctioneer)
  }
}

// =====================================================================================================================
// Validator
// =====================================================================================================================

validator {
  fn run(
    datum: ContractDatum<StorageUtxo>,
    redeemer: ContractRedeemer<CallFunction>,
    context: ScriptContext,
  ) -> Bool {
    contract_validate(datum, redeemer, context, script, storage_aggregate)
  }
}
