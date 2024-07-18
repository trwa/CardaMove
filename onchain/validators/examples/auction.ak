use aiken/list.{filter_map}
use aiken/transaction.{ScriptContext}
use cardamove/cardano/contract.{
  ContractDatum, ContractRedeemer, contract_validate,
}
use cardamove/move/prelude.{Address, Signer}
use cardamove/move/storage.{
  MoveState, RefMut, Storage, assert, assign_mut, bind, borrow_global_mut,
  deref_mut, from_list, move_from, move_to, return,
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

fn storage_global_get_auction(g: StorageGlobal) -> Storage<Auction> {
  g.auction
}

fn storage_global_set_auction(
  g: StorageGlobal,
  s: Storage<Auction>,
) -> StorageGlobal {
  StorageGlobal { ..g, auction: s }
}

fn storage_global_get_bid(g: StorageGlobal) -> Storage<Bid> {
  g.bid
}

fn storage_global_set_bid(g: StorageGlobal, s: Storage<Bid>) -> StorageGlobal {
  StorageGlobal { ..g, bid: s }
}

fn storage_global_get_coin(g: StorageGlobal) -> Storage<Coin> {
  g.coin
}

fn storage_global_set_coin(g: StorageGlobal, s: Storage<Coin>) -> StorageGlobal {
  StorageGlobal { ..g, coin: s }
}

fn storage_utxo_get_auction(
  s: ContractDatum<StorageUtxo>,
) -> Option<(Address, Auction)> {
  when s.data.auction is {
    Some(x) -> Some((s.address, x))
    None -> None
  }
}

fn storage_utxo_get_bid(s: ContractDatum<StorageUtxo>) -> Option<(Address, Bid)> {
  when s.data.bid is {
    Some(x) -> Some((s.address, x))
    None -> None
  }
}

fn storage_utxo_get_coin(
  s: ContractDatum<StorageUtxo>,
) -> Option<(Address, Coin)> {
  when s.data.coin is {
    Some(x) -> Some((s.address, x))
    None -> None
  }
}

fn from_datums(
  datums: List<ContractDatum<StorageUtxo>>,
  get: fn(ContractDatum<StorageUtxo>) -> Option<(Address, t)>,
) -> Storage<t> {
  from_list(filter_map(datums, get))
}

fn storage_aggregate(datums: List<ContractDatum<StorageUtxo>>) -> StorageGlobal {
  StorageGlobal {
    auction: from_datums(datums, storage_utxo_get_auction),
    bid: from_datums(datums, storage_utxo_get_bid),
    coin: from_datums(datums, storage_utxo_get_coin),
  }
}

// Questo va qui perche' non si possono expectare tipi generici.
fn datum_downcast(data: Data) -> ContractDatum<StorageUtxo> {
  expect datum: ContractDatum<StorageUtxo> = data
  datum
}

// =====================================================================================================================
// Start
// =====================================================================================================================

fn start(auctioneer: Signer, base: Coin) -> MoveState<StorageGlobal, Void> {
  let auction = Auction { auctioneer, top_bidder: auctioneer, expired: False }
  let _: Void <-
    bind(
      move_to(
        auctioneer,
        auction,
        storage_global_get_auction,
        storage_global_set_auction,
      ),
    )
  let _: Void <-
    bind(
      move_to(
        auctioneer,
        Bid { coins: base },
        storage_global_get_bid,
        storage_global_set_bid,
      ),
    )
  return(Void)
}

// =====================================================================================================================
// Bid
// =====================================================================================================================

fn bid(
  bidder: Signer,
  auctioneer: Address,
  coins: Coin,
) -> MoveState<StorageGlobal, Void> {
  let auction: RefMut<Auction> <-
    bind(
      borrow_global_mut(
        auctioneer,
        storage_global_get_auction,
        storage_global_set_auction,
      ),
    )
  let Bid { coins: top_bid } <-
    bind(move_from(bidder, storage_global_get_bid, storage_global_set_bid))
  let _: Void <- bind(assert(deref_mut(auction).expired == False))
  let _: Void <- bind(assert(gt(coins, top_bid)))
  let _: Void <-
    bind(
      move_to(
        deref_mut(auction).top_bidder,
        coins,
        storage_global_get_coin,
        storage_global_set_coin,
      ),
    )
  let _: RefMut<Auction> <-
    bind(
      assign_mut(
        auction,
        Auction { ..deref_mut(auction), top_bidder: bidder },
        storage_global_get_auction,
        storage_global_set_auction,
      ),
    )
  let _: Void <-
    bind(
      move_to(
        bidder,
        Bid { coins },
        storage_global_get_bid,
        storage_global_set_bid,
      ),
    )
  return(Void)
}

// =====================================================================================================================
// End
// =====================================================================================================================

fn end(auctioneer: Signer) -> MoveState<StorageGlobal, Void> {
  let auction: RefMut<Auction> <-
    bind(
      borrow_global_mut(
        auctioneer,
        storage_global_get_auction,
        storage_global_set_auction,
      ),
    )
  let _: Void <- bind(assert(deref_mut(auction).auctioneer == auctioneer))
  let auction: RefMut<Auction> <-
    bind(
      assign_mut(
        auction,
        Auction { ..deref_mut(auction), expired: True },
        storage_global_get_auction,
        storage_global_set_auction,
      ),
    )
  let Bid { coins: top_bid } <-
    bind(
      move_from(
        deref_mut(auction).top_bidder,
        storage_global_get_bid,
        storage_global_set_bid,
      ),
    )
  let _: Void <-
    bind(
      move_to(
        auctioneer,
        top_bid,
        storage_global_get_coin,
        storage_global_set_coin,
      ),
    )
  return(Void)
}

// =====================================================================================================================
// Script
// =====================================================================================================================

type CallFunction {
  CallStart { auctioneer: Signer, base: Coin }
  CallBid { bidder: Signer, auctioneer: Address, coins: Coin }
  CallEnd { auctioneer: Signer }
}

fn script(dispatch: CallFunction) -> MoveState<StorageGlobal, Void> {
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
    contract_validate(
      datum,
      redeemer,
      context,
      script,
      storage_aggregate,
      datum_downcast,
    )
  }
}
