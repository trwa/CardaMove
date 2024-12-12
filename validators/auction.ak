use cardamove/internal/storage
use cardamove/prelude.{Coin, gt}
use cardamove/primitive.{
  Mv_Address, Mv_Bool, Mv_Reference, Mv_Unit, dereference, mv_unit,
}
use cardamove/program.{
  Mv_Program, assert, assign, bind, borrow_global, move_from, move_to, return,
}
use cardano/transaction.{InlineDatum, OutputReference, Transaction}

pub type Auction {
  auctioneer: Mv_Address,
  top_bidder: Mv_Address,
  expired: Mv_Bool,
}

pub type Bid {
  coins: Coin,
}

type GlobalStorage {
  auction: storage.Storage<Auction>,
  bid: storage.Storage<Bid>,
  coin: storage.Storage<Coin>,
}

pub type SerializedGlobalStorage {
  auction: Pairs<Mv_Address, Auction>,
  bid: Pairs<Mv_Address, Bid>,
  coin: Pairs<Mv_Address, Coin>,
}

fn global_storage_parse(gs: SerializedGlobalStorage) -> GlobalStorage {
  GlobalStorage {
    auction: storage.from_pairs(gs.auction),
    bid: storage.from_pairs(gs.bid),
    coin: storage.from_pairs(gs.coin),
  }
}

fn select_auction(g: GlobalStorage) -> storage.Storage<Auction> {
  g.auction
}

fn update_auction(
  g: GlobalStorage,
  s: storage.Storage<Auction>,
) -> GlobalStorage {
  GlobalStorage { ..g, auction: s }
}

fn select_bid(g: GlobalStorage) -> storage.Storage<Bid> {
  g.bid
}

fn update_bid(g: GlobalStorage, s: storage.Storage<Bid>) -> GlobalStorage {
  GlobalStorage { ..g, bid: s }
}

fn select_coin(g: GlobalStorage) -> storage.Storage<Coin> {
  g.coin
}

fn update_coin(g: GlobalStorage, s: storage.Storage<Coin>) -> GlobalStorage {
  GlobalStorage { ..g, coin: s }
}

// =====================================================================================================================
// Entry points
// =====================================================================================================================

fn start(
  auctioneer: Mv_Address,
  base: Coin,
) -> Mv_Program<GlobalStorage, Mv_Unit> {
  let auction = Auction { auctioneer, top_bidder: auctioneer, expired: False }
  let _: Mv_Unit <-
    bind(move_to(auctioneer, auction, select_auction, update_auction))
  let _: Mv_Unit <-
    bind(move_to(auctioneer, Bid { coins: base }, select_bid, update_bid))
  return(mv_unit())
}

fn bid(
  bidder: Mv_Address,
  auctioneer: Mv_Address,
  coins: Coin,
) -> Mv_Program<GlobalStorage, Mv_Unit> {
  let auction: Mv_Reference<Auction> <-
    bind(borrow_global(auctioneer, select_auction, update_auction))
  let Bid { coins: top_bid } <- bind(move_from(bidder, select_bid, update_bid))
  let _: Mv_Unit <- bind(assert(dereference(auction).expired == False))
  let _: Mv_Unit <- bind(assert(gt(coins, top_bid)))
  let _: Mv_Unit <-
    bind(
      move_to(dereference(auction).top_bidder, coins, select_coin, update_coin),
    )
  let _: Mv_Reference<Auction> <-
    bind(
      assign(
        auction,
        Auction { ..dereference(auction), top_bidder: bidder },
        select_auction,
        update_auction,
      ),
    )
  let _: Mv_Unit <- bind(move_to(bidder, Bid { coins }, select_bid, update_bid))
  return(mv_unit())
}

fn end(auctioneer: Mv_Address) -> Mv_Program<GlobalStorage, Mv_Unit> {
  let auction: Mv_Reference<Auction> <-
    bind(borrow_global(auctioneer, select_auction, update_auction))
  let _: Mv_Unit <- bind(assert(dereference(auction).auctioneer == auctioneer))
  let auction: Mv_Reference<Auction> <-
    bind(
      assign(
        auction,
        Auction { ..dereference(auction), expired: True },
        select_auction,
        update_auction,
      ),
    )
  let Bid { coins: top_bid } <-
    bind(move_from(dereference(auction).top_bidder, select_bid, update_bid))
  let _: Mv_Unit <- bind(move_to(auctioneer, top_bid, select_coin, update_coin))
  return(mv_unit())
}

// =====================================================================================================================
// Validator
// =====================================================================================================================

pub type EntryPoint {
  EntryPointStart { auctioneer: Mv_Address, base: Coin }
  EntryPointBid { bidder: Mv_Address, auctioneer: Mv_Address, coins: Coin }
  EntryPointEnd { auctioneer: Mv_Address }
}

validator auction {
  spend(
    datum: Option<SerializedGlobalStorage>,
    redeemer: EntryPoint,
    _input: OutputReference,
    self: Transaction,
  ) {
    expect Some(input_serialized_global_storage) = datum
    let before: GlobalStorage =
      global_storage_parse(input_serialized_global_storage)

    expect [output] = self.outputs
    expect InlineDatum(data) = output.datum
    expect output_serialized_global_storage: SerializedGlobalStorage = data
    let after: GlobalStorage =
      global_storage_parse(output_serialized_global_storage)
    when redeemer is {
      EntryPointStart { auctioneer, base } ->
        start(auctioneer, base) |> program.check(before, after)
      EntryPointBid { bidder, auctioneer, coins } ->
        bid(bidder, auctioneer, coins) |> program.check(before, after)
      EntryPointEnd { auctioneer } ->
        end(auctioneer) |> program.check(before, after)
    }
  }

  else(_) {
    fail
  }
}
