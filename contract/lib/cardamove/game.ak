use aiken/list.{concat, length, map, unique}
use aiken/transaction.{Input, Output, ScriptContext, TransactionId}
use aiken/transaction/credential.{Address}

pub type EmptyDatum {
  EmptyDatum
}

pub type NewPlayersList<s> {
  new_players: List<(Address, s)>,
}

pub type CurrentPlayersList {
  current_players: List<Address>,
}

fn transaction_inputs(ctx: ScriptContext) -> List<(Input, TransactionId)> {
  let ins = ctx.transaction.inputs
  map(ins, fn(i) { (i, i.output_reference.transaction_id) })
}

fn transaction_outputs(ctx: ScriptContext) -> List<Output> {
  ctx.transaction.outputs
}

pub fn start(
  _datum: EmptyDatum,
  redeemer: NewPlayersList<s>,
  ctx: ScriptContext,
) -> Bool {
  let ks = map(redeemer.new_players, fn(p) { p.1st })
  let c1 = length(ks) == length(unique(ks))

  // TODO: validate the outputs
  let outputs = ctx.outputs

  c1
}
