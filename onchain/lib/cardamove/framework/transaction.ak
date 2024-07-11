use aiken/list.{length, map}
use aiken/transaction.{Input, Output, ScriptContext, TransactionId}
use aiken/transaction/credential.{Address}

pub fn transaction_inputs(ctx: ScriptContext) -> List<(Input, TransactionId)> {
  let ins = ctx.transaction.inputs
  map(ins, fn(i) { (i, i.output_reference.transaction_id) })
}

pub fn transaction_outputs(ctx: ScriptContext) -> List<Output> {
  ctx.transaction.outputs
}

pub fn transaction_input_addresses(ctx: ScriptContext) -> List<Address> {
  map(transaction_inputs(ctx), fn(i) { i.1st.output.address })
}

pub fn transaction_output_addresses(ctx: ScriptContext) -> List<Address> {
  map(transaction_outputs(ctx), fn(o) { o.address })
}

pub fn transaction_input_size(ctx: ScriptContext) -> Int {
  length(transaction_inputs(ctx))
}

pub fn transaction_output_size(ctx: ScriptContext) -> Int {
  length(transaction_outputs(ctx))
}
