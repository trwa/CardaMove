use aiken/hash.{Blake2b_256, Hash, blake2b_256}
use aiken/list.{map}
use aiken/transaction.{Input, Output, ScriptContext, Transaction, TransactionId}
use aiken/transaction/credential.{Address}

// Find all the inputs of the current transaction + the hashes of the transactions that created them
pub fn transaction_inputs(ctx: ScriptContext) -> List<(Input, TransactionId)> {
  let ins = ctx.transaction.inputs
  map(ins, fn(i) { (i, i.output_reference.transaction_id) })
}

// Find all the outputs of the current transaction
pub fn transaction_outputs(ctx: ScriptContext) -> List<Output> {
  ctx.transaction.outputs
}

// Find all the addresses of the inputs of the current transaction
// Who "owns" each utxo that is being spent
pub fn transaction_input_addresses(ctx: ScriptContext) -> List<Address> {
  map(transaction_inputs(ctx), fn(i) { i.1st.output.address })
}

// Find all the addresses of the outputs of the current transaction
// Who "will own" each utxo that is being created
pub fn transaction_output_addresses(ctx: ScriptContext) -> List<Address> {
  map(transaction_outputs(ctx), fn(o) { o.address })
}

// The txbody is expected to be a Hex encoded (CBOR) string submitted by the off-chain code.
pub fn transaction_body_hash(txbody: ByteArray) -> Hash<Blake2b_256, Transaction> {
  blake2b_256(txbody)
}
