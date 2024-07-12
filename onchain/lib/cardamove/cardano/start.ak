use aiken/list.{concat, filter, length, unique}
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{Address}
use cardamove/cardano/script.{script_address}
use cardamove/cardano/transaction.{transaction_inputs,
  transaction_outputs} as transaction2

pub type StartDatum {
  keys: List<Address>,
}

pub type StartRedeemer {
  keys: List<Address>,
  tx_body: ByteArray,
}

// Fully validate the start script conditions
pub fn start_validate(
  contract_address: Address,
  datum: StartDatum,
  redeemer: StartRedeemer,
  context: ScriptContext,
) -> Bool {
  let new_keys = concat(datum.keys, redeemer.keys)
  let inputs = transaction_inputs(context)
  let outputs = transaction_outputs(context)
  let outputs_to_contract =
    filter(outputs, fn(o) { o.address == contract_address })
  let outputs_to_itself =
    filter(outputs, fn(o) { o.address == script_address(context) })
  and {
    // No duplicate keys in the datum or redeemer
    length(datum.keys) == length(unique(datum.keys)),
    length(redeemer.keys) == length(unique(redeemer.keys)),
    // Datum and redeemer are disjoint
    length(new_keys) == length(unique(new_keys)),
    // The transaction consumes only one input and produces only one output
    length(inputs) == 1,
    length(outputs) == 1,
    // One output is to the contract itself
    length(outputs_to_itself) == 1,
    // The rest of the outputs are to the contract
    length(outputs_to_contract) == length(outputs) - 1,
  }
  // Every output to the contract has one of the keys
  // TODO: Implement this check
}
