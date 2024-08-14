use aiken/dict
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, ScriptContext, Transaction,
}
use aiken/transaction/value.{AssetName, MintedValue, quantity_of}

// =====================================================================================================================
// Conditions
// =====================================================================================================================

// The UTXO parameter is consumed in the inputs
fn c0(utxo: OutputReference, context: ScriptContext) -> Bool {
  let inputs: List<Input> = context.transaction.inputs
  let maybe: Option<Input> =
    list.find(inputs, fn(input) { input.output_reference == utxo })
  when maybe is {
    Some(_) -> True
    None -> False
  }
}

fn c1(address: AssetName, context: ScriptContext) -> Bool {
  // The transaction is a minting transaction with a single NFT of single amount
  let transaction: Transaction = context.transaction
  expect Mint(policy_id) = context.purpose
  let mint: MintedValue = transaction.mint
  let tokens: List<(AssetName, Int)> =
    mint |> value.from_minted_value |> value.tokens(policy_id) |> dict.to_list()
  expect [(_, amount)] = tokens
  // The minted NFT is in the outputs and the datum is ([] : List<AssetName>)
  let outputs: List<Output> = transaction.outputs
  expect [output] =
    list.filter(
      outputs,
      fn(output) { 1 == quantity_of(output.value, policy_id, address) },
    )
  expect InlineDatum(datum) = output.datum
  expect addresses: List<AssetName> = datum
  and {
    addresses == [],
    amount == 1,
  }
}

// =====================================================================================================================
// Validator
// =====================================================================================================================

validator(address: AssetName, utxo: OutputReference) {
  fn run(_redeemer: Data, context: ScriptContext) -> Bool {
    and {
      c0(utxo, context),
      c1(address, context),
    }
  }
}
