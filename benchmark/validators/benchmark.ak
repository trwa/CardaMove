use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, tokens}
use cardano/transaction.{
  InlineDatum, Output, OutputReference, Transaction, find_input,
  find_script_outputs,
}

pub type Storage {
  pairs: Pairs<ByteArray, ByteArray>,
}

pub type Empty {
  Empty
}

validator single(_id: ByteArray, _s: Int) {
  spend(
    datum: Option<Storage>,
    _redeemer: Empty,
    _output_reference: OutputReference,
    _self: Transaction,
  ) {
    // Force the validator to read the datum
    expect Some(_) = datum

    // Always succeed
    True
  }

  else(_) {
    fail
  }
}

validator token {
  mint(_redeemer: Empty, _policy_id: PolicyId, _self: Transaction) {
    // Always mint the tokens (we are just benchmarking the validator)
    True
  }

  else(_) {
    fail
  }
}

validator multi(policy_id: ByteArray, _c: Int, _n: Int, _t: Int) {
  spend(
    _datum: Option<Storage>,
    _redeemer: Empty,
    output_reference: OutputReference,
    self: Transaction,
  ) {
    // Get own input
    expect Some(input) = find_input(self.inputs, output_reference)

    // Get the assets in the own input
    let assets = tokens(input.output.value, policy_id)

    // There must be exactly one key asset in the own input
    expect [key] = dict.keys(assets)

    // Get own hash
    expect Some(script_hash) = input.output.reference_script

    // Get all inputs to the "multi" scripts
    let inputs: List<Output> =
      find_script_outputs(
        self.inputs |> list.map(fn(input) { input.output }),
        script_hash,
      )

    // Get all keys in the inputs 
    let keys =
      inputs
        |> list.map(
            fn(input) {
              let assets = tokens(input.value, policy_id)
              expect [key] = dict.keys(assets)
              key
            },
          )

    // Sort the keys
    let sorted_keys = list.sort(keys, bytearray.compare)

    // Get the smallest key
    expect Some(smallest_key) = list.head(sorted_keys)

    // The smallest key must be the same as the key in the own input
    if smallest_key == key {
      // Force the validator to read all the pairs from all scripts
      expect _ =
        inputs
          |> list.flat_map(
              fn(input) {
                expect InlineDatum(datum) = input.datum
                expect storage: Storage = datum
                storage.pairs
              },
            )
      True
    } else {
      // Just return true
      True
    }
  }

  else(_) {
    fail
  }
}
