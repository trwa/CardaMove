use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use aiken/primitive/int
use cardano/assets.{AssetName, PolicyId, policies, tokens}
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

validator single(_s: Int, _t: Int) {
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
    datum: Option<Storage>,
    _redeemer: Empty,
    output_reference: OutputReference,
    self: Transaction,
  ) {
    // Force the validator to read the datum
    expect Some(_) = datum

    // Get own output
    let input: Output =
      find_script_outputs(self.outputs, output_reference) |> list.head

    // Get input token
    let token = tokens(input.value, policy_id)
    

    // Get the inputs that have the token
    let tokens_datums: Pairs<AssetName, Storage> =
      self.inputs 
        |> list.map(fn(input) { input.output })
        |> list.filter(
            fn(output) { policies(output.value) |> list.has(policy_id) },
          )
        |> list.map(
            fn(output) {
              expect [pair]: Pairs<AssetName, Int> =
                tokens(output.value, policy_id) |> dict.to_pairs

              expect InlineDatum(datum) = output.datum
              expect storage: Storage = datum
              Pair(pair.1st, storage)
            },
          )
        |> list.map(
            fn(pair) {
              let name: AssetName = pair.1st
              compare(name, )
              Pair(name, storage)
            },
          )
        )

    // Check that my asset is the smallest
    |> list.all

    True
  }

  else(_) {
    fail
  }
}
