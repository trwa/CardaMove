use aiken/collection/dict
use cardano/transaction.{
  InlineDatum, OutputReference, Transaction, find_input, find_script_outputs,
}

pub type Datum {
  pairs: Pairs<ByteArray, Int>,
}

validator monolithic_0 {
  spend(
    datum: Option<Datum>,
    _redeemer: Data,
    _utxo: OutputReference,
    _self: Transaction,
  ) {
    expect Some(datum) = datum
    expect _ = dict.from_pairs(datum.pairs)
    True
  }

  else(_) {
    fail
  }
}

validator monolithic_1 {
  spend(
    datum: Option<Datum>,
    _redeemer: Data,
    utxo: OutputReference,
    self: Transaction,
  ) {
    // Unpack the Datum 
    expect Some(datum) = datum
    let kv_in = dict.from_pairs(datum.pairs)

    // Find the contract's output (just one) and its datum
    expect Some(input) = find_input(self.inputs, utxo)
    expect Some(address) = input.output.reference_script
    expect [output] = find_script_outputs(self.outputs, address)
    expect InlineDatum(datum) = output.datum
    expect datum: Datum = datum
    let kv_out = dict.from_pairs(datum.pairs)

    // Compare the two dictionaries
    expect kv_in == kv_out
    True
  }

  else(_) {
    fail
  }
}
