use aiken/collection/dict
use cardano/transaction.{
  InlineDatum, OutputReference, Transaction, find_input, find_script_outputs,
}

pub type Storage {
  pairs: List<Pair<ByteArray, Int>>,
}

pub type Redeemer {
  Redeemer
}

validator benchmark_0(_id: ByteArray) {
  spend(_d, _r, _u, _s) {
    trace @"datum": @"halting"
    False
  }

  else(_) {
    fail
  }
}

validator benchmark_1(_id: ByteArray) {
  spend(
    datum: Option<Storage>,
    _redeemer: Redeemer,
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

validator benchmark_2(_id: ByteArray) {
  spend(
    datum: Option<Storage>,
    _redeemer: Redeemer,
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
    expect datum: Storage = datum
    let kv_out = dict.from_pairs(datum.pairs)

    // Compare only the first two keys
    expect dict.get(kv_in, "a") == dict.get(kv_out, "a")
    True
  }

  else(_) {
    fail
  }
}
