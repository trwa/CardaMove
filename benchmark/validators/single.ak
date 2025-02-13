use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use aiken/primitive/int
use cardano/transaction.{
  InlineDatum, OutputReference, Transaction, find_input, find_script_outputs,
}

// =====================================================================================================================
// Types
// =====================================================================================================================

pub type Storage {
  pairs: Pairs<ByteArray, Int>,
}

pub type Empty {
  Empty
}

// =====================================================================================================================
// Helpers
// =====================================================================================================================

fn dict_from_input(datum: Option<Storage>) -> dict.Dict<ByteArray, Int> {
  expect Some(datum) = datum
  dict.from_pairs(datum.pairs)
}

fn dict_from_output(
  self: Transaction,
  utxo: OutputReference,
) -> dict.Dict<ByteArray, Int> {
  expect Some(input) = find_input(self.inputs, utxo)
  expect Some(address) = input.output.reference_script
  expect [output] = find_script_outputs(self.outputs, address)
  expect InlineDatum(datum) = output.datum
  expect datum: Storage = datum
  dict.from_pairs(datum.pairs)
}

// =====================================================================================================================
// Validators
// =====================================================================================================================

validator baseline(_id: ByteArray) {
  spend(_d: Option<Storage>, _r: Empty, _u: OutputReference, _s: Transaction) {
    True
  }

  else(_) {
    fail
  }
}

validator read(_id: ByteArray, n: Int) {
  spend(
    datum: Option<Storage>,
    _redeemer: Empty,
    utxo: OutputReference,
    self: Transaction,
  ) {
    let in = dict_from_input(datum)
    let out = dict_from_output(self, utxo)

    expect _ =
      list.range(0, n)
        |> list.map(int.to_string)
        |> list.map(bytearray.from_string)
        |> list.for_each(fn(i) { expect dict.get(in, i) == dict.get(out, i) })
    True
  }

  else(_) {
    fail
  }
}
