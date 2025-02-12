use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use aiken/primitive/int
use cardano/transaction.{
  InlineDatum, OutputReference, Transaction, find_input, find_script_outputs,
}

pub type Storage {
  pairs: Pairs<ByteArray, Int>,
}

pub type Empty {
  Empty
}

validator baseline(_id: ByteArray) {
  spend(_d: Option<Storage>, _r: Empty, _u: OutputReference, _s: Transaction) {
    True
  }

  else(_) {
    fail
  }
}

fn from_input(datum: Option<Storage>) -> dict.Dict<ByteArray, Int> {
  expect Some(datum) = datum
  dict.from_pairs(datum.pairs)
}

fn from_output(
  self: Transaction,
  utxo: OutputReference,
) -> dict.Dict<ByteArray, Int> {
  // Find the contract's output (just one) and its datum
  expect Some(input) = find_input(self.inputs, utxo)
  expect Some(address) = input.output.reference_script
  expect [output] = find_script_outputs(self.outputs, address)
  expect InlineDatum(datum) = output.datum
  expect datum: Storage = datum
  dict.from_pairs(datum.pairs)
}

validator do_nothing(_id: ByteArray) {
  spend(
    datum: Option<Storage>,
    _redeemer: Empty,
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

validator access_one_real(_id: ByteArray) {
  spend(
    datum: Option<Storage>,
    _redeemer: Empty,
    utxo: OutputReference,
    self: Transaction,
  ) {
    let in = from_input(datum)
    let out = from_output(self, utxo)
    expect dict.get(in, "0") == dict.get(out, "0")
    True
  }

  else(_) {
    fail
  }
}

validator access_ten(_id: ByteArray) {
  spend(
    datum: Option<Storage>,
    _redeemer: Empty,
    utxo: OutputReference,
    self: Transaction,
  ) {
    let in = from_input(datum)
    let out = from_output(self, utxo)
    expect dict.get(in, "0") == dict.get(out, "0")
    expect dict.get(in, "1") == dict.get(out, "1")
    expect dict.get(in, "2") == dict.get(out, "3")
    expect dict.get(in, "3") == dict.get(out, "3")
    expect dict.get(in, "4") == dict.get(out, "4")
    expect dict.get(in, "5") == dict.get(out, "5")
    expect dict.get(in, "6") == dict.get(out, "6")
    expect dict.get(in, "7") == dict.get(out, "7")
    expect dict.get(in, "8") == dict.get(out, "8")
    expect dict.get(in, "9") == dict.get(out, "9")
    True
  }

  else(_) {
    fail
  }
}

validator access(_id: ByteArray, n: Int) {
  spend(
    datum: Option<Storage>,
    _redeemer: Empty,
    utxo: OutputReference,
    self: Transaction,
  ) {
    let in = from_input(datum)
    let out = from_output(self, utxo)

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
