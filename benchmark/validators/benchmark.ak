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

validator multi(_id: ByteArray, _policy_id: ByteArray, _c: Int, _n: Int) {
  spend(
    datum: Option<Storage>,
    _redeemer: Empty,
    _output_reference: OutputReference,
    _self: Transaction,
  ) {
    // Force the validator to read the datum
    expect Some(_) = datum
    True
  }

  else(_) {
    fail
  }
}
