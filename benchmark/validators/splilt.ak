use aiken/collection/dict
use aiken/collection/list
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction, find_input,
  find_script_outputs,
}

pub type Storage {
  value: Int,
}

pub type TokenName {
  name: ByteArray,
}

pub type Empty {
  Empty
}

validator token(_id: ByteArray) {
  mint(_r: Empty, _p: PolicyId, _s: Transaction) {
    True
  }

  else(_) {
    fail
  }
}

validator baseline(_id: ByteArray) {
  spend(_d: Option<Storage>, _r: Empty, _u: OutputReference, _s: Transaction) {
    True
  }

  else(_) {
    fail
  }
}

fn find_inputs(self: Transaction) -> List<Output> {
  self.inputs
    |> list.map(fn(input: Input) { input.output })
}

fn find_outputs(self: Transaction) -> List<Output> {
  self.outputs
}
