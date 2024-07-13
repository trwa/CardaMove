use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{Address}
use cardamove/move/prelude.{Unit}
use cardamove/move/state.{run_state}
use cardamove/move/storage.{GuardState}

// s is expected to be a record of optional fields.
pub type ContractDatum<s> {
  local_storage: (Address, s),
}

// f is expected to represent the different possible function calls
// TODO: in reality it should be a single script, so maybe just the arguments to it!
pub type ContractRedeemer<f> {
  funcall: f,
}

// Fully validate the contract
pub fn contract_validate(
  _datum: ContractDatum<s>,
  redeemer: ContractRedeemer<f>,
  _context: ScriptContext,
  aggregate_storage: fn(List<(Address, s)>) -> g,
  script: fn(f) -> GuardState<g, Unit>,
) -> Bool {
  // TODO: take list from all inputs
  let before = aggregate_storage([])
  // TODO: take list from all outputs
  let after = aggregate_storage([])
  let (assert, expected) = script(redeemer.funcall) |> run_state(before)
  when assert is {
    None -> False
    _ -> expected == after
  }
}
