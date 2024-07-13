use aiken/transaction.{ScriptContext}
use cardamove/move/prelude.{Unit}
use cardamove/move/state.{run_state}
use cardamove/move/storage.{Address, GuardState}

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
  script: fn(f) -> GuardState<g, Unit>,
  aggregate_storage: fn(List<(Address, s)>) -> g,
) -> Bool {
  // TODO: remove this dummy placeholder
  let dummy_placeholder: List<(Address, s)> =
    []
  // TODO: take list from all inputs
  let before: g = aggregate_storage(dummy_placeholder)
  // TODO: take list from all outputs
  let after: g = aggregate_storage(dummy_placeholder)
  let (assert, expected) = script(redeemer.funcall) |> run_state(before)
  when assert is {
    None -> False
    _ -> expected == after
  }
}
