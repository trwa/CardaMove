use aiken/dict.{Dict}
use aiken/list.{concat, filter, length, unique}
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{Address}
use cardamove/cardano/script.{script_address}
use cardamove/cardano/transaction.{transaction_inputs,
  transaction_outputs} as transaction2

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
  _edeemer: ContractRedeemer<f>,
  _context: ScriptContext,
  _aggregate_storage: fn(List<(Address, s)>) -> g,
  _run: fn(g) -> (Bool, g),
) -> Bool {
  True
}
