use aiken/collection/dict.{to_pairs}
use aiken/collection/list.{filter, find}
use aiken/collection/pairs
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use cardano/assets.{AssetName, PolicyId, quantity_of, tokens}
use cardano/transaction.{Datum, Output, OutputReference, Redeemer, Transaction}

validator start(output_reference: OutputReference) {
  mint(_redeemer: Redeemer, policy_id: PolicyId, transaction: Transaction) {
    todo @"Implement the mint validator"
  }

  else(_) {
    fail
  }
}

validator auth(
  start_policy_id: PolicyId,
  add_key_spend: ScriptHash,
  add_key_mint: ScriptHash,
) {
  spend(
    _datum: Option<Datum>,
    _redeemer: Redeemer,
    _output_reference: OutputReference,
    _transaction: Transaction,
  ) {
    todo @"Implement the spend validator"
  }

  else(_) {
    fail
  }
}

validator add_key(start_policy_id: PolicyId) {
  spend(
    _datum: Option<Datum>,
    _redeemer: Redeemer,
    _output_reference: OutputReference,
    _transaction: Transaction,
  ) {
    todo @"Implement the spend validator"
  }

  mint(_redeemer: Redeemer, _policy_id: PolicyId, _transaction: Transaction) {
    todo @"Implement the mint validator"
  }

  else(_) {
    fail
  }
}
