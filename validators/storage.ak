use aiken/crypto.{ScriptHash}
use cardamove/io.{get_boundary}
use cardamove/validators.{
  KeyDatum, SttPropagateDatum, key_mint, key_spend, stt_mint, stt_spend,
}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Redeemer, Transaction}

validator stt1(output_reference: OutputReference) {
  mint(_redeemer: Redeemer, policy_id: PolicyId, transaction: Transaction) {
    get_boundary(transaction) |> stt_mint(policy_id, output_reference)
  }

  else(_) {
    fail
  }
}

validator stt2(
  stt_policy: PolicyId,
  stt_hash: ScriptHash,
  key_policy: PolicyId,
  key_hash: ScriptHash,
  max_size: Int,
) {
  spend(
    _datum: Option<SttPropagateDatum>,
    _redeemer: Redeemer,
    _output_reference: OutputReference,
    transaction: Transaction,
  ) {
    get_boundary(transaction)
      |> stt_spend(stt_policy, stt_hash, key_policy, key_hash, max_size)
  }

  else(_) {
    fail
  }
}

validator key(stt_policy: PolicyId) {
  mint(_redeemer: Redeemer, _policy_id: PolicyId, transaction: Transaction) {
    get_boundary(transaction) |> key_mint(stt_policy)
  }

  spend(
    _datum: Option<KeyDatum>,
    _redeemer: Redeemer,
    _output_reference: OutputReference,
    transaction: Transaction,
  ) {
    get_boundary(transaction) |> key_spend(stt_policy)
  }

  else(_) {
    fail
  }
}
