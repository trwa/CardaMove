use aiken/collection/dict.{to_pairs}
use aiken/collection/list.{filter, find}
use aiken/collection/pairs
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use cardano/assets.{AssetName, PolicyId, quantity_of, tokens}
use cardano/transaction.{Datum, Output, OutputReference, Redeemer, Transaction}

validator start(output_reference: OutputReference) {
  mint(_redeemer: Redeemer, policy_id: PolicyId, transaction: Transaction) {
    // 1. The output reference is consumed by the transaction
    expect Some(_input) =
      transaction.inputs
        |> list.find(fn(input) { input.output_reference == output_reference })
    // 2. There is a single NFT in the minted assets
    expect [Pair(asset_name, 1)]: Pairs<AssetName, Int> =
      transaction.mint |> tokens(policy_id) |> dict.to_pairs
    // 3. There is a single output with the minted NFT
    expect [output] =
      transaction.outputs
        |> list.filter(
            fn(output: Output) {
              1 == quantity_of(output.value, policy_id, "Auth")
            },
          )
    expect [] =
      transaction.outputs
        |> list.filter(
            fn(output: Output) {
              1 != quantity_of(output.value, policy_id, "Auth")
            },
          )
    // 4. The minted NFT is in a UTxO with an empty list as Datum
    expect InlineDatum(datum) = output.datum
    expect []: List<AssetName> = datum
    True
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
