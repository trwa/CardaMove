use aiken/collection/dict.{to_pairs}
use aiken/collection/list.{filter, find}
use aiken/collection/pairs
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use cardano/assets.{AssetName, PolicyId, quantity_of, tokens}
use cardano/transaction.{
  Datum, InlineDatum, Output, OutputReference, Redeemer, Transaction,
}

// =====================================================================================================================
// Datums
// =====================================================================================================================

type StateThreadDatum {
  started: Bool,
}

type AddressList =
  List<ScriptHash>

// Mi servono 3 datim
// 1. Quello dello startscript e' totalmente ininfluente, quindi e' il generico Datum
// 

const stt_name = "STT"

fn stt__utxo_is_spent(transaction: Transaction, utxo: OutputReference) -> Bool {
  when
    transaction.inputs
      |> list.find(fn(input) { input.output_reference == utxo })
  is {
    Some(_) -> True
    None -> False
  }
}

fn stt__one_token_is_minted(
  transaction: Transaction,
  policy_id: PolicyId,
) -> Bool {
  let pairs = transaction.mint |> tokens(policy_id) |> dict.to_pairs
  when pairs is {
    [Pair(asset_name, amount)] -> amount == 1 && stt_name == asset_name
    _ -> False
  }
}

fn stt__correct_datum(transaction: Transaction, policy_id: PolicyId) -> Bool {
  let outputs =
    transaction.outputs
      |> list.filter(
          fn(output: Output) {
            1 == quantity_of(output.value, policy_id, stt_name)
          },
        )
  when outputs is {
    [output] ->
      when output.datum is {
        InlineDatum(datum) -> {
          expect StateThreadDatum { started: False } = datum
          True
        }
        _ -> False
      }
    _ -> False
  }
}

validator create_stt(output_reference: OutputReference) {
  mint(_redeemer: Redeemer, policy_id: PolicyId, transaction: Transaction) {
    and {
      stt__utxo_is_spent(transaction, output_reference),
      stt__one_token_is_minted(transaction, policy_id),
      stt__correct_datum(transaction, policy_id),
    }
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
