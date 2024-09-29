use aiken/collection/dict.{to_pairs}
use aiken/collection/list.{filter, find}
use aiken/collection/pairs
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/primitive/bytearray
use cardano/address.{Address, Script, StakeCredential}
use cardano/assets.{AssetName, PolicyId, quantity_of, tokens}
use cardano/transaction.{
  Datum, InlineDatum, Input, Mint, Output, OutputReference, Redeemer, Spend,
  Transaction,
}

// =====================================================================================================================
// Types
// =====================================================================================================================

// Cosa voglio predicare sugli input?
// Nello stt_create, nulla
// Nello stt_propagate, che gli input contengano uno stt giusto
// Nello key_add, che gli input contengano uno stt giusto
// Nello key_split, che gli input contengano uno stt giusto
// Nello key_spend, che gli input contengano un singolo utxo con due asset dello key_split

pub type StateThreadDatum {
  started: Bool,
}

pub type KeySplitDatum {
  keys: List<AssetName>,
}

// =====================================================================================================================
// Constants
// =====================================================================================================================

const stt_name: AssetName = "STT"

// =====================================================================================================================
// Functions
// =====================================================================================================================

fn utxo_is_spent(transaction: Transaction, utxo: OutputReference) -> Bool {
  when
    transaction.inputs
      |> list.find(fn(input) { input.output_reference == utxo })
  is {
    Some(_) -> True
    None -> False
  }
}

fn token_is_minted(
  transaction: Transaction,
  policy_id: PolicyId,
  asset_name: AssetName,
  amount: Int,
) -> Bool {
  let tokens = transaction.mint |> tokens(policy_id) |> dict.to_pairs
  when tokens is {
    [Pair(name, amt)] -> amount == amt && name == asset_name
    _ -> False
  }
}

fn token_in_inputs(
  transaction: Transaction,
  policy_id: PolicyId,
  asset_name: AssetName,
  amount: Int,
) -> Bool {
  let amounts: List<Int> =
    transaction.inputs
      |> list.map(
          fn(input: Input) {
            quantity_of(input.output.value, policy_id, asset_name)
          },
        )
  amount == list.foldl(amounts, 0, with: fn(amt, sum) { amt + sum })
}

fn token_in_outputs(
  transaction: Transaction,
  policy_id: PolicyId,
  asset_name: AssetName,
  amount: Int,
) -> Bool {
  let amounts: List<Int> =
    transaction.outputs
      |> list.map(
          fn(output: Output) {
            quantity_of(output.value, policy_id, asset_name)
          },
        )
  amount == list.foldl(amounts, 0, with: fn(amt, sum) { amt + sum })
}

fn tokens_with_policy(
  transaction: Transaction,
  output_reference: OutputReference,
  policy_id: PolicyId,
) -> List<Pair<AssetName, Int>> {
  let inputs =
    transaction.inputs
      |> list.find(fn(input) { input.output_reference == output_reference })
  when inputs is {
    Some(input) -> input.output.value |> tokens(policy_id) |> dict.to_pairs
    None -> []
  }
}

fn inputs_with_token(
  transaction: Transaction,
  policy_id: PolicyId,
  asset_name: AssetName,
  amount: Int,
) -> List<Input> {
  transaction.inputs
    |> list.filter(
        fn(input: Input) {
          amount == quantity_of(input.output.value, policy_id, asset_name)
        },
      )
}

fn outputs_with_token(
  transaction: Transaction,
  policy_id: PolicyId,
  asset_name: AssetName,
  amount: Int,
) -> List<Output> {
  transaction.outputs
    |> list.filter(
        fn(output: Output) {
          amount == quantity_of(output.value, policy_id, asset_name)
        },
      )
}

// =====================================================================================================================
// Validators
// =====================================================================================================================

validator stt(output_reference: OutputReference) {
  mint(_redeemer: Redeemer, policy_id: PolicyId, transaction: Transaction) {
    // Condition. The output is spent in the transaction
    expect utxo_is_spent(transaction, output_reference)
    // Condition. 1 STT is minted
    expect token_is_minted(transaction, policy_id, stt_name, 1)
    // Condition. 1 STT is in the outputs
    expect token_in_outputs(transaction, policy_id, stt_name, 1)
    // Binding. STT is in a UTxO
    expect [output] = outputs_with_token(transaction, policy_id, stt_name, 1)
    // Binding. UTxO has an InlineDatum
    expect InlineDatum(datum) = output.datum
    // Condition. Datum is a StateThreadDatum with started == False
    expect StateThreadDatum { started: False } = datum
    True
  }

  else(_) {
    fail
  }
}

validator main(
  stt_policy: PolicyId,
  key_mint: PolicyId,
  key_spend: ScriptHash,
  _max_split: Int,
) {
  spend(
    _datum: Option<StateThreadDatum>,
    _redeemer: Redeemer,
    output_reference: OutputReference,
    transaction: Transaction,
  ) {
    // Condition. STT is in the inputs
    expect token_in_inputs(transaction, stt_policy, stt_name, 1)
    // Condition. STT is in the outputs
    expect token_in_outputs(transaction, stt_policy, stt_name, 1)
    // Condition. STT is in its own input
    expect [Pair(_, 1)] =
      tokens_with_policy(transaction, output_reference, stt_policy)
    // Condition. The key_new mint is in involved in the transaction
    expect transaction.redeemers |> pairs.has_key(Mint(key_mint))
    // Condition. The key_propagate script is involved in the transaction
    expect [_input] =
      transaction.inputs
        |> list.filter(
            fn(input) {
              input.output.address == Address {
                payment_credential: Script(key_spend),
                stake_credential: None,
              }
            },
          )
    True
  }

  else(_) {
    fail
  }
}

validator key(stt_policy: PolicyId) {
  mint(_redeemer: Redeemer, _policy_id: PolicyId, transaction: Transaction) {
    // Condition. STT is in the inputs
    expect token_in_inputs(transaction, stt_policy, stt_policy, 1)
    // Condition. STT is in the outputs
    expect token_in_outputs(transaction, stt_policy, stt_name, 1)
    // Binding. Minted tokens
    //let minted: Pairs<AssetName, Int> =      transaction.mint |> tokens(policy_id) |> dict.to_pairs
    //when minted is {
    // Alternative. One key is minted (no split)
    //[Pair(_, 1)] ->
    // TODO Condition. 
    //True
    // Alternative. Two keys are minted (split)
    //[Pair(name, 1), Pair(key1, 1), Pair(key2, 1)] ->
    // TODO Condition.
    //True
    //_ -> False
    //}
    True
  }

  spend(
    _datum: Option<Datum>,
    _redeemer: Redeemer,
    _output_reference: OutputReference,
    transaction: Transaction,
  ) {
    // Condition. STT is in the inputs
    expect token_in_inputs(transaction, stt_policy, stt_name, 1)
    // Condition. STT is in the outputs
    expect token_in_outputs(transaction, stt_policy, stt_name, 1)
    // Condition. Datum is present
    //expect Some(KeySplitDatum { keys }) = datum
    // Condition. Existing keys are lexicographically ordered
    //expect keys == list.sort(keys, bytearray.compare)
    // Condition. The Output being spent has 2 tokens of key_policy
    //expect [Pair(key1, 1),
    //  Pair(key2, 1)]: Pairs<AssetName, Int> =
    //  tokens_with_policy(transaction, output_reference, key_policy)
    // Binding. Lower and upper keys
    //let lowr =
    //  if bytearray.compare(key1, key2) == Less {
    //    key1
    //  } else {
    //    key2
    //  }
    //let uppr =
    //  if bytearray.compare(key1, key2) == Less {
    //    key2
    //  } else {
    //    key1
    //  }
    //when keys is {
    //  [] ->
    //    // TODO
    //    True
    //  _ -> {
    //    // Binding. First and last keys
    //    expect Some(first) = list.head(keys)
    //   expect Some(last) = list.last(keys)
    //    // Condition. Lowr <= first key
    //    expect bytearray.compare(lowr, first) != Greater
    //    // Condition. Last key <= uppr
    //    expect bytearray.compare(last, uppr) != Greater
    //    // Condition. Number of keys is less than max_size
    //   expect list.length(keys) <= max_size
    //   True
    // }
    //}
    True
  }

  else(_) {
    fail
  }
}
