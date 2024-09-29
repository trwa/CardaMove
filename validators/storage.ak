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
// Datums
// =====================================================================================================================

pub type MainDatum {
  started: Bool,
}

pub type KeyDatum {
  keys: List<AssetName>,
}

// =====================================================================================================================
// Types
// =====================================================================================================================

type MainInput {
  started: Bool,
}

type MainOutput =
  MainInput

type KeyInput {
  ordered_keys: List<AssetName>,
  lower_key: AssetName,
  upper_key: AssetName,
}

type KeyOutput {
  WithSplit { left: KeyInput, right: KeyInput }
  WithoutSplit { middle: KeyInput }
}

// =====================================================================================================================
// Constants
// =====================================================================================================================

const stt_name: AssetName = "STT"

// =====================================================================================================================
// Functions Old
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

fn inputs_with_tokens_given_policy(
  transaction: Transaction,
  policy_id: PolicyId,
) -> Pairs<Input, Pairs<AssetName, Int>> {
  transaction.inputs
    |> list.map(
        fn(input: Input) {
          Pair(input, input.output.value |> tokens(policy_id) |> dict.to_pairs)
        },
      )
    |> list.filter(fn(Pair(_, tokens)) { tokens != [] })
}

fn outputs_with_tokens_given_policy(
  transaction: Transaction,
  policy_id: PolicyId,
) -> Pairs<Output, Pairs<AssetName, Int>> {
  transaction.outputs
    |> list.map(
        fn(output: Output) {
          Pair(output, output.value |> tokens(policy_id) |> dict.to_pairs)
        },
      )
    |> list.filter(fn(Pair(_, tokens)) { tokens != [] })
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
// Functions
// =====================================================================================================================

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
    // Condition. Datum is a MainDatum
    expect MainDatum { started: False } = datum
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
    _datum: Option<Datum>,
    _redeemer: Redeemer,
    output_reference: OutputReference,
    transaction: Transaction,
  ) {
    // =================================================================================================================
    // Condition. STT in the input
    // =================================================================================================================
    expect [
      Pair(Input {
        output: Output {
          address: Address { payment_credential: Script(main_hash1), .. },
          datum: InlineDatum(main_datum1),
          ..
        },
        ..
      },
      [Pair(stt_name1, stt_amount1)]),
    ]: Pairs<Input, Pairs<AssetName, Int>> =
      inputs_with_tokens_given_policy(transaction, stt_policy)
    expect stt_name1 == stt_name
    expect stt_amount1 == 1
    expect MainDatum { started: main_started1 } = main_datum1
    // TODO: main_hash1
    // =================================================================================================================
    // Condition. STT in the output
    // =================================================================================================================
    expect [
      Pair(Output {
        address: Address { payment_credential: Script(main_hash2), .. },
        datum: InlineDatum(main_datum2),
        ..
      },
      [Pair(stt_name2, stt_amount2)]),
    ]: Pairs<Output, Pairs<AssetName, Int>> =
      outputs_with_tokens_given_policy(transaction, stt_policy)
    expect stt_name2 == stt_name
    expect stt_amount2 == 1
    expect MainDatum { started: main_started2 } = main_datum2
    // TODO: main_hash2
    // =================================================================================================================
    // Condition. Key tokens in the input
    // =================================================================================================================
    expect [
      Pair(Input {
        output: Output {
          address: Address { payment_credential: Script(key_hash1), .. },
          datum: InlineDatum(key_datum1),
          ..
        },
        ..
      },
      [Pair(key_name1a, key_amount1a), Pair(key_name1b, key_amount1b)]),
    ]: Pairs<Input, Pairs<AssetName, Int>> =
      inputs_with_tokens_given_policy(transaction, key_mint)
    expect bytearray.compare(key_name1a, key_name1b) != Equal
    expect key_amount1a == 1
    expect key_amount1b == 1
    expect KeyDatum { keys: key_keys1 } = key_datum1
    expect key_hash1 == key_spend
    // =================================================================================================================
    // Condition. Key tokens in the output
    // =================================================================================================================
    let key_outputs = outputs_with_tokens_given_policy(transaction, key_mint)
    let _ =
      when key_outputs is {
        // NO SPLIT
        [
          // Key propagation
          Pair(Output {
            address: Address { payment_credential: Script(key_hash2), .. },
            datum: InlineDatum(key_datum2),
            ..
          },
          // New key
          [Pair(key_name2a, key_amount2a), Pair(key_name2b, key_amount2b)]),
          Pair(Output {
            address: Address { payment_credential: Script(key_hash2), .. },
            datum: InlineDatum(key_datum2),
            ..
          },
          [Pair(key_name2a, key_amount2a), Pair(key_name2b, key_amount2b)]),
        ] -> True
        // Alternative. Split happened
        [
          // Left
          Pair(Output {
            address: Address { payment_credential: Script(key_hash2l), .. },
            datum: InlineDatum(key_datum2l),
            ..
          },
          [Pair(key_name2la, key_amount2la), Pair(key_name2lb, key_amount2lb)]),
          // Right
          Pair(Output {
            address: Address { payment_credential: Script(key_hash2r), .. },
            datum: InlineDatum(key_datum2r),
            ..
          },
          [Pair(key_name1a, key_amount1a), Pair(key_name1b, key_amount1b)]),
        ] -> True
        _ -> False
      }
    // -----------------------------------------------------------------------------------------------------------------
    // STT validation
    // -----------------------------------------------------------------------------------------------------------------
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
    //expect Some(KeyDatum { keys }) = datum
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
