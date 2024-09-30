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
  script_address: ScriptHash,
  has_started: Bool,
}

type MainOutput =
  MainInput

type KeyInput {
  script_address: ScriptHash,
  ordered_keys: List<AssetName>,
  lower_key: AssetName,
  upper_key: AssetName,
}

type KeyOutput {
  WithSplit {
    left_output: KeyInput,
    right_output: KeyInput,
    minted_key: AssetName,
  }
  WithoutSplit { only_output: KeyInput, minted_key: AssetName }
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
// Helper functions
// =====================================================================================================================

fn to_main_input(inputs: Pairs<Input, Pairs<AssetName, Int>>) -> MainInput {
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
  ] = inputs
  expect stt_name1 == stt_name
  expect stt_amount1 == 1
  expect MainDatum { started: main_started1 } = main_datum1
  // TODO: main_hash1
  MainInput { script_address: main_hash1, has_started: main_started1 }
}

fn to_main_output(outputs: Pairs<Output, Pairs<AssetName, Int>>) -> MainOutput {
  expect [
    Pair(Output {
      address: Address { payment_credential: Script(main_hash2), .. },
      datum: InlineDatum(main_datum2),
      ..
    },
    [Pair(stt_name2, stt_amount2)]),
  ] = outputs
  expect stt_name2 == stt_name
  expect stt_amount2 == 1
  expect MainDatum { started: main_started2 } = main_datum2
  // TODO: main_hash2
  MainInput { script_address: main_hash2, has_started: main_started2 }
}

fn to_key_input(inputs: Pairs<Input, Pairs<AssetName, Int>>) -> KeyInput {
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
  ] = inputs
  expect bytearray.compare(key_name1a, key_name1b) != Equal
  expect key_amount1a == 1
  expect key_amount1b == 1
  expect KeyDatum { keys: key_keys1 } = key_datum1
  // TODO: key_hash1
  KeyInput {
    script_address: key_hash1,
    ordered_keys: key_keys1,
    lower_key: key_name1a,
    upper_key: key_name1b,
  }
}

fn to_key_output(outputs: Pairs<Output, Pairs<AssetName, Int>>) -> KeyOutput {
  todo @"to_key_output"
}

// =====================================================================================================================
// Validators
// =====================================================================================================================

validator stt_make(output_reference: OutputReference) {
  mint(_redeemer: Redeemer, policy_id: PolicyId, transaction: Transaction) {
    // Condition. The output is spent in the transaction
    expect utxo_is_spent(transaction, output_reference)
    // Condition. 1 STT is minted_key
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
  spend(_datum, _redeemer, _output_reference, transaction: Transaction) {
    let main_input =
      inputs_with_tokens_given_policy(transaction, stt_policy)
        |> to_main_input()
    let main_output =
      outputs_with_tokens_given_policy(transaction, stt_policy)
        |> to_main_output()
    let key_output =
      outputs_with_tokens_given_policy(transaction, key_mint) |> to_key_output()
    if !main_input.has_started {
      expect [] == inputs_with_tokens_given_policy(transaction, key_mint)
      True
    } else {
      let key_input =
        inputs_with_tokens_given_policy(transaction, key_mint)
          |> to_key_input()
      expect key_input.script_address == key_spend
      when key_output is {
        WithoutSplit(only_output, minted_key) -> {
          // Condition. Output is Input + Minted
          expect
            only_output.ordered_keys == list.sort(
              list.push(key_input.ordered_keys, minted_key),
              bytearray.compare,
            )
          expect
            list.length(only_output.ordered_keys) == list.length(
              key_input.ordered_keys,
            ) + 1
          // Condition. Middle is paid to the script
          expect only_output.script_address == key_spend
          True
        }
        WithSplit(left_output, right_output, minted_key) -> {
          // Condition. Output is Input + Minted
          expect
            list.concat(left_output.ordered_keys, right_output.ordered_keys) == list.sort(
              list.push(key_input.ordered_keys, minted_key),
              bytearray.compare,
            )
          expect
            list.length(left_output.ordered_keys) == list.length(
              right_output.ordered_keys,
            ) + 1 || list.length(left_output.ordered_keys) + 1 == list.length(
              right_output.ordered_keys,
            )
          expect
            list.length(left_output.ordered_keys) == list.length(
              key_input.ordered_keys,
            ) || list.length(right_output.ordered_keys) == list.length(
              key_input.ordered_keys,
            )
          // Condition. Left and right_output are paid to the script
          expect left_output.script_address == key_spend
          expect right_output.script_address == key_spend
          True
        }
      }
    }
  }

  else(_) {
    fail
  }
}

validator key_add(stt_policy: PolicyId) {
  mint(_redeemer, _policy_id, transaction: Transaction) {
    // Condition. STT is in the inputs
    expect token_in_inputs(transaction, stt_policy, stt_policy, 1)
    // Condition. STT is in the outputs
    expect token_in_outputs(transaction, stt_policy, stt_name, 1)
    True
  }

  spend(_datum, _redeemer, _output_reference, transaction: Transaction) {
    // Condition. STT is in the inputs
    expect token_in_inputs(transaction, stt_policy, stt_name, 1)
    // Condition. STT is in the outputs
    expect token_in_outputs(transaction, stt_policy, stt_name, 1)
    True
  }

  else(_) {
    fail
  }
}
