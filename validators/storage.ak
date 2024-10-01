use aiken/collection/dict.{to_pairs}
use aiken/collection/list.{filter, find}
use aiken/collection/pairs
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/primitive/bytearray
use cardano/address.{Address, Script, StakeCredential, VerificationKey}
use cardano/assets.{AssetName, PolicyId, flatten, quantity_of, tokens}
use cardano/transaction.{
  Datum, InlineDatum, Input, Mint, Output, OutputReference, Redeemer, Spend,
  Transaction,
}

// =====================================================================================================================
// Transaction Raw I/O
// =====================================================================================================================

type Asset =
  (PolicyId, AssetName, Int)

type UserUTxO {
  user_address: VerificationKeyHash,
  assets: List<Asset>,
}

type ScriptUTxO<d> {
  script_address: ScriptHash,
  datum: d,
  assets: List<Asset>,
}

type UserInput {
  output_reference: OutputReference,
  utxo: UserUTxO,
}

type ScriptInput<d> {
  output_reference: OutputReference,
  utxo: ScriptUTxO<d>,
}

type UserOutput {
  utxo: UserUTxO,
}

type ScriptOutput<d> {
  utxo: ScriptUTxO<d>,
}

type Boundary {
  user_inputs: List<UserInput>,
  user_outputs: List<UserOutput>,
  script_inputs: List<ScriptInput<Datum>>,
  script_outputs: List<ScriptOutput<Datum>>,
}

fn get_user_inputs(self: Transaction) -> List<UserInput> {
  self.inputs
    |> list.filter_map(
        fn(input: Input) {
          when input.output.address.payment_credential is {
            VerificationKey(hash) ->
              Some(
                UserInput {
                  output_reference: input.output_reference,
                  utxo: UserUTxO {
                    user_address: hash,
                    assets: flatten(input.output.value),
                  },
                },
              )
            _ -> None
          }
        },
      )
}

fn get_script_inputs(self: Transaction) -> List<ScriptInput<Data>> {
  self.inputs
    |> list.filter_map(
        fn(input: Input) {
          when input.output.address.payment_credential is {
            Script(hash) -> {
              expect InlineDatum(data) = input.output.datum
              Some(
                ScriptInput {
                  output_reference: input.output_reference,
                  utxo: ScriptUTxO {
                    script_address: hash,
                    assets: flatten(input.output.value),
                    datum: data,
                  },
                },
              )
            }
            _ -> None
          }
        },
      )
}

fn get_user_outputs(self: Transaction) -> List<UserOutput> {
  self.outputs
    |> list.filter_map(
        fn(output: Output) {
          when output.address.payment_credential is {
            VerificationKey(hash) ->
              Some(
                UserOutput {
                  utxo: UserUTxO {
                    user_address: hash,
                    assets: flatten(output.value),
                  },
                },
              )
            _ -> None
          }
        },
      )
}

fn get_script_outputs(self: Transaction) -> List<ScriptOutput<Data>> {
  self.outputs
    |> list.filter_map(
        fn(output: Output) {
          when output.address.payment_credential is {
            Script(hash) -> {
              expect InlineDatum(data) = output.datum
              Some(
                ScriptOutput {
                  utxo: ScriptUTxO {
                    script_address: hash,
                    assets: flatten(output.value),
                    datum: data,
                  },
                },
              )
            }
            _ -> None
          }
        },
      )
}

// =====================================================================================================================
// Datums
// =====================================================================================================================

pub type SttPropagateDatum {
  started: Bool,
}

pub type KeyDatum {
  keys: List<AssetName>,
}

// =====================================================================================================================
// From Raw I/O to Typed I/O
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
  expect SttPropagateDatum { started: main_started1 } = main_datum1
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
  expect SttPropagateDatum { started: main_started2 } = main_datum2
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
    // Condition. The output_reference is spent during the transaction
    let user_inputs_with_output_reference =
      get_user_inputs(transaction)
        |> list.filter(fn(input) { input.output_reference == output_reference })
    let script_inputs_with_output_reference =
      get_script_inputs(transaction)
        |> list.filter(fn(input) { input.output_reference == output_reference })
    expect or {
        and {
          [] != user_inputs_with_output_reference,
          [] == script_inputs_with_output_reference,
        },
        and {
          [] == user_inputs_with_output_reference,
          [] != script_inputs_with_output_reference,
        },
      }
    // Condition. There is no STT paid to any user in the outputs
    let user_outputs_with_token =
      get_user_outputs(transaction)
        |> list.filter(
            fn(output) {
              list.any(output.utxo.assets, fn(asset) { asset.1st == policy_id })
            },
          )
    expect [] == user_outputs_with_token
    // Condition. There is one output with "some" STT
    let script_outputs_with_token =
      get_script_outputs(transaction)
        |> list.filter(
            fn(output) {
              list.any(output.utxo.assets, fn(asset) { asset.1st == policy_id })
            },
          )
    expect [output] = script_outputs_with_token
    // Condition. The output has exactly 1 STT with asset_name = address of the script it is paid to
    let assets = output.utxo.assets
    // From https://aiken-lang.github.io/stdlib/aiken/crypto.html#Hash
    // A Hash is nothing more than a ByteArray, but it carries extra information for readability.
    // On-chain, any hash digest value is represented as a plain ‘ByteArray’.
    // Though in practice, hashes come from different sources and have different semantics.
    let paid_to: ByteArray = output.utxo.script_address
    let list_one =
      fn(xs, pred) { 1 == list.count(xs, pred) }
    expect and {
        list_one(assets, fn(asset) { asset.1st == policy_id }),
        list_one(assets, fn(asset) { asset == (policy_id, paid_to, 1) }),
      }
    // Condition. The SttPropagateDatum encodes that the process has not started
    expect SttPropagateDatum { started: False } = output.utxo.datum
    True
  }

  else(_) {
    fail
  }
}

validator stt_propagate(
  stt_policy: PolicyId,
  key_mint: PolicyId,
  key_spend: ScriptHash,
  _max_split: Int,
) {
  spend(
    _datum: Option<SttPropagateDatum>,
    _redeemer: Redeemer,
    _output_reference: OutputReference,
    transaction: Transaction,
  ) {
    let user_inputs = get_user_inputs(transaction)
    let user_outputs = get_user_outputs(transaction)
    let script_inputs = get_script_inputs(transaction)
    let script_outputs = get_script_outputs(transaction)

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
