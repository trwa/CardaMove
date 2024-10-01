use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardamove/io.{TransactionBoundary}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{OutputReference}

pub type SttPropagateDatum {
  started: Bool,
}

pub type KeyDatum {
  keys: List<AssetName>,
}

pub fn stt_mint(
  io: TransactionBoundary,
  stt_policy: PolicyId,
  output_reference: OutputReference,
) -> Bool {
  // Condition. The output_reference is spent during the transaction
  let user_inputs_with_output_reference =
    io.user_inputs
      |> list.filter(fn(input) { input.output_reference == output_reference })
  let script_inputs_with_output_reference =
    io.script_inputs
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
    io.user_outputs
      |> list.filter(
          fn(output) {
            list.any(output.utxo.assets, fn(asset) { asset.1st == stt_policy })
          },
        )
  expect [] == user_outputs_with_token
  // Condition. There is one output with "some" STT
  let script_outputs_with_token =
    io.script_outputs
      |> list.filter(
          fn(output) {
            list.any(output.utxo.assets, fn(asset) { asset.1st == stt_policy })
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
      list_one(assets, fn(asset) { asset.1st == stt_policy }),
      list_one(assets, fn(asset) { asset == (stt_policy, paid_to, 1) }),
    }
  // Condition. The SttPropagateDatum encodes that the process has not started
  expect SttPropagateDatum { started: False } = output.utxo.datum
  True
}

pub fn stt_spend(
  io: TransactionBoundary,
  stt_policy: PolicyId,
  stt_hash: ScriptHash,
  key_policy: PolicyId,
  key_hash: ScriptHash,
  max_size: Int,
) -> Bool {
  todo
}

pub fn key_mint(io: TransactionBoundary, stt_policy: PolicyId) -> Bool {
  some_token_in_script_inputs(io, stt_policy)
}

pub fn key_spend(io: TransactionBoundary, stt_policy: PolicyId) -> Bool {
  some_token_in_script_inputs(io, stt_policy)
}

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

fn some_token_in_script_inputs(
  io: TransactionBoundary,
  policy_id: PolicyId,
) -> Bool {
  [] != list.filter(
    io.script_inputs,
    fn(output) {
      list.any(output.utxo.assets, fn(asset) { asset.1st == policy_id })
    },
  )
}
