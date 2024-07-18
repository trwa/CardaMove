use aiken/list.{difference, length, map, unique}
use aiken/transaction.{InlineDatum, Input, Output, ScriptContext}
use cardamove/cardano/transaction.{transaction_inputs,
  transaction_outputs} as cardano_transaction
use cardamove/move/prelude.{Address}
use cardamove/move/state.{run_state}
use cardamove/move/storage.{MoveState}

// s is state_expected to be a record of optional fields.
pub type ContractDatum<s> {
  address: Address,
  data: s,
}

// f is state_expected to represent the different possible function calls
// TODO: in reality it should be a single script, so maybe just the arguments to it!
pub type ContractRedeemer<f> {
  dispatch: f,
}

fn all_input_inline_datums(
  ctx: ScriptContext,
  downcast: fn(Data) -> ContractDatum<s>,
) -> List<ContractDatum<s>> {
  map(
    transaction_inputs(ctx),
    fn(i) {
      expect InlineDatum(data) = i.1st.output.datum
      downcast(data)
    },
  )
}

fn all_output_inline_datums(
  ctx: ScriptContext,
  downcast: fn(Data) -> ContractDatum<s>,
) -> List<ContractDatum<s>> {
  map(
    transaction_outputs(ctx),
    fn(o) {
      expect InlineDatum(data) = o.datum
      downcast(data)
    },
  )
}

fn all_input_addresses(ctx: ScriptContext) {
  map(transaction_inputs(ctx), fn(i) { i.1st.output.address })
}

fn all_output_addresses(ctx: ScriptContext) {
  map(transaction_outputs(ctx), fn(o) { o.address })
}

pub fn contract_validate(
  _datum: ContractDatum<s>,
  redeemer: ContractRedeemer<f>,
  context: ScriptContext,
  script: fn(f) -> MoveState<g, Void>,
  storage: fn(List<ContractDatum<s>>) -> g,
  downcast: fn(Data) -> ContractDatum<s>,
) -> Bool {
  let input_datums = all_input_inline_datums(context, downcast)
  let output_datums = all_output_inline_datums(context, downcast)
  let input_keys = map(input_datums, fn(i) { i.address })
  let output_keys = map(output_datums, fn(o) { o.address })
  expect and {
      length(input_datums) == length(output_datums),
      length(input_keys) == length(unique(input_keys)),
      length(output_keys) == length(unique(output_keys)),
      difference(input_keys, output_keys) == [],
    }
  let input_addresses = unique(all_input_addresses(context))
  let output_addresses = unique(all_output_addresses(context))
  expect and {
      length(input_addresses) == 1,
      input_addresses == output_addresses,
    }
  let state_before: g = storage(input_datums)
  let state_after: g = storage(output_datums)
  let (assert, state_expected) =
    script(redeemer.dispatch) |> run_state(state_before)
  expect Some(_) = assert
  state_expected == state_after
}
