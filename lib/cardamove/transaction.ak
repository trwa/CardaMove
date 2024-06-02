use aiken/list
use aiken/transaction.{Input, ScriptContext, Spend}
use aiken/transaction/credential.{Address}

pub fn transaction_input(ctx: ScriptContext) -> Input {
  // TODO: Does this at least fail at runtime, or is "expect" totally unsafe??
  expect Spend(output_reference) = ctx.purpose

  let inputs = ctx.transaction.inputs
  let condition =
    fn(input: Input) { input.output_reference == output_reference }

  // TODO: Same as above.
  expect Some(input) = list.find(inputs, condition)

  input
}

pub fn contract_address(ctx: ScriptContext) -> Address {
  transaction_input(ctx).output.address
}
