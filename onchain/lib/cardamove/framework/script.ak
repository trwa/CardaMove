use aiken/transaction.{Input, Output, ScriptContext, Spend, find_input}
use aiken/transaction/credential.{Address}

pub fn script_input(ctx: ScriptContext) -> Input {
  expect Spend(output_reference) = ctx.purpose
  expect Some(input) =
    ctx.transaction.inputs
      |> find_input(output_reference)
  input
}

pub fn script_address(ctx: ScriptContext) -> Address {
  script_input(ctx).output.address
}

pub fn script_is_spending(ctx: ScriptContext) -> Bool {
  when ctx.purpose is {
    Spend(_) -> True
    _ -> False
  }
}
