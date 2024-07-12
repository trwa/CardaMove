use aiken/transaction.{Input, Output, ScriptContext, Spend, find_input}
use aiken/transaction/credential.{Address}

// Find the input of the current script
pub fn script_input(ctx: ScriptContext) -> Input {
  expect Spend(output_reference) = ctx.purpose
  expect Some(input) =
    ctx.transaction.inputs
      |> find_input(output_reference)
  input
}

// Find the address of the current script
pub fn script_address(ctx: ScriptContext) -> Address {
  script_input(ctx).output.address
}

// Check if the current script is spending
pub fn script_is_spending(ctx: ScriptContext) -> Bool {
  when ctx.purpose is {
    Spend(_) -> True
    _ -> False
  }
}
