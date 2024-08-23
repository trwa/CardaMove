use aiken/dict
use aiken/list
use aiken/transaction.{Input, Mint, Output, ScriptContext}
use aiken/transaction/value.{AssetName, PolicyId, quantity_of}

validator(s: PolicyId) {
  fn run(r: AssetName, ctx: ScriptContext) -> Bool {
    // 0. The transaction's purpose is to mint a new token
    expect Mint(policy_id) = ctx.purpose
    // 1. There is a single NFT named as the Redeemer in the minted assets
    expect [(asset, amount)]: List<(AssetName, Int)> =
      ctx.transaction.mint
        |> value.from_minted_value()
        |> value.tokens(policy_id)
        |> dict.to_list()
    expect r == asset && 1 == amount
    // 2. There is a single output with the minted NFT
    expect [_] =
      ctx.transaction.outputs
        |> list.filter(
            fn(output: Output) { 1 == quantity_of(output.value, policy_id, r) },
          )
    expect [] =
      ctx.transaction.outputs
        |> list.filter(
            fn(output: Output) { 1 != quantity_of(output.value, policy_id, r) },
          )
    // 3. There is a single output with the "Auth" NFT
    expect [_] =
      ctx.transaction.outputs
        |> list.filter(
            fn(output: Output) { 1 == quantity_of(output.value, s, "Auth") },
          )
    expect [] =
      ctx.transaction.outputs
        |> list.filter(
            fn(output: Output) { 1 != quantity_of(output.value, s, "Auth") },
          )
    // 4. There is a single input with the "Auth" NFT
    expect [_] =
      ctx.transaction.inputs
        |> list.filter(
            fn(input: Input) { 1 == quantity_of(input.output.value, s, "Auth") },
          )
    expect [] =
      ctx.transaction.inputs
        |> list.filter(
            fn(input: Input) { 1 != quantity_of(input.output.value, s, "Auth") },
          )
    True
  }
}
