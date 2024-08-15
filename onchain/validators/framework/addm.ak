use aiken/dict
use aiken/list
use aiken/transaction.{Mint, Output, ScriptContext}
use aiken/transaction/value.{AssetName, PolicyId, quantity_of}

validator(name: AssetName, start: PolicyId) {
  fn run(new_address: AssetName, context: ScriptContext) -> Bool {
    // 0. The transaction is a minting transaction
    expect Mint(policy_id) = context.purpose
    // 1. There is a single NFT minted in this transaction, with unit amount
    expect [(asset, amount)]: List<(AssetName, Int)> =
      context.transaction.mint
        |> value.from_minted_value()
        |> value.tokens(policy_id)
        |> dict.to_list()
    expect 1 == amount
    // 2. The asset name of the NFT is tue new_address
    expect asset == new_address
    // 3. There is only one output with the NFT minted
    expect [_output] =
      context.transaction.outputs
        |> list.filter(
            fn(output: Output) {
              1 == quantity_of(output.value, policy_id, new_address)
            },
          )
    // 4. There is a single output with the start NFT
    // (this is sufficient since only addv can spend the NFT to itself)
    expect [_output] =
      context.transaction.outputs
        |> list.filter(
            fn(output: Output) { 1 == quantity_of(output.value, start, name) },
          )
    True
  }
}
