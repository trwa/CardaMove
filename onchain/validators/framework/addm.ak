use aiken/dict
use aiken/list
use aiken/transaction.{Mint, Output, ScriptContext}
use aiken/transaction/value.{AssetName, PolicyId, quantity_of}

validator(startm_script: PolicyId, startm_asset: AssetName) {
  fn run(new_asset: AssetName, context: ScriptContext) -> Bool {
    // 0. The ScriptPurpose is to Mint a new Token
    expect Mint(policy_id) = context.purpose
    // 1. There is a single minted asset
    expect [(asset, amount)]: List<(AssetName, Int)> =
      context.transaction.mint
        |> value.from_minted_value()
        |> value.tokens(policy_id)
        |> dict.to_list()
    // 2. The amount of the minted asset is 1
    expect 1 == amount
    // 3. The name of the new asset is new_asset
    expect asset == new_asset
    // 4. There is one output UTxO with the new_asset
    // TODO: This might be an unnecessary check.
    expect [_] =
      context.transaction.outputs
        |> list.filter(
            fn(output: Output) {
              1 == quantity_of(output.value, policy_id, new_asset)
            },
          )
    // 5. There is one output UTxO with the authorizing start NFT
    expect [_] =
      context.transaction.outputs
        |> list.filter(
            fn(output: Output) {
              1 == quantity_of(output.value, startm_script, startm_asset)
            },
          )
    True
  }
}
