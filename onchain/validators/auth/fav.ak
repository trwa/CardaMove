use aiken/dict
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Mint, Output, Redeemer, ScriptContext, ScriptPurpose,
  Spend,
}
use aiken/transaction/value.{AssetName, PolicyId, quantity_of}

validator(s: PolicyId, am: PolicyId) {
  fn run(d: List<AssetName>, r: AssetName, ctx: ScriptContext) -> Bool {
    // 0. The script's purpose is to spend an UTxO
    expect Spend(outref): ScriptPurpose = ctx.purpose
    // 1. The script is spending the UTxO with the "Auth" NFT
    expect Some(input): Option<Input> =
      ctx.transaction.inputs |> list.at(outref.output_index)
    expect 1 == quantity_of(input.output.value, s, "Auth")
    // 2. The minting validator is involved in the Tx
    expect Some(redeemer): Option<Redeemer> =
      ctx.transaction.redeemers |> dict.get(Mint(am))
    // 3. Both the spend and minting scripts have the same Redeemer
    expect mr: AssetName = redeemer
    expect mr == r
    // 4. There is a single output with the "Auth" NFT
    expect [output] =
      ctx.transaction.outputs
        |> list.filter(
            fn(output: Output) { 1 == quantity_of(output.value, s, "Auth") },
          )
    expect [] =
      ctx.transaction.outputs
        |> list.filter(
            fn(output: Output) { 1 != quantity_of(output.value, s, "Auth") },
          )
    // 5. The Datum of the output UTxO is correctly formed
    expect InlineDatum(inline_datum) = output.datum
    expect datum: List<AssetName> = inline_datum
    let old_assets: List<AssetName> = list.unique(d)
    let new_assets: List<AssetName> = list.unique(datum)
    expect 1 == list.length(new_assets) - list.length(old_assets)
    expect [asset] = list.difference(new_assets, old_assets)
    expect asset == r
    True
  }
}
