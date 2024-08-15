use aiken/dict
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Mint, Output, Redeemer, ScriptContext, ScriptPurpose,
  Spend,
}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{AssetName, PolicyId, quantity_of}

validator(
  startm_script: PolicyId,
  startm_asset: AssetName,
  addm_script: PolicyId,
) {
  fn run(
    old_addm_assets: List<AssetName>,
    new_addm_asset: AssetName,
    context: ScriptContext,
  ) -> Bool {
    // 0. The ScriptPurpose is to Spend an UTxO
    expect Spend(outref): ScriptPurpose = context.purpose
    // 1. The addm_script is involved in the transaction
    expect Some(redeemer): Option<Redeemer> =
      context.transaction.redeemers |> dict.get(Mint(addm_script))
    // 2. The redeemer of the addm_script is the same as our redeemer
    expect address: AssetName = redeemer
    expect address == new_addm_asset
    // 3. The transaction pays a single UTxO to this script
    expect Some(input): Option<Input> =
      context.transaction.inputs |> list.at(outref.output_index)
    let self_address: Address = input.output.address
    expect [output] =
      context.transaction.outputs
        |> list.filter(fn(output: Output) { output.address == self_address })
    // 4. The output UTxO has the authorizing start NFT
    expect 1 == quantity_of(output.value, startm_script, startm_asset)
    // 5. The datum of the output UTxO is the correct list of new_addm_assets
    expect InlineDatum(datum) = output.datum
    expect new_addm_assets: List<AssetName> = datum
    let uold_addm_assets: List<AssetName> = list.unique(old_addm_assets)
    let unew_addm_assets: List<AssetName> = list.unique(new_addm_assets)
    expect 1 == list.length(unew_addm_assets) - list.length(uold_addm_assets)
    expect
      [new_addm_asset] == list.difference(unew_addm_assets, uold_addm_assets)
    True
  }
}
