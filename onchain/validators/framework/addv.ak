use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Mint, Output, Redeemer, ScriptContext, ScriptPurpose,
  Spend,
}
use aiken/transaction/credential.{Address, Script}
use aiken/transaction/value.{AssetName, PolicyId, quantity_of}

validator(newm_script: PolicyId, addm_script: PolicyId) {
  fn run(
    datum: (List<AssetName>, Hash<Blake2b_224, Script>),
    redeemer: AssetName,
    context: ScriptContext,
  ) -> Bool {
    // 0. The ScriptPurpose is to Spend an UTxO
    expect Spend(outref): ScriptPurpose = context.purpose
    // 1. The addm_script is involved in the transaction
    expect Some(mint_redeemer): Option<Redeemer> =
      context.transaction.redeemers |> dict.get(Mint(addm_script))
    // 2. The redeemer of the addm_script is the same as our redeemer
    expect address: AssetName = mint_redeemer
    expect address == redeemer
    // 3. The transaction pays a single UTxO to this script
    expect Some(input): Option<Input> =
      context.transaction.inputs |> list.at(outref.output_index)
    let self_address: Address = input.output.address
    expect [output] =
      context.transaction.outputs
        |> list.filter(fn(output: Output) { output.address == self_address })
    // 4. The output UTxO has the authorizing start NFT
    expect 1 == quantity_of(output.value, newm_script, "Auth")
    // 5. The datum of the output UTxO is the correct list of new_addm_assets
    expect InlineDatum(inline_output_datum) = output.datum
    expect output_datum: (List<AssetName>, Hash<Blake2b_224, Script>) =
      inline_output_datum
    let old_addm_assets: List<AssetName> = list.unique(datum.1st)
    let new_addm_assets: List<AssetName> = list.unique(output_datum.1st)
    expect 1 == list.length(new_addm_assets) - list.length(old_addm_assets)
    expect [redeemer] == list.difference(new_addm_assets, old_addm_assets)
    // 6. The address of the pay script is always the same
    expect datum.2nd == output_datum.2nd
    True
  }
}
