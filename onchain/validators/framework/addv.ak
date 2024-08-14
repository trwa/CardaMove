use aiken/dict
use aiken/list
use aiken/transaction.{InlineDatum, Mint, Output, ScriptContext, Spend}
use aiken/transaction/value.{AssetName, PolicyId, quantity_of}

validator(name: AssetName, start: PolicyId, addm: PolicyId) {
  fn run(
    old_addresses: List<AssetName>,
    new_address: AssetName,
    context: ScriptContext,
  ) -> Bool {
    // 0. The transaction is a spending transaction
    expect Spend(_outref) = context.purpose
    // 2. There is a single minting redeemer with the given policy ID
    // TODO: here I check that also the minting redeemer is the one I expect (with the new_address)
    expect True == ( context.transaction.redeemers |> dict.has_key(Mint(addm)) )
    // 3. There is a single output with the start NFT
    expect [output] =
      context.transaction.outputs
        |> list.filter(
            fn(output: Output) { 1 == quantity_of(output.value, start, name) },
          )
    // 4. The datum of the NFT output is a InlineDatum
    expect InlineDatum(datum) = output.datum
    // 5. The datum is a list of AssetName(s)
    expect new_addresses: List<AssetName> = datum
    // 6. The new_addresses are the old_addresses plus the new_address
    let unique_old_addresses: List<AssetName> = list.unique(old_addresses)
    let unique_new_addresses: List<AssetName> = list.unique(new_addresses)
    expect
      1 == list.length(unique_new_addresses) - list.length(unique_old_addresses)
    expect
      [new_address] == list.difference(
        unique_new_addresses,
        unique_old_addresses,
      )
    True
  }
}
