use aiken/dict
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Mint, Output, Redeemer, ScriptContext, ScriptPurpose,
  Spend,
}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{AssetName, PolicyId, quantity_of}

validator(name: AssetName, start: PolicyId, addm: PolicyId) {
  fn run(
    old_addresses: List<AssetName>,
    new_address: AssetName,
    context: ScriptContext,
  ) -> Bool {
    // 0. The transaction is a spending transaction
    expect Spend(outref): ScriptPurpose = context.purpose
    expect Some(input): Option<Input> =
      context.transaction.inputs |> list.at(outref.output_index)
    let self_address: Address = input.output.address
    // 2. There is a minting redeemer (with the given policy ID) containing the new address as well
    expect Some(redeemer): Option<Redeemer> =
      context.transaction.redeemers |> dict.get(Mint(addm))
    expect address: AssetName = redeemer
    expect address == new_address
    // 3. There is a single output spent to this validator
    expect [output] =
      context.transaction.outputs
        |> list.filter(fn(output: Output) { output.address == self_address })
    // 4. The output has the NFT of the start policy ID
    expect 1 == quantity_of(output.value, start, name)
    // 5. The datum of the NFT output is a InlineDatum
    expect InlineDatum(datum) = output.datum
    // 6. The datum is a list of AssetName(s)
    expect new_addresses: List<AssetName> = datum
    // 7. The new_addresses are the old_addresses plus the new_address
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
