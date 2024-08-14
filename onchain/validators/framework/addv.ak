use aiken/dict.{Dict}
use aiken/list
use aiken/transaction.{
  InlineDatum, Mint, Redeemer, ScriptContext, ScriptPurpose, Spend,
}
use aiken/transaction/value.{AssetName, PolicyId, quantity_of}

validator(addm: PolicyId, start: PolicyId, name: AssetName) {
  fn run(
    addresses: List<AssetName>,
    address: AssetName,
    context: ScriptContext,
  ) -> Bool {
    // 0. The transaction is a spending transaction
    expect Spend(_output_reference) = context.purpose
    // 1. The address is not in the list of addresses
    expect False == list.has(addresses, address)
    let redeemers: Dict<ScriptPurpose, Redeemer> = context.transaction.redeemers
    // 2. There is a single minting redeemer with the given policy ID
    // TODO: here I check that also the minting redeemer is the one I expect (with the address)
    expect True == dict.has_key(redeemers, Mint(addm))
    let outputs = context.transaction.outputs
    // 3. There is a single output with the start NFT
    expect [output] =
      list.filter(
        outputs,
        fn(output) { 1 == quantity_of(output.value, start, name) },
      )
    // 4. The datum of the NFT output is a InlineDatum
    expect InlineDatum(datum) = output.datum
    // 5. The datum is a list of AssetName(s)
    expect new_addresses: List<AssetName> = datum
    let uaddresses = list.unique(addresses)
    let unew_addresses = list.unique(new_addresses)
    // 6. The new addresses are the old addresses plus the address
    expect list.difference(unew_addresses, uaddresses) == [address]
    expect list.length(unew_addresses) == list.length(uaddresses) + 1
    True
  }
}
