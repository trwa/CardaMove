use aiken/dict
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Redeemer, ScriptContext,
}
use aiken/transaction/value.{AssetName, quantity_of}

validator(utxo: OutputReference) {
  fn run(_redeemer: Redeemer, context: ScriptContext) -> Bool {
    // 0. The UTXO is consumed in the inputs
    expect Some(_input) =
      context.transaction.inputs
        |> list.find(fn(input) { input.output_reference == utxo })
    // 1. The transaction is a minting transaction
    expect Mint(policy_id) = context.purpose
    // 2. There is a single NFT minted in this transaction, with unit amount
    expect [(asset, 1)]: List<(AssetName, Int)> =
      context.transaction.mint
        |> value.from_minted_value()
        |> value.tokens(policy_id)
        |> dict.to_list()
    // 3. The asset name of the NFT is "Auth"
    expect asset == "Auth"
    // 4. There is only one output with the NFT minted
    expect [output] =
      context.transaction.outputs
        |> list.filter(
            fn(output: Output) {
              1 == quantity_of(output.value, policy_id, "Auth")
            },
          )
    // 5. The datum of the NFT output is a InlineDatum
    expect InlineDatum(datum) = output.datum
    // 6. The datum is an empty list of AssetName(s)
    expect []: List<AssetName> = datum
    True
  }
}
