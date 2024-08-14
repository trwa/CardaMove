use aiken/dict
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, ScriptContext, Transaction,
}
use aiken/transaction/value.{AssetName, MintedValue, quantity_of}

validator(address: AssetName, utxo: OutputReference) {
  fn run(_redeemer: Data, context: ScriptContext) -> Bool {
    let transaction: Transaction = context.transaction
    let inputs: List<Input> = transaction.inputs
    let maybe: Option<Input> =
      list.find(inputs, fn(input) { input.output_reference == utxo })
    // 0. The UTXO is consumed in the inputs
    expect Some(_input) = maybe
    // 1. The transaction is a minting transaction
    expect Mint(policy_id) = context.purpose
    let mint: MintedValue = transaction.mint
    let tokens: List<(AssetName, Int)> =
      mint
        |> value.from_minted_value
        |> value.tokens(policy_id)
        |> dict.to_list()
    // 2. There is a single NFT minted in this transaction, with unit amount
    expect [(asset, 1)] = tokens
    // 3. The asset name of the NFT is the address
    expect asset == address
    let outputs: List<Output> = transaction.outputs
    // 4. There is only one output with the NFT minted
    expect [output] =
      list.filter(
        outputs,
        fn(output) { 1 == quantity_of(output.value, policy_id, address) },
      )
    // 5. The datum of the NFT output is a InlineDatum
    expect InlineDatum(datum) = output.datum
    // 6. The datum is an empty list of AssetName(s)
    expect addresses: List<AssetName> = datum
    // 7. The list of addresses is empty
    addresses == []
  }
}
