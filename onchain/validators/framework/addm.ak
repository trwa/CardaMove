use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction.{Mint, ScriptContext, Transaction}
use aiken/transaction/credential.{Address, Script}
use aiken/transaction/value.{AssetName, PolicyId}

validator(start: PolicyId, name: AssetName) {
  fn run(_r: Data, ctx: ScriptContext) {
    True
  }
}
