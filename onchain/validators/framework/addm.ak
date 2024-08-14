use aiken/transaction.{ScriptContext}
use aiken/transaction/value.{AssetName, PolicyId}

validator(_name: AssetName, _start: PolicyId) {
  fn run(_redeemer: Data, _context: ScriptContext) {
    True
  }
}
