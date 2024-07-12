use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{Address}
use cardamove/cardano/start.{StartDatum, StartRedeemer, start_validate}

validator(contract: Address) {
  fn run(datum: StartDatum, redeemer: StartRedeemer, context: ScriptContext) {
    start_validate(contract, datum, redeemer, context)
  }
}
