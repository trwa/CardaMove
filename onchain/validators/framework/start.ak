use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{Address}
use cardamove/cardano/start.{Datum, Redeemer, validate}

validator(contract: Address) {
  fn run(
    datum: Datum,
    redeemer: Redeemer,
    context: ScriptContext,
  ) {
    validate(contract, datum, redeemer, context)
  }
}
