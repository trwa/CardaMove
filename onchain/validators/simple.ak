use aiken/transaction.{ScriptContext, Transaction}

type Datum {
  txHex: ByteArray,
}

type Redeemer {
  Redeemer
}

validator {
  fn run(_datum: Datum, _redeemer: Redeemer, _ctx: ScriptContext) -> Bool {
    True
  }
}
