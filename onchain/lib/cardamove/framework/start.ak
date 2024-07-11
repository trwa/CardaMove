use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{Address}

pub type Datum {
  keys: List<Address>,
}

pub type Redeemer {
  keys: List<Address>,
  tx_body: ByteArray,
}

pub fn validate(contract: Address, datum: Datum, redeemer: Redeemer, context: ScriptContext) -> Bool {
  True
}