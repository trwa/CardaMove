use aiken/transaction/credential.{Address}
use aiken/transaction.{ScriptContext}

type ExtendScriptDatum {
    current_players: List<Address>,
}

type ExtendScriptRedeemer {
    new_player: List<Address>,
}

validator (contract: Address) {
    fn run(datum: ExtendScriptDatum, redeemer: ExtendScriptRedeemer, context: ScriptContext) {
        True
    }
}