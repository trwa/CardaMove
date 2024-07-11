import {Data} from "https://deno.land/x/lucid@0.10.7/src/mod.ts";
import {setup} from "./setup.ts";
import {ContractDatum, StartDatum} from "./framework/internal/datum.ts";
import {Contract} from "./framework/contract.ts";
import {Start} from "./framework/start.ts";

if (import.meta.main) {
    const lucid = await setup();

    const contract = new Contract(
        lucid,
        "/data/Workspace/cardamove/onchain/plutus.json",
        "simple.run",
    );

    const start = new Start(
        lucid,
        "/data/Workspace/cardamove/onchain/plutus.json",
        "framework/start.run",
        contract,
    );

    const utxos = await contract.run([]);

    console.log("Address: ", contract.getAddress());
    console.log("Utxos: ", utxos);
    console.log("Datum: ", Data.from<typeof ContractDatum>(utxos[0].datum!));

    console.log("Address: ", start.getAddress());
}
