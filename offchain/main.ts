import {Data} from "https://deno.land/x/lucid@0.10.7/src/mod.ts";
import {setupLucid} from "./setup.ts";
import {StartDatum} from "./framework/internal/datum.ts";
import {Contract} from "./framework/contract.ts";
import {Start} from "./framework/start.ts";

if (import.meta.main) {
    const lucid = await setupLucid();

    const contract = new Contract(
        lucid,
        "/data/Workspace/cardamove/onchain/plutus.json",
        "simple.run",
    );

    const start = new Start(
        lucid,
        "/data/Workspace/cardamove/onchain/plutus.json",
        "simple.run",
        contract,
    );

    const utxos = await contract.run([]);

    console.log("Script: ", contract);
    console.log("Address: ", contract.getAddress());
    console.log("Utxos: ", utxos);
    console.log("Datum: ", Data.from<typeof StartDatum>(utxos[0].datum!));

    console.log("Script: ", start);
}
