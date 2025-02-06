import { getLucidInstance, serializeDatum, stringToHex } from "../common.ts";
import {
    ByteArray,
    Int,
    SingleAccessOneSpend,
    SingleAccessTenSpend,
    SingleBaselineSpend,
    SingleDoNothingSpend,
    SingleStorage,
} from "../../benchmark/plutus.ts";
import { Lucid, Utxo } from "https://deno.land/x/lucid@0.20.4/src/mod.ts";
import { Data, Script } from "https://deno.land/x/lucid@0.20.4/mod.ts";
import { makeStorage } from "./fund.ts";

async function getUtxos(lucid: Lucid, script: Script) {
    const address = lucid.utils.scriptToAddress(script);
    return await lucid.utxosAt(address);
}

async function submitTx(
    lucid: Lucid,
    script: Script,
    datum: string,
    utxos: Utxo[],
) {
    const redeemer = Data.void();
    const address = lucid.utils.scriptToAddress(script);
    const tx = lucid.newTx()
        .collectFrom(utxos, redeemer)
        .payToContract(
            address,
            { Inline: datum, scriptRef: script },
            { lovelace: 100_000_000n },
        );

    const txComplete = await tx.commit();
    const txSigned = await txComplete.sign().commit();

    const txHash = await txSigned.submit();

    console.log("Tx:", tx);
    console.log("Tx complete:", txComplete);
    console.log("Tx signed:", txSigned);

    console.log("Script address:", address);
    console.log("Tx hash:", txHash);
}

async function runBaseline(lucid: Lucid, id: string) {
    const script = new SingleBaselineSpend(stringToHex(id));
    const utxos = await getUtxos(lucid, script);
    const storage = makeStorage(utxos.length);
    const datum = serializeDatum(storage, SingleBaselineSpend._d);
    await submitTx(lucid, script, datum, utxos);
}

if (import.meta.main) {
    const lucid = getLucidInstance();
    const id = "baseline";
    await runBaseline(lucid, id);
}
