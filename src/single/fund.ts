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
import { Lucid } from "https://deno.land/x/lucid@0.20.4/src/mod.ts";
import { Script } from "https://deno.land/x/lucid@0.20.4/mod.ts";

function makeStorage(n: number): SingleStorage {
    const pairs = new Map<ByteArray, Int>();
    for (let i = 0; i < n; i++) {
        pairs.set(stringToHex(i.toString()), BigInt(i));
    }
    return { pairs: pairs };
}

async function submitTx(lucid: Lucid, script: Script, datum: string) {
    const address = lucid.utils.scriptToAddress(script);
    const tx = lucid.newTx().payToContract(
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

async function waitSeconds(seconds: number) {
    await new Promise((resolve) => setTimeout(resolve, seconds * 1000));
}

async function fundBaseline(lucid: Lucid, id: string, n: number) {
    const script = new SingleBaselineSpend(stringToHex(id));
    const storage = makeStorage(n);
    const datum = serializeDatum(storage, SingleBaselineSpend._d);
    await submitTx(lucid, script, datum);
}

async function fundDoNothing(lucid: Lucid, id: string, n: number) {
    const script = new SingleDoNothingSpend(stringToHex(id));
    const storage = makeStorage(n);
    const datum = serializeDatum(storage, SingleDoNothingSpend.datum);
    await submitTx(lucid, script, datum);
}

async function fundAccessOne(lucid: Lucid, id: string, n: number) {
    const script = new SingleAccessOneSpend(stringToHex(id));
    const storage = makeStorage(n);
    const datum = serializeDatum(storage, SingleAccessOneSpend.datum);
    await submitTx(lucid, script, datum);
}

async function fundAccessTen(lucid: Lucid, id: string) {
    const script = new SingleAccessTenSpend(stringToHex(id));
    const storage = makeStorage(0);
    const datum = serializeDatum(storage, SingleAccessTenSpend.datum);
    await submitTx(lucid, script, datum);
}

if (import.meta.main) {
    const lucid = getLucidInstance();
    const id = "0";
    const storageSize = 100;
    const seconds = 60;

    await fundBaseline(lucid, id, storageSize);
    await waitSeconds(seconds);

    await fundDoNothing(lucid, id, storageSize);
    await waitSeconds(seconds);

    await fundAccessOne(lucid, id, storageSize);
    await waitSeconds(seconds);
}
