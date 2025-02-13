import { getLucidInstance, serializeDatum, stringToHex } from "../common.ts";
import {
    ByteArray,
    Int,
    SingleAccessSpend,
    SingleAccessTenSpend,
    SingleBaselineSpend,
    SingleDoNothingSpend,
    SingleStorage,
} from "../../benchmark/plutus.ts";
import { Lucid } from "https://deno.land/x/lucid@0.20.4/src/mod.ts";
import { Script } from "https://deno.land/x/lucid@0.20.4/mod.ts";





export async function fundDoNothing(lucid: Lucid, id: string, n: number) {
    const script = new SingleDoNothingSpend(stringToHex(id));
    const storage = makeStorage(n);
    const datum = serializeDatum(storage, SingleDoNothingSpend.datum);
    await submitTx(lucid, script, datum);
}



export async function fundAccessTen(lucid: Lucid, id: string, n: number) {
    const script = new SingleAccessTenSpend(stringToHex(id));
    const storage = makeStorage(n);
    const datum = serializeDatum(storage, SingleAccessTenSpend.datum);
    await submitTx(lucid, script, datum);
}

if (import.meta.main) {
    const lucid = getLucidInstance();
    const id = "10";
    const storageSize = 10;
    const seconds = 60;

    //await fundBaseline(lucid, id, storageSize);
    //await waitSeconds(seconds);

    //await fundDoNothing(lucid, id, storageSize);
    //await waitSeconds(seconds);

    await fundAccessOne(lucid, id, storageSize, 1);
    await waitSeconds(seconds);
}
