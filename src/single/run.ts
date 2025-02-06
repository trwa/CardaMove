import { getLucidInstance, serializeDatum, stringToHex } from "../common.ts";
import {
    ByteArray,
    Int,
    MonolithicBenchmark_1Spend,
    MonolithicStorage,
} from "../../benchmark/plutus.ts";

if (import.meta.main) {
    const lucid = getLucidInstance();

    const script = new MonolithicBenchmark_1Spend(stringToHex("abaco"));
    const address = lucid.utils.scriptToAddress(script);

    // Collect all UTxOs in the script
    const utxos = await lucid.utxosAt(address);

    // Set

    console.log("UTxOs:", utxos);
}