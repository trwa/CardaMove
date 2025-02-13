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
import { Lucid, Utxo } from "https://deno.land/x/lucid@0.20.4/src/mod.ts";
import { Data, Script } from "https://deno.land/x/lucid@0.20.4/mod.ts";
import { makeStorage } from "./fund.ts";



export async function runAccessOne(lucid: Lucid, id: string, accesses: number) {
  console.log("Running one step of access one...");
  const script = new SingleAccessSpend(stringToHex(id), BigInt(accesses));
  const utxos = await getUtxos(lucid, script);
  console.log("No. UTXOs: ", utxos.length);
  const storage = makeStorage(utxos.length);
  const datum = serializeDatum(storage, SingleAccessSpend.datum);
  await submitTx(lucid, script, datum, utxos);
}

export async function runAccessTen(lucid: Lucid, id: string) {
  console.log("Running one step of access ten...");
  const script = new SingleAccessTenSpend(stringToHex(id));
  const utxos = await getUtxos(lucid, script);
  console.log("No. UTXOs: ", utxos.length);
  const storage = makeStorage(0);
  const datum = serializeDatum(storage, SingleAccessTenSpend.datum);
  await submitTx(lucid, script, datum, utxos);
}

if (import.meta.main) {
  const lucid = getLucidInstance();
  const id = "10";
  await runAccessOne(lucid, id, 1);
}
