import {getLucidInstance, serializeDatum, stringToHex} from "./common.ts";
import {BenchmarkSingleSpend, BenchmarkStorage, ByteArray,} from "../benchmark/plutus.ts";
import {Data, Lucid, Script} from "https://deno.land/x/lucid@0.20.4/mod.ts";
import {parse, stringify} from "jsr:@std/csv";

export async function waitSeconds(seconds: number) {
  console.log(`delay ${seconds}s...`);
  await new Promise((resolve) => setTimeout(resolve, seconds * 1000));
}

export function makeRandomId(): string {
  let result = "";
  const characters =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  const charactersLength = characters.length;
  let counter = 0;
  while (counter < 32) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
    counter += 1;
  }
  return result;
}

export function makeKey32(i: number): string {
  let s = i.toString(16);
  while (s.length < 32) {
    s = "0" + s;
  }
  return s;
}

export function makeStorage(size: number): BenchmarkStorage {
  const pairs = new Map<ByteArray, ByteArray>();
  for (let i = 0; i < size; i++) {
    pairs.set(stringToHex(makeKey32(i)), stringToHex(""));
  }
  return { pairs: pairs };
}

async function txFund(lucid: Lucid, script: Script, datum: string) {
  const address = lucid.utils.scriptToAddress(script);
  const tx = lucid.newTx().payToContract(
    address,
    { Inline: datum, scriptRef: script },
    { lovelace: 100_000_000n },
  );
  const txComplete = await tx.commit();
  const txSigned = await txComplete.sign().commit();
  return await txSigned.submit();
}

async function txRun(
  lucid: Lucid,
  script: Script,
  datum: string,
) {
  const redeemer = Data.void();
  const address = lucid.utils.scriptToAddress(script);
  const utxos = await lucid.utxosAt(address);
  console.log("n.utxo ", utxos.length);
  if (utxos.length === 0) {
    throw new Error("no utxo");
  }
  const tx = lucid.newTx()
    .collectFrom(utxos, redeemer)
    .payToContract(
      address,
      { Inline: datum, scriptRef: script },
      { lovelace: 100_000_000n },
    );
  const txComplete = await tx.commit();
  const txSigned = await txComplete.sign().commit();
  return await txSigned.submit();
}

async function fundSingle(
  lucid: Lucid,
  id: string,
  size: number,
) {
  const script = new BenchmarkSingleSpend(stringToHex(id), BigInt(size));
  const storage = makeStorage(size);
  const datum = serializeDatum(storage, BenchmarkSingleSpend.datum);
  return await txFund(lucid, script, datum);
}

async function runSingle(
  lucid: Lucid,
  id: string,
  size: number,
) {
  const script = new BenchmarkSingleSpend(stringToHex(id), BigInt(size));
  const storage = makeStorage(size);
  const datum = serializeDatum(storage, BenchmarkSingleSpend.datum);
  return await txRun(lucid, script, datum);
}

const text = `
id,size,hash
`;

if (import.meta.main) {
  const lucid = getLucidInstance();
  const id = makeRandomId();
  const delay = 60;

  //const ns = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100];
  //const cs = [1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100];


  const fundings = parse(text, {
    columns: ["id", "size", "hash"],
    skipFirstRow: true,
  });
  console.log(fundings);
  const transactions: Array<{ id: string; size: number; hash: string }> = [];

  let i = 0;
  while (i < fundings.length) {
    try {
      const id = fundings[i].id;
      const size = parseInt(fundings[i].size);
      //const size = ss[i];
      const hash = await runSingle(lucid, id, size);
      console.log(`[size ${size}] run`);
      transactions.push({ id: id, size: size, hash: hash });
      i += 1;
      await waitSeconds(delay);
    } catch (_) {
      console.error("retrying...");
      await waitSeconds(delay);
    }
  }

  const csv = stringify(transactions, {
    columns: [
      "id",
      "size",
      "hash",
    ],
  });
  console.log(csv);
}
