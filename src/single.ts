import {getLucidInstance, serializeDatum, stringToHex,} from "./common.ts";
import {BenchmarkSingleSpend, BenchmarkStorage, ByteArray,} from "../benchmark/plutus.ts";
import {Data, Lucid, Script} from "https://deno.land/x/lucid@0.20.4/mod.ts";
import {stringify} from "jsr:@std/csv";

async function waitSeconds(seconds: number) {
  console.log(`delay ${seconds}s...`);
  await new Promise((resolve) => setTimeout(resolve, seconds * 1000));
}

function makeRandomId(): string {
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

function makeKey32(i: number): string {
  let s = i.toString(16);
  while (s.length < 32) {
    s = "0" + s;
  }
  return s;
}

function makeStorage(size: number): BenchmarkStorage {
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
  transactions: number,
) {
  const script = new BenchmarkSingleSpend(
    stringToHex(id),
    BigInt(size),
    BigInt(transactions),
  );
  const storage = makeStorage(size);
  const datum = serializeDatum(storage, BenchmarkSingleSpend.datum);
  return await txFund(lucid, script, datum);
}

async function runSingle(
  lucid: Lucid,
  id: string,
  size: number,
  transactions: number,
) {
  const script = new BenchmarkSingleSpend(
    stringToHex(id),
    BigInt(size),
    BigInt(transactions),
  );
  const storage = makeStorage(size);
  const datum = serializeDatum(storage, BenchmarkSingleSpend.datum);
  return await txRun(lucid, script, datum);
}

if (import.meta.main) {
  const lucid = getLucidInstance();
  let id = makeRandomId();
  const delay = 2 * 60;

  const ts = [1, 5, 10];
  const ss = [100, 90, 80, 70, 60, 50, 40, 30, 20, 10];

  //const ns = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100];
  //const cs = [1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100];

  const fundings: Array<{ size: number; transactions: number; hash: string }> =
    [];
  for (const t of ts) {
    for (const s of ss) {
      await waitSeconds(delay);
      console.log(`[size ${s}] [transactions ${t}] fund`);
      const hash = await fundSingle(lucid, id, s, t);
      fundings.push({ size: s, transactions: t, hash: hash });
    }
  }
  const csv = stringify(fundings, {
    columns: [
      "size",
      "transactions",
      "hash",
    ],
  });
  console.log(csv);
}
