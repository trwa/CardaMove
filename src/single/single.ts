import {
  getLucidInstance,
  queryTx,
  serializeDatum,
  stringToHex,
} from "../common.ts";
import {
  ByteArray,
  Int,
  SingleBaselineSpend,
  SingleReadSpend,
  SingleStorage,
} from "../../benchmark/plutus.ts";
import { Data, Lucid, Script } from "https://deno.land/x/lucid@0.20.4/mod.ts";
import exit = Deno.exit;

async function waitSeconds(seconds: number) {
  console.log(`delay ${seconds}s...`);
  await new Promise((resolve) => setTimeout(resolve, seconds * 1000));
}

function makeRandomId(length: number): string {
  let result = "";
  const characters =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  const charactersLength = characters.length;
  let counter = 0;
  while (counter < length) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
    counter += 1;
  }
  return result;
}

function makeStorage(size: number): SingleStorage {
  const pairs = new Map<ByteArray, Int>();
  for (let i = 0; i < size; i++) {
    pairs.set(stringToHex(i.toString()), BigInt(i));
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

async function fundBaseline(lucid: Lucid, id: string, storageSize: number) {
  const script = new SingleBaselineSpend(stringToHex(id), BigInt(storageSize));
  const storage = makeStorage(storageSize);
  const datum = serializeDatum(storage, SingleBaselineSpend.datum);
  return await txFund(lucid, script, datum);
}

async function runBaseline(lucid: Lucid, id: string, storageSize: number) {
  const script = new SingleBaselineSpend(stringToHex(id), BigInt(storageSize));
  const storage = makeStorage(storageSize);
  const datum = serializeDatum(storage, SingleBaselineSpend.datum);
  return await txRun(lucid, script, datum);
}
async function fundRead(
  lucid: Lucid,
  id: string,
  storageSize: number,
  nReads: number,
) {
  const script = new SingleReadSpend(
    stringToHex(id),
    BigInt(storageSize),
    BigInt(nReads),
  );
  //console.log("script ", script);
  const storage = makeStorage(storageSize);
  const datum = serializeDatum(storage, SingleReadSpend.datum);
  return await txFund(lucid, script, datum);
}

async function runRead(
  lucid: Lucid,
  id: string,
  storageSize: number,
  nReads: number,
) {
  const script = new SingleReadSpend(
    stringToHex(id),
    BigInt(storageSize),
    BigInt(nReads),
  );
  //console.log("script ", script);
  const storage = makeStorage(storageSize);
  const datum = serializeDatum(storage, SingleReadSpend.datum);
  return await txRun(lucid, script, datum);
}

const runBaselineHash = new Map<number, string>();
const runReadHash = new Map<[number, number], string>();

if (import.meta.main) {
  const lucid = getLucidInstance();
  let id = makeRandomId(32);
  const delay = 5*60;

  let storageSize = 90;
  while (storageSize <= 100) {
    // -----------------------------------------------------------------------------------------------------------------
    // BASELINE
    // ----------------------------------------------------------------------------------------------------------------
    /*
    try {
      await waitSeconds(delay);
      console.log(`[size ${storageSize}] [baseline] fund`);
      await fundBaseline(lucid, id, storageSize);

      await waitSeconds(delay);
      console.log(`[size ${storageSize}] [baseline] run`);
      const hash = await runBaseline(lucid, id, storageSize);
      runBaselineHash.set(storageSize, hash);

      console.log(runBaselineHash);
    } catch (_) {
      console.error(`[size ${storageSize}] [baseline] error`);
      id = makeRandomId(32);
      continue;
    }
    */
    // -----------------------------------------------------------------------------------------------------------------
    // READs
    // ----------------------------------------------------------------------------------------------------------------

    let nReads = 10;

    while (nReads <= 40) {
      //try {
      await waitSeconds(delay);
      console.log(`[size ${storageSize}] [reads ${nReads}] fund`);
      await fundRead(lucid, id, storageSize, nReads);

      await waitSeconds(delay);
      console.log(`[size ${storageSize}] [reads ${nReads}] run`);
      const hash = await runRead(lucid, id, storageSize, nReads);
      runReadHash.set([storageSize, nReads], hash);
      console.log(runReadHash);
      /*} catch (_) {
        console.error(`[size ${storageSize}] [reads ${nReads}] error`);
        id = makeRandomId(32);
        continue;
      }*/

      nReads += 10;
    }

    storageSize += 10;
  }

  console.log("Waiting for transactions to be registered...");
  await waitSeconds(120);

  // -------------------------------------------------------------------------------------------------------------------
  // Query
  // -------------------------------------------------------------------------------------------------------------------

  console.log("Querying...");

  for (const [storageSize, hash] of runBaselineHash) {
    const tx = await queryTx(hash);
    console.log(`[size ${storageSize}] [baseline] ${tx["fees"]}`);
  }

  for (const [[storageSize, nReads], hash] of runReadHash) {
    const tx = await queryTx(hash);
    console.log(`[size ${storageSize}] [reads ${nReads}] ${tx["fees"]}`);
  }
}
