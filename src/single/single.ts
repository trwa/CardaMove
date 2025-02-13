import { getLucidInstance, serializeDatum, stringToHex } from "../common.ts";
import {
  ByteArray,
  Int,
  SingleBaselineSpend,
  SingleReadSpend,
  SingleStorage,
} from "../../benchmark/plutus.ts";
import { Data, Lucid, Script } from "https://deno.land/x/lucid@0.20.4/mod.ts";

async function waitSeconds(seconds: number) {
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
  const _ = await txSigned.submit();
  console.log("Funded, script address:", address);
}

async function txRun(
  lucid: Lucid,
  script: Script,
  datum: string,
) {
  const redeemer = Data.void();
  const address = lucid.utils.scriptToAddress(script);
  const utxos = await lucid.utxosAt(address);
  const tx = lucid.newTx()
    .collectFrom(utxos, redeemer)
    .payToContract(
      address,
      { Inline: datum, scriptRef: script },
      { lovelace: 100_000_000n },
    );
  const txComplete = await tx.commit();
  const txSigned = await txComplete.sign().commit();
  const _ = await txSigned.submit();
  console.log("Run, script address:", address);
}

async function fundBaseline(lucid: Lucid, id: string, size: number) {
  const script = new SingleBaselineSpend(stringToHex(id));
  const storage = makeStorage(size);
  const datum = serializeDatum(storage, SingleBaselineSpend._d);
  await txFund(lucid, script, datum);
}

async function fundRead(
  lucid: Lucid,
  id: string,
  size: number,
  nReads: number,
) {
  const script = new SingleReadSpend(stringToHex(id), BigInt(nReads));
  const storage = makeStorage(size);
  const datum = serializeDatum(storage, SingleReadSpend.datum);
  await txFund(lucid, script, datum);
}

async function runBaseline(lucid: Lucid, id: string) {
  const script = new SingleBaselineSpend(stringToHex(id));
  const utxos = await lucid.utxosAt(lucid.utils.scriptToAddress(script));
  const storage = makeStorage(utxos.length);
  const datum = serializeDatum(storage, SingleBaselineSpend._d);
  await txRun(lucid, script, datum);
}

if (import.meta.main) {
  const lucid = getLucidInstance();
  const id = makeRandomId(32);
  const delay = 120;

  for (let storageSize = 0; storageSize < 150; storageSize += 10) {
    console.log(`Size ${storageSize}. Funding baseline...`);
    await fundBaseline(lucid, id, storageSize);
    await waitSeconds(delay);

    console.log(`Size ${storageSize}. Running baseline...`);
    await runBaseline(lucid, id);
    await waitSeconds(delay);

    /*
    for (let nReads = 0; nReads <= storageSize; nReads++) {
      console.log(
        `Size ${storageSize}. Funding script with ${nReads} reads...`,
      );
      await fundRead(lucid, id, storageSize, nReads);
      await waitSeconds(delay);

      console.log(
        `Size ${storageSize}. Running script with ${nReads} reads...`,
      );
      await runAccessOne(lucid, id, nReads);
      await waitSeconds(delay);
    }
    */
  }
}
