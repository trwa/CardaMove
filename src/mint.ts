import {getLucidInstance, serializeDatum} from "./common.ts";
import {fromText, Hasher} from "https://deno.land/x/lucid@0.20.4/src/mod.ts";
import {Data, Lucid, Script} from "https://deno.land/x/lucid@0.20.4/mod.ts";
import {makeKey32, makeRandomId, makeStorage, waitSeconds} from "./single.ts";
import {BenchmarkMultiSpend, BenchmarkStorage, BenchmarkTokenMint,} from "../benchmark/plutus.ts";
import {stringify} from "jsr:@std/csv";

function makeAsset(policy: Script, name: string, amount: bigint) {
  const policyId = Hasher.hashScript(policy);
  const unit = policyId + fromText(name);
  return { [unit]: amount };
}

async function mintKeys(lucid: Lucid, nKeys: number) {
  const policy = new BenchmarkTokenMint();
  let tx = lucid
    .newTx()
    .attachScript(policy);
  for (let i = 0; i < nKeys; i++) {
    const name = makeKey32(i);
    const asset = makeAsset(policy, name, 100n);
    tx = tx.mint(asset, Data.void());
  }
  const txComplete = await tx.commit();
  const signedTx = await txComplete.sign().commit();
  const txHash = await signedTx.submit();
  console.log("tx hash: ", txHash);
}

async function fundMulti(
  lucid: Lucid,
  id: string,
  chunkSize: number,
  nChunks: number,
) {
  const policy = new BenchmarkTokenMint();
  const policyId = Hasher.hashScript(policy);
  const scriptSpend = new BenchmarkMultiSpend(
    fromText(id),
    policyId,
    BigInt(chunkSize),
    BigInt(nChunks),
  );
  const address = lucid.utils.scriptToAddress(scriptSpend);

  let tx = lucid
    .newTx();

  for (let i = 0; i < nChunks; i++) {
    const storage: BenchmarkStorage = makeStorage(chunkSize);
    const datum: string = serializeDatum(storage, BenchmarkMultiSpend._datum);
    const key = makeAsset(policy, makeKey32(i), 1n);
    tx = tx.payToContract(
      address,
      { Inline: datum, scriptRef: scriptSpend },
      key,
    );
  }

  const txComplete = await tx.commit();
  const txSigned = await txComplete.sign().commit();

  return await txSigned.submit();
}

/*
async function spend(
  lucid: Lucid,
  scriptMint: Script,
  scriptSpend: Script,
  n: bigint,
) {
  const utxos = await getUtxos(lucid, scriptSpend);

  const redeemer = Data.void();
  const address = lucid.utils.scriptToAddress(scriptSpend);
  let tx = lucid.newTx()
    .collectFrom(utxos, redeemer);

  for (let i = 0; i < n; i++) {
    const sto: SplitStorage = { value: BigInt(i) };
    const dat: string = serializeDatum(sto, SplitBaselineSpend._d);
    tx = tx.payToContract(
      address,
      { Inline: dat, scriptRef: scriptSpend },
      makeAsset(scriptMint, i.toString(), 1n),
    );
  }

  const txComplete = await tx.commit();
  const txSigned = await txComplete.sign().commit();

  const txHash = await txSigned.submit();

  console.log("Script address:", address);
}
*/
if (import.meta.main) {
  const lucid = getLucidInstance();
  const id = makeRandomId();
  const delay = 60;

  /*
  const nKeys = 20;
  console.log("minting...");
  await mintKeys(lucid, nKeys);
  await waitSeconds(60);
  */

  const nChunks = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const chunkSize = [
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
  ];

  let nci = 0;
  let csi = 0;
  const transactions: Array<
    { id: string; nChunks: number; chunkSize: number; hash: string }
  > = [];
  while (nci < nChunks.length) {
    while (csi < chunkSize.length) {
      try {
        const cs = chunkSize[csi];
        const nc = nChunks[nci];
        const hash = await fundMulti(lucid, id, cs, nc);
        console.log(
          `[nChunks ${nc}] [chunkSize ${cs}] fund`,
        );
        transactions.push({
          id: id,
          nChunks: nc,
          chunkSize: cs,
          hash: hash,
        });
        await waitSeconds(delay);
        csi += 1;
      } catch (_) {
        console.error("retrying...");
        await waitSeconds(delay);
      }
    }
    nci += 1;
  }

  const csv = stringify(transactions, {
    columns: [
      "id",
      "nChunks",
      "chunkSize",
      "hash",
    ],
  });
  console.log(csv);

  /*
  console.log("funding...");
  await fundMulti(lucid, scriptMint, scriptSpend, n);
  await waitSeconds(60);

  console.log("spending...");
  await spend(lucid, scriptMint, scriptSpend, n);
  await waitSeconds(60);
  */
}
