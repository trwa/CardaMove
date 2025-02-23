import {getLucidInstance, serializeDatum, stringToHex} from "./common.ts";
import {BenchmarkSingleSpend, BenchmarkStorage, ByteArray,} from "../benchmark/plutus.ts";
import {Data, Lucid, Script} from "https://deno.land/x/lucid@0.20.4/mod.ts";
import {parse, stringify} from "jsr:@std/csv";

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
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,100,f5ffba886446b52d90f005b8d331b4c996fdce66c132c6da3b6a960e311d9661
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,90,1d9fba62400d27426abe0caa02b64a653acea484e9423474bece260359891092
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,80,e55d7c465f33cac7e9490930a380de86f2c229fb6ca42a3b09c92f483b8ce1b1
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,70,9eba3dda5d4d80b67d3ba637fe640cfc3d80918057fb806ebf0f0fa36fd2fb74
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,60,5cef47102bb9ce055f53f16d4bdc64cb787b750f651772bdd228893db6561f02
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,50,070c10adf65121386c51212ace18e99c75acd4c2b403a32ad25311f49d73a62b
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,40,980425611b807df8987d596ebdb53fb8a2f7f6b7a5b9cdda59f620c82270f1e7
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,30,f91dd62dc30ea8cda935ad8736f03eb55622ca8b2b5b4e9ac270e218c64ed5cb
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,20,afa89b08ddd509f36e15bad49be2609ca2e6c09b17cbfc706540f270d379bb70
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,10,3b58ee3e30a99291a1c6accc63f4f5603e09eb2d1af2f593b7f4b83719044b4c
`;

if (import.meta.main) {
  const lucid = getLucidInstance();
  const id = makeRandomId();
  const delay = 60;

  const ss = [
    1000,
    900,
    800,
    700,
    600,
    500,
    400,
    300,
    200,
    9,
    8,
    7,
    6,
    5,
    4,
    3,
    2,
    1,
  ];

  //const ns = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100];
  //const cs = [1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100];

  const fundings = parse(text, {
    columns: ["id", "size", "hash"],
    skipFirstRow: true,
  });
  console.log(fundings);
  const transactions: Array<{ id: string; size: number; hash: string }> = [];
  let i = 0;
  while (i < ss.length) {
    try {
      await waitSeconds(delay);
      //const id = fundings[i].id;
      //const size = parseInt(fundings[i].size);
      const size = ss[i];
      const hash = await fundSingle(lucid, id, size);
      console.log(`[size ${size}] run`);
      transactions.push({ id: id, size: size, hash: hash });
      i += 1;
    } catch (_) {
      console.error("retrying...");
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
