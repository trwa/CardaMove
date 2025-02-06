import { getLucidInstance, serializeDatum, stringToHex } from "./common.ts";
import {
  ByteArray,
  Int,
  MonolithicBenchmark_1Spend,
  MonolithicStorage,
} from "../benchmark/plutus.ts";

if (import.meta.main) {
  const lucid = getLucidInstance();

  const script = new MonolithicBenchmark_1Spend(stringToHex("abaco"));

  const pairs = new Map<ByteArray, Int>();
  for (let i = 0; i < 1; i++) {
    pairs.set(i.toString(), BigInt(i));
  }
  const storage: MonolithicStorage = { pairs: pairs };

  const datum = serializeDatum(storage, MonolithicBenchmark_1Spend.datum);
  const address = lucid.utils.scriptToAddress(script);

  const tx = lucid.newTx().payToContract(
    address,
    { Inline: datum, scriptRef: script },
    { lovelace: 1000n },
  );

  const txComplete = await tx.commit();
  const txSigned = await txComplete.sign().commit();
  const txHash = await txSigned.submit();

  console.log("Tx:", tx);
  console.log("Tx complete:", txComplete);
  console.log("Tx signed:", txSigned);

  console.log("Script address:", address);
  console.log("Datum:", storage);
  console.log("Tx hash:", txHash);
}
