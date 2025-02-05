import { setupLucid } from "./src/setup.ts";
import {
  ByteArray,
  Int,
  MonolithicBenchmark_0Spend,
  MonolithicStorage, MonolithicStupidSpend,
} from "./benchmark/plutus.ts";
import { Data } from "https://deno.land/x/lucid@0.20.4/mod.ts";

if (import.meta.main) {
  const lucid = setupLucid();
  console.log(lucid);

  const script = new MonolithicStupidSpend();
  console.log(script);

  const pairs = new Map<ByteArray, Int>();
  //pairs.set("1", 10n);
  //pairs.set("2", 14n);
  //pairs.set("b", 20n);
  console.log(pairs);

  const storage: MonolithicStorage = { pairs: pairs };
  console.log(storage);

  console.log("Hey1");

  const datum = Data.to(storage, MonolithicBenchmark_0Spend.datum);
  //const datum = new Constr(0, [Array.from(pairs.entries())]);
  // Serialize the datum to a string

  console.log(datum);

  console.log("Hey2");

  const address = lucid.utils.scriptToAddress(script);
  console.log(address);

  let tx = lucid.newTx()
      .attachScript(script)
      .payToContract(address, Data.to(""), { lovelace: 100n })

  console.log(tx);

  const txComplete = await tx.commit();

  const txSigned = await txComplete.sign().commit();

  const txHash = await txSigned.submit();

  console.log(txHash);
}
