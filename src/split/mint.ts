import {
  SplitAccessOneSpend,
  SplitBaselineSpend,
  SplitStorage,
  SplitTokenMint,
} from "../../benchmark/plutus.ts";
import { getLucidInstance, serializeDatum, stringToHex } from "../common.ts";
import {
  Assets,
  fromText,
  Hasher,
} from "https://deno.land/x/lucid@0.20.4/src/mod.ts";
import { Data, Lucid, Script } from "https://deno.land/x/lucid@0.20.4/mod.ts";
import { waitSeconds } from "../single/fund.ts";
import { randomId } from "../single.ts";
import { getUtxos } from "../single/run.ts";

function asset(policy: Script, name: string, amount: bigint) {
  const policyId = Hasher.hashScript(policy);
  const unit = policyId + fromText(name);
  return { [unit]: amount };
}

async function mint(lucid: Lucid, script: Script, n: bigint) {
  let tx = lucid
    .newTx()
    .attachScript(script);
  for (let i = 0; i < n; i++) {
    const ass = asset(script, i.toString(), 1n);
    tx = tx.mint(ass, Data.void());
  }
  const txComplete = await tx.commit();
  const signedTx = await txComplete.sign().commit();
  const txHash = await signedTx.submit();

  console.log("tx hash: ", txHash);
}

async function fund(lucid: Lucid, scriptMint: Script, scriptSpend: Script, n: bigint) {
  const address = lucid.utils.scriptToAddress(scriptSpend);

  let tx = lucid
    .newTx();

  for (let i = 0; i < n; i++) {
    const sto: SplitStorage = { value: BigInt(i) };
    const dat: string = serializeDatum(sto, SplitBaselineSpend._d);
    tx = tx.payToContract(
      address,
      { Inline: dat, scriptRef: scriptSpend },
      asset(scriptMint, i.toString(), 1n),
    );
  }

  const txComplete = await tx.commit();
  const txSigned = await txComplete.sign().commit();

  const txHash = await txSigned.submit();
  console.log(txHash);
}

async function spend(lucid: Lucid, scriptMint: Script, scriptSpend: Script, n: bigint) {
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
      asset(scriptMint, i.toString(), 1n),
    );
  }

  const txComplete = await tx.commit();
  const txSigned = await txComplete.sign().commit();

  const txHash = await txSigned.submit();

  console.log("Script address:", address);
}

if (import.meta.main) {
  const lucid = getLucidInstance();
  const id = randomId(32);
  const scriptMint = new SplitTokenMint(fromText(id));
  const scriptSpend = new SplitAccessOneSpend(fromText(id));
  const n = 1n;

  console.log("minting...");
  await mint(lucid, scriptMint, n);
  await waitSeconds(60);

  console.log("funding...");
  await fund(lucid, scriptMint, scriptSpend, n);
  await waitSeconds(60);

  console.log("spending...");
  await spend(lucid, scriptMint, scriptSpend, n);
  await waitSeconds(60);
}
