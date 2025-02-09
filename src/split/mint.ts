import { SpliltTokenMint } from "../../benchmark/plutus.ts";
import { getLucidInstance, stringToHex } from "../common.ts";
import {fromText, Hasher} from "https://deno.land/x/lucid@0.20.4/src/mod.ts";
import {Data} from "https://deno.land/x/lucid@0.20.4/mod.ts";

export async function mintNFT(
  name: string,
): Promise<string> {
  const lucid = getLucidInstance();
  const script = new SpliltTokenMint(fromText("aa"));
  const policyId = Hasher.hashScript(script);
  const unit = policyId + fromText(name);

  console.log("Policy ID:", policyId, "Length:", policyId.length);

  const tx = await lucid
    .newTx()
    .attachScript(script)
    .mint({ [unit]: 1n }, Data.void())
    .commit();

  const signedTx = await tx.sign().commit();
  return await signedTx.submit();
}

if (import.meta.main) {
  const txHash = await mintNFT("token");
  console.log("Tx hash:", txHash);
}
