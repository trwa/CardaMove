import {C, Data, Tx, TxComplete, TxSigned} from "https://deno.land/x/lucid@0.10.7/src/mod.ts";
import {setupLucid} from "./setupLucid.ts";
import {ContractDatum} from "./framework/internal/datum.ts";
import {Contract} from "./framework/contract.ts";
import {makeStartDatum, Start} from "./framework/start.ts";
import {Buffer} from "jsr:@std/io/buffer";
import {serializeTxBody} from "./framework/transaction.ts";

if (import.meta.main) {
    const lucid = await setupLucid();

    const contract = new Contract(
        lucid,
        "/data/Workspace/cardamove/onchain/plutus.json",
        "examples/simple.run",
    );

    const start = new Start(
        lucid,
        "/data/Workspace/cardamove/onchain/plutus.json",
        "framework/start.run",
        contract,
    );

    const utxos = await contract.run([]);

    //utxos[0].txHash;

    console.log("Address: ", contract.getAddress());
    console.log("Utxos: ", utxos);
    //console.log("Datum: ", Data.from<typeof ContractDatum>(utxos[0].datum!));

    console.log("Address: ", start.getAddress());

    // Build the transaction
    const tx: Tx = lucid
        .newTx()
        .payToContract(start.getAddress(), {
            inline: makeStartDatum([]),
            scriptRef: start.getScript(),
        }, {
            lovelace: 100n,
        });



    // Complete the transaction
    const completedTx: TxComplete = await tx
        .complete();

    // Sign the transaction
    const signedTx: TxSigned = await completedTx
        .sign()
        .complete();

    //console.log("Tx body (json): ", await serializeTxBody(tx));
    serializeTxBody(signedTx);

    // Submit the transaction
    const txHash = await signedTx
        .submit();

    console.log("Tx hash: ", txHash);
}
