import { load, setup } from "./lucid.js";
import { C, Data } from "lucid-cardano";
const Datum = () => Data.void();
const Redeemer = () => Data.void();
async function payToSimpleValidator() {
    try {
        // Set up the Lucid instance
        const lucid = await setup();
        // This is my wallet address
        const address = await lucid.wallet.address();
        // Load the validator script and get the address
        const script = await load("/data/Workspace/cardamove/contract/plutus.json", "simple.run");
        const scriptAddress = lucid.utils.validatorToAddress(script);
        // Build the transaction
        const tx = lucid
            .newTx()
            .payToContract(scriptAddress, {
            inline: Datum(), scriptRef: script
        }, {
            lovelace: 1000n
        });
        const a = await tx.toString();
        console.log("TransactionBody (as Hex): ", a);
        const b = Buffer.from(a, 'hex');
        console.log("TransactionBody (as ByteArray): ", b);
        const h = C.hash_blake2b256(b);
        console.log("TransactionBody (as Hash): ", h.toLocaleString());
        const k = C.TransactionBody.from_bytes(Buffer.from(a, 'hex'));
        console.log("TransactionBody: ", k.to_json());
        // Complete the transaction
        const completedTx = await tx
            .complete();
        // Sign the transaction
        const signedTx = await completedTx
            .sign()
            .complete();
        // Submit the transaction
        const result = await signedTx
            .submit();
        // Log the result
        console.log(result);
    }
    catch (e) {
        console.error(e);
    }
}
async function redeemFromSimpleValidator() {
    try {
        // Set up the Lucid instance
        const lucid = await setup();
        // Load the validator script and get the address
        const script = await load("contracts/plutus.json", "simple.run");
        const scriptAddress = lucid.utils.validatorToAddress(script);
        console.log(scriptAddress);
        // Get the UTXOs at the script address
        const utxos = await lucid.utxosAt(scriptAddress);
        console.log(utxos);
        const tx = lucid
            .newTx()
            .collectFrom(utxos, Redeemer());
        //.payToContract(scriptAddress, {inline: Datum(), scriptRef: script}, utxos.at(1).assets);
        //const completedTx = await tx.complete();
        //const signedTx = await completedTx.sign().complete();
        //const result: string = await signedTx.submit();
        //console.log(result);
    }
    catch (e) {
        console.error(`Error: ${e}`);
    }
}
payToSimpleValidator().then(() => console.log("Done"));
