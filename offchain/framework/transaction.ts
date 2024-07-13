import {C, TxSigned} from "https://deno.land/x/lucid@0.10.7/src/mod.ts";
import {Buffer} from "https://deno.land/std@0.177.0/node/buffer.ts";
import {decode}  from "https://deno.land/x/cbor@v1.5.9/index.js";
export function txGet(tx: TxSigned): C.Transaction {
    return tx.txSigned;
}

export function txGetBody(tx: TxSigned): C.TransactionBody {
    return txGet(tx).body();
}

export function txBytes(tx: TxSigned): Uint8Array {
    return txGetBody(tx).to_bytes();
}

export function txHash(tx: TxSigned) {
    return C.hash_transaction(txGetBody(tx)).to_hex();
}
export function serializeTxBody(txSigned: TxSigned): string {
    const body = txGetBody(txSigned);
    const hash = txHash(txSigned);

    console.log("Tx body: ", body.to_json());
    console.log("Ts hash: ", hash);
    //console.log("TransactionBody (as Hex): ", a);

    //const base64String = Buffer.from('Hello World').toString('base64'); // Hello World
    //const utf8String = Buffer.from(base64String, 'base64').toString();

    //const b = Buffer.from(a, "hex"); //Deno.Buffer.from(a, "hex");
    //console.log("TransactionBody (as ByteArray): ", b);

    const b = txBytes(txSigned);
    const h = C.hash_blake2b256(b);
    console.log("Tb hash: ", Buffer.from(h).toString("hex"));

    // parse the bytes from cbor
    const s = Buffer.from(b);
    const j = decode(s);

    console.log("Tb bytes: ", j);

    //const k = C.TransactionBody.from_bytes(b);
    //console.log("TransactionBody: ", k.to_json());
    return "";
    //return k.to_json().toString();
}
