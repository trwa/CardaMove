import {setup} from "./setup.ts";
import {Contract} from "./framework/contract.ts";
import {Constr, Data, fromText, Lucid,} from "https://deno.land/x/lucid@0.10.7/src/mod.ts";

function emptyDatum(address: string): string {
    return Data.to(
        new Constr(0, [
            // address
            fromText(address),

            // data
            new Constr(0, []),
        ]),
    );
}

function emptyRedeemer(): string {
    return Data.to(
        new Constr(0, [new Constr(0, [])]),
    );
}

async function fundSimple(lucid: Lucid, contract: Contract) {
    return (
        await (
            await lucid
                .newTx()
                // 1st UTxO
                .payToContract(contract.getAddress(), {
                    inline: emptyDatum("12345678"),
                }, {
                    lovelace: 2n * 1000000n,
                })
                // 2nd UTxO
                .payToContract(contract.getAddress(), {
                    inline: emptyDatum("987654321"),
                }, {
                    lovelace: 2n * 1000000n,
                })
                .complete()
        ).sign().complete()
    ).submit();
}

async function spendCorrect2UTxO(lucid: Lucid, contract: Contract) {
    const utxos = await lucid.utxosAt(contract.getAddress());

    return (
        await (
            await lucid
                .newTx()
                .collectFrom(utxos, emptyRedeemer())
                // 1st UTxO
                .payToContract(contract.getAddress(), {
                    inline: emptyDatum("12345678"),
                }, {
                    lovelace: 4n * 1000000n,
                })
                // 2nd UTxO
                .payToContract(contract.getAddress(), {
                    inline: emptyDatum("987654321"),
                }, {
                    lovelace: 4n * 1000000n,
                })
                .attachSpendingValidator(contract.getScript())
                .complete()
        ).sign().complete()
    ).submit();
}

if (import.meta.main) {
    const lucid = await setup();

    const contract = new Contract(
        lucid,
        "/data/Workspace/cardamove/onchain/plutus.json",
        "examples/simple.run",
    );

    const utxos = await lucid.utxosAt(
        contract.getAddress(),
    );

    console.log("Contract address: ", contract.getAddress());
    console.log("Contract utxos: ", utxos);

    //await fundSimple(lucid, contract);
    console.log(await spendCorrect2UTxO(lucid, contract));

    /*
    // Build the transaction
    const tx: Tx = lucid
        .newTx()
        .collectFrom(utxos)
        .payToContract(contract.getAddress(), {
            inline: makeStartDatum([]),
            scriptRef: start.getScript(),
        }, {
            lovelace: 100n,
        });

    // Complete the transaction
    const completedTx: TxComplete = await tx
        .complete();

    console.log(
        "======================================================================================",
    );
    console.log(llTxCompleteBody(completedTx).to_json());
    console.log(
        "======================================================================================",
    );

    // Sign the transaction
    const signedTx: TxSigned = await completedTx
        .sign()
        .complete();

    console.log(
        "======================================================================================",
    );
    console.log(llTxSignedBody(signedTx).inputs().to_json());
    console.log(
        "======================================================================================",
    );

    //console.log("Tx body (json): ", await serializeTxBody(tx));
    // serializeTxBody(signedTx);

    console.log(
        "======================================================================================",
    );
    console.log(llTxCompleteHash(completedTx));
    console.log(llTxSignedHash(signedTx));
    console.log(
        "======================================================================================",
    );

    console.log(
        "======================================================================================",
    );
    console.log(llBlake2b256Hex(llTxCompleteToBytes(completedTx)));
    console.log(llBlake2b256Hex(llTxSignedToBytes(signedTx)));
    console.log(
        "======================================================================================",
    );

    // Submit the transaction
    const txHash = await signedTx
        .submit();

    console.log(
        "======================================================================================",
    );
    console.log(txHash);
    console.log(
        "======================================================================================",
    );

    //console.log("Tx hash: ", txHash);

    //console.log("Utxos: ", utxos);

     */
}
