import { lucidGetWalletFromSeedWithBlockfrostProvider } from "./framework/lucid.ts";
import { Lucid } from "https://deno.land/x/lucid@0.10.7/src/lucid/lucid.ts";

export function setup(): Promise<Lucid> {
    return lucidGetWalletFromSeedWithBlockfrostProvider(
        "https://cardano-preprod.blockfrost.io/api/v0",
        "preprodJhvjyIJRht5PD65tHuPS6TA0Vh06GElp",
        "Preprod",
        [
            "wrong",
            "umbrella",
            "chunk",
            "engine",
            "run",
            "resist",
            "horn",
            "anger",
            "key",
            "point",
            "relief",
            "dismiss",
            "fossil",
            "obtain",
            "liquid",
            "pioneer",
            "save",
            "wing",
            "bright",
            "siege",
            "have",
            "area",
            "meat",
            "whale",
        ],
    );
}

export function setupBlockfrostQuery(): (txHash: string) => Promise<Response> {
    return (txHash: string) => {
        /*
                curl -L -X GET 'https://cardano-mainnet.blockfrost.io/api/v0/txs/:hash' \
                -H 'Accept: application/json' \
                -H 'project_id: <API_KEY_VALUE>'
                */

        const headers = new Headers();
        headers.set("Accept", "application/json");
        headers.set("project_id", "preprodJhvjyIJRht5PD65tHuPS6TA0Vh06GElp");
        return fetch(
            `https://cardano-preprod.blockfrost.io/api/v0/txs/${txHash}`,
            {
                method: "GET",
                headers: headers,
            },
        );
    };
}
