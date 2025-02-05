import { Lucid } from "jsr:@spacebudz/lucid";
import { Blockfrost, Network, Provider } from "jsr:@spacebudz/lucid";

export function setupLucid(): Lucid {
    function fromBlockfrost(
        url: string,
        projectId: string,
        network: Network,
        seed: string[],
        accountIndex: number = 0,
    ): Lucid {
        const provider: Provider = new Blockfrost(url, projectId);
        const lucid: Lucid = new Lucid({
            provider: provider,
            network: network,
        });
        return lucid.selectWalletFromSeed(seed.join(" "), {
            index: accountIndex,
        });
    }

    return fromBlockfrost(
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
