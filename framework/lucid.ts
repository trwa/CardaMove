import {Blockfrost, Lucid, Network, Provider,} from "https://deno.land/x/lucid@0.10.7/src/mod.ts";

export async function lucidGetWalletFromSeedWithBlockfrostProvider(
    url: string,
    projectId: string,
    network: Network,
    seed: string[],
    accountIndex: number = 0,
): Promise<Lucid> {
    const provider: Provider = new Blockfrost(url, projectId);
    const lucid: Lucid = await Lucid.new(provider, network);
    return lucid.selectWalletFromSeed(seed.join(" "), {
        accountIndex: accountIndex,
    });
}
