import {Blockfrost, fromHex, Lucid, Network, Provider, SpendingValidator, toHex} from "lucid-cardano";
import * as fs from "fs";
import cbor from "cbor";

// 24 words seed
// Yes it's a real seed, but it's on preprod net
const seed: string[] = ["wrong", "umbrella", "chunk", "engine", "run", "resist", "horn", "anger", "key", "point", "relief", "dismiss", "fossil", "obtain", "liquid", "pioneer", "save", "wing", "bright", "siege", "have", "area", "meat", "whale",]

async function setup(): Promise<Lucid> {
    const url: string = "https://cardano-preprod.blockfrost.io/api/v0";
    const projectId: string = "preprodJhvjyIJRht5PD65tHuPS6TA0Vh06GElp";
    const provider: Provider = new Blockfrost(url, projectId);
    const network: Network = "Preprod";
    const lucid: Lucid = await Lucid.new(provider, network);
    return lucid.selectWalletFromSeed(seed.join(" "), {accountIndex: 0});
}

async function load(path: string, title: string): Promise<SpendingValidator> {
    const validators: {
        title: string,
        datum: [Object],
        redeemer: [Object],
        compiledCode: string,
        hash: string
    }[] = JSON.parse(fs.readFileSync(path, "utf8")).validators;
    const validator = validators.filter((validator) => validator.title === title)[0];
    return {
        type: "PlutusV2", script: validator.compiledCode,
    };
}

export {setup, load};
