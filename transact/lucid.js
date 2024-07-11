import { Blockfrost, Lucid } from "lucid-cardano";
import * as fs from "fs";
// 24 words seed
// Yes it's a real seed, but it's on preprod net
const seed = ["wrong", "umbrella", "chunk", "engine", "run", "resist", "horn", "anger", "key", "point", "relief", "dismiss", "fossil", "obtain", "liquid", "pioneer", "save", "wing", "bright", "siege", "have", "area", "meat", "whale",];
async function setup() {
    const url = "https://cardano-preprod.blockfrost.io/api/v0";
    const projectId = "preprodJhvjyIJRht5PD65tHuPS6TA0Vh06GElp";
    const provider = new Blockfrost(url, projectId);
    const network = "Preprod";
    const lucid = await Lucid.new(provider, network);
    return lucid.selectWalletFromSeed(seed.join(" "), { accountIndex: 0 });
}
async function load(path, title) {
    const validators = JSON.parse(fs.readFileSync(path, "utf8")).validators;
    const validator = validators.filter((validator) => validator.title === title)[0];
    return {
        type: "PlutusV2", script: validator.compiledCode,
    };
}
export { setup, load };
