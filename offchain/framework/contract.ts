import {Lucid, SpendingValidator, UTxO,} from "https://deno.land/x/lucid@0.10.7/src/mod.ts";
import {scriptGetAllUtxos, scriptLoadFromPlutusJson,} from "./internal/script.ts";

export class Contract {
    private readonly script: SpendingValidator;
    private readonly lucid: Lucid;
    constructor(lucid: Lucid, path: string, title: string) {
        this.script = scriptLoadFromPlutusJson(path, title);
        this.lucid = lucid;
    }

    public async run(keys: string[]): Promise<UTxO[]> {
        return await scriptGetAllUtxos(this.lucid, this.script);
    }

    public getAddress(): string {
        return this.lucid.utils.validatorToAddress(this.script);
    }
}
