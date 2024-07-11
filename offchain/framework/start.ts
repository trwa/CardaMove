import {SpendingValidator} from "https://deno.land/x/lucid@0.10.7/src/types/types.ts";
import {scriptApplyToString, scriptGetAddress, scriptLoadFromPlutusJson,} from "./internal/script.ts";
import {Lucid} from "https://deno.land/x/lucid@0.10.7/src/lucid/lucid.ts";

class StartScript {
    constructor(
        public lucid: Lucid,
        public path: string,
        public title: string,
        contract: SpendingValidator,
    ) {

    }

    public load(): SpendingValidator {
        return scriptLoadFromPlutusJson(this.path, this.title);
    }

    public apply(contract: SpendingValidator): SpendingValidator {
        return scriptApplyToString(this.load(), scriptGetAddress(this.lucid, contract));
    }

}

export function startLoadScript(
    lucid: Lucid,
    path: string,
    title: string,
): (contract: SpendingValidator) => SpendingValidator {
    const script = scriptLoadFromPlutusJson(path, title);
    return (contract) => {
        return scriptApplyToString(script, scriptGetAddress(lucid, contract));
    };
}

export function startExtendStorage(
    lucid: Lucid,
    startScript: SpendingValidator,
    keys: string[],
) {

}
