import {
    applyParamsToScript,
    fromText,
    Lucid,
} from "jsr:@spacebudz/lucid";

/*
export function scriptFromJson(
    path: string,
    title: string,
): Script {
    const validators: {
        title: string;
        datum: [object];
        redeemer: [object];
        compiledCode: string;
        hash: string;
    }[] = JSON.parse(Deno.readTextFileSync(path)).validators;
    const validator =
        validators.filter((validator) => validator.title === title)[0];
    return {
        type: "PlutusV2",
        script: validator.compiledCode,
    };
}



export function scriptApplyStringParam(
    script: Script
    s: string,
): Script {
    return {
        type: script.type,
        script: applyDoubleCborEncoding(
            applyParamsToScript(
                script.script,
                [fromText(s)],
            ),
        ),
    };
}

export function scriptGetAddress(
    lucid: Lucid,
    script: SpendingValidator,
): string {
    return lucid.utils.validatorToAddress(script);
}

export function scriptGetAllUtxos(
    lucid: Lucid,
    script: SpendingValidator,
): Promise<UTxO[]> {
    return lucid.utxosAt(scriptGetAddress(lucid, script));
}
*/