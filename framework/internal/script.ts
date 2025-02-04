import {
    applyDoubleCborEncoding,
    applyParamsToScript,
    fromText,
    Lucid,
    SpendingValidator,
    UTxO,
} from "https://deno.land/x/lucid@0.10.7/src/mod.ts";

export function scriptLoadFromPlutusJson(
    path: string,
    title: string,
): SpendingValidator {
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

export function scriptApplyToString(
    script: SpendingValidator,
    s: string,
): SpendingValidator {
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
