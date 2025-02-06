// deno-lint-ignore-file
import {
  applyParamsToScript,
  Data,
  Script,
} from "https://deno.land/x/lucid@0.20.4/mod.ts";

export type ByteArray = string;
export type Data = Data;
export type Int = bigint;
export type ListPairByteArrayInt = Map<ByteArray, Int>;
export type MonolithicRedeemer = undefined;
export type MonolithicStorage = { pairs: ListPairByteArrayInt };

const definitions = {
  "ByteArray": { "dataType": "bytes" },
  "Data": { "title": "Data", "description": "Any Plutus data." },
  "Int": { "dataType": "integer" },
  "List$Pair$ByteArray_Int": {
    "dataType": "map",
    "keys": { "$ref": "#/definitions/ByteArray" },
    "values": { "$ref": "#/definitions/Int" },
  },
  "monolithic/Redeemer": {
    "title": "Redeemer",
    "anyOf": [{
      "title": "Redeemer",
      "dataType": "constructor",
      "index": 0,
      "fields": [],
    }],
  },
  "monolithic/Storage": {
    "title": "Storage",
    "anyOf": [{
      "title": "Storage",
      "dataType": "constructor",
      "index": 0,
      "fields": [{
        "title": "pairs",
        "$ref": "#/definitions/List$Pair$ByteArray_Int",
      }],
    }],
  },
};

export interface MonolithicBenchmark_0Spend {
  new (_id: ByteArray): Script;
  _d: Data;
  _r: Data;
}

export const MonolithicBenchmark_0Spend = Object.assign(
  function (_id: ByteArray) {
    return {
      type: "PlutusV3",
      script: applyParamsToScript(
        [_id],
        "5860010100229800aba2aba1aab9eaab9dab9a9bae00248888896600264653001300700198039804000cc01c0092225980099b8748008c020dd500144c8cc892818058009805980600098049baa0028b200e180380098021baa0078a4d1365640081",
        {
          "shape": {
            "dataType": "list",
            "items": [{ "$ref": "#/definitions/ByteArray" }],
          },
          definitions,
        } as any,
      ),
    };
  },
  { _d: { "shape": { "$ref": "#/definitions/Data" }, definitions } },
  { _r: { "shape": { "$ref": "#/definitions/Data" }, definitions } },
) as unknown as MonolithicBenchmark_0Spend;

export interface MonolithicBenchmark_1Spend {
  new (_id: ByteArray): Script;
  datum: MonolithicStorage;
  _redeemer: MonolithicRedeemer;
}

export const MonolithicBenchmark_1Spend = Object.assign(
  function (_id: ByteArray) {
    return {
      type: "PlutusV3",
      script: applyParamsToScript(
        [_id],
        "590144010100229800aba2aba1aba0aab9faab9eaab9dab9a9bae0024888888896600264653001300900198049805000cc0240092225980099b8748008c020dd500144c8cc896600266e1d2000300b375400d15980098061baa0068acc004cdc3a400060166ea8006264944c8cc004004dd5980818069baa3010300d375400444b30010018a5eb7bdb182264664464660020026600c00c602c00a44b300100189980a99bb037520086ea000d2f5bded8c113298009bae30130019bad3014001980c0012444b300133720010007133019337606ea4020dd4003802c56600266e3c02000e26603266ec0dd48041ba800700189980c99bb037520066ea0008cc018018005015202a180b000a028375c601c0026eb4c03c004c04400500f45900a45900d45900a18068009806980700098049baa0028b200e180480098021baa0098a4d13656400801",
        {
          "shape": {
            "dataType": "list",
            "items": [{ "$ref": "#/definitions/ByteArray" }],
          },
          definitions,
        } as any,
      ),
    };
  },
  {
    datum: {
      "shape": { "$ref": "#/definitions/monolithic/Storage" },
      definitions,
    },
  },
  {
    _redeemer: {
      "shape": { "$ref": "#/definitions/monolithic/Redeemer" },
      definitions,
    },
  },
) as unknown as MonolithicBenchmark_1Spend;

export interface MonolithicBenchmark_2Spend {
  new (_id: ByteArray): Script;
  datum: MonolithicStorage;
  _redeemer: MonolithicRedeemer;
}

export const MonolithicBenchmark_2Spend = Object.assign(
  function (_id: ByteArray) {
    return {
      type: "PlutusV3",
      script: applyParamsToScript(
        [_id],
        "590331010100229800aba2aba1aba0aab9faab9eaab9dab9a9bae0024888888896600264653001300900198049805000cc0240092225980099b8748008c020dd500144ca6002601a003300d300e0019b874800122259800980098061baa0078acc004c034dd5003c566002600260186ea800a264646644b300130053010375400313259800980318089baa001899192cc004c060006264b30013370e9002180a1baa0018992cc004c028c054dd5000c4c9660026036003132330010013756603600444b30010018acc004c8cdd79980098059bab301d301a3754603a60346ea803d22101610033001300b3756603a60346ea801522010161002232330010010032259800800c530103d87a8000899192cc004cdc8802800c56600266e3c014006266e95200033021301f0024bd7045300103d87a80004075133004004302300340746eb8c074004c08000501e4528c5901744c8cc88cc014014c080010dd7180c8009bad301a001301c0014069164060602c6ea80062c80a0c060c054dd5000c590131801180a1baa30170018b202a323322330020020012259800800c52f5c11332259800992cc004cdc3a400460306ea8006266e3c018dd7180e180c9baa0018a50405c603660306ea8c06cc060dd500144cc068008cc01001000626600800800280b0c064004c0680050171bac3002301437540186eb8c058c04cdd50011180b180b980b800c59010180a180a980a980a98089baa3014301530113754602860226ea80062c80788c008004c8cc004004dd6180998081baa0082259800800c530103d87a80008992cc004cdd7980a98091baa001008899ba548000cc0500052f5c11330030033016002404060280028090c004004896600200314bd6f7b63044c8cc88c8cc004004cc018018c05c0148966002003133016337606ea4010dd4001a5eb7bdb1822653001375c6028003375a602a003301900248896600266e4002000e26603466ec0dd48041ba80070058acc004cdc7804001c4cc068cdd81ba9008375000e00313301a337606ea400cdd400119803003000a02c4058301700140546eb8c03c004dd698080009809000a0208b20168b201c8b201618049baa0028b200e180480098021baa0098a4d13656400801",
        {
          "shape": {
            "dataType": "list",
            "items": [{ "$ref": "#/definitions/ByteArray" }],
          },
          definitions,
        } as any,
      ),
    };
  },
  {
    datum: {
      "shape": { "$ref": "#/definitions/monolithic/Storage" },
      definitions,
    },
  },
  {
    _redeemer: {
      "shape": { "$ref": "#/definitions/monolithic/Redeemer" },
      definitions,
    },
  },
) as unknown as MonolithicBenchmark_2Spend;
