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
  new (): Script;
  datum: MonolithicStorage;
  _redeemer: MonolithicRedeemer;
}

export const MonolithicBenchmark_0Spend = Object.assign(
  function () {
    return {
      type: "PlutusV3",
      script:
        "59013f01010029800aba2aba1aba0aab9faab9eaab9dab9a488888896600264653001300800198041804800cc0200092225980099b8748008c01cdd500144c8cc896600266e1d2000300a375400d15980098059baa0068acc004cdc3a400060146ea8006264944c8cc004004dd5980798061baa300f300c375400444b30010018a5eb7bdb182264664464660020026600c00c602a00a44b300100189980a19bb037520086ea000d2f5bded8c113298009bae30120019bad3013001980b8012444b300133720010007133018337606ea4020dd4003802c56600266e3c02000e26603066ec0dd48041ba800700189980c19bb037520066ea0008cc0180180050142028180a800a026375c601a0026eb4c038004c04000500e45900945900c45900918060009806180680098041baa0028b200c180400098019baa0088a4d1365640041",
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
) as unknown as MonolithicBenchmark_0Spend;

export interface MonolithicBenchmark_1Spend {
  new (): Script;
  datum: MonolithicStorage;
  _redeemer: MonolithicRedeemer;
}

export const MonolithicBenchmark_1Spend = Object.assign(
  function () {
    return {
      type: "PlutusV3",
      script:
        "59032d01010029800aba2aba1aba0aab9faab9eaab9dab9a488888896600264653001300800198041804800cc0200092225980099b8748008c01cdd500144ca60026018003300c300d0019b874800122259800980098059baa0078acc004c030dd5003c566002600260166ea800a264646644b30013005300f375400313259800980318081baa001899192cc004c05c006264b30013370e900218099baa0018992cc004c028c050dd5000c4c9660026034003132330010013756603400444b30010018acc004c8cdd79980098059bab301c30193754603860326ea803d220101610033001300b3756603860326ea801522010161002232330010010032259800800c530103d87a8000899192cc004cdc8802800c56600266e3c014006266e95200033020301e0024bd7045300103d87a80004071133004004302200340706eb8c070004c07c00501d4528c5901644c8cc88cc014014c07c010dd7180c0009bad3019001301b001406516405c602a6ea80062c8098c05cc050dd5000c59012180118099baa30160018b2028323322330020020012259800800c52f5c11332259800992cc004cdc3a4004602e6ea8006266e3c018dd7180d980c1baa0018a5040586034602e6ea8c068c05cdd500144cc064008cc01001000626600800800280a8c060004c0640050161bac3002301337540186eb8c054c048dd50011180a980b180b000c5900f1809980a180a180a18081baa3013301430103754602660206ea80062c80708c008004c8cc004004dd6180918079baa0082259800800c530103d87a80008992cc004cdd7980a18089baa001008899ba548000cc04c0052f5c11330030033015002403c60260028088c004004896600200314bd6f7b63044c8cc88c8cc004004cc018018c0580148966002003133015337606ea4010dd4001a5eb7bdb1822653001375c6026003375a6028003301800248896600266e4002000e26603266ec0dd48041ba80070058acc004cdc7804001c4cc064cdd81ba9008375000e003133019337606ea400cdd400119803003000a02a4054301600140506eb8c038004dd698078009808800a01e8b20148b201a8b201418041baa0028b200c180400098019baa0088a4d13656400401",
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

export interface MonolithicStupidSpend {
  new (): Script;
  _d: Data;
  _r: Data;
}

export const MonolithicStupidSpend = Object.assign(
  function () {
    return {
      type: "PlutusV3",
      script:
        "585c01010029800aba2aba1aab9eaab9dab9a4888896600264653001300600198031803800cc0180092225980099b8748008c01cdd500144c8cc892898050009805180580098041baa0028b200c180300098019baa0068a4d13656400401",
    };
  },
  { _d: { "shape": { "$ref": "#/definitions/Data" }, definitions } },
  { _r: { "shape": { "$ref": "#/definitions/Data" }, definitions } },
) as unknown as MonolithicStupidSpend;
