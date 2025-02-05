// deno-lint-ignore-file
import {
  applyParamsToScript,
  Script,
} from "https://deno.land/x/lucid@0.20.4/mod.ts";

export type ByteArray = string;
export type Int = bigint;
export type ListPairByteArrayInt = Map<ByteArray, Int>;
export type MonolithicRedeemer = undefined;
export type MonolithicStorage = { pairs: ListPairByteArrayInt };

const definitions = {
  "ByteArray": { "dataType": "bytes" },
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
        "5901320101003232323232323225333002323232323253330073370e900118041baa0011323322533300a3370e900018059baa0051533300d300c375400a2a66601466e1d2000300b3754002264944c8cc004004dd5980818069baa3010300d375400444a66601e002297adef6c6013233223233001001330060063016005225333014001133015337606ea4010dd4001a5eb7bdb1804c8ccc8894ccc054cdc800400189980c99bb037520106ea001c01454ccc054cdc780400189980c99bb037520106ea001c0044cc064cdd81ba900337500046600c00c0026eb8c04c004dd6980a000980c001180b0009bae300e001375a601e00260220022c2c2c601a002601a601c00260126ea800458c02cc03000cc028008c024008c024004c010dd50008a4c26cacae6955ceaab9e5573eae815d0aba201",
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
        "5903230101003232323232323225333002323232323253330073370e900118041baa001132332253323300b3001300c375400c2a66601c601a6ea801854ccc02cc004c030dd500109919191919192999808980398091baa00113253330123008301337540022646464a6660306036004264a66602c66e1d200430173754002264a66602e601a60306ea80044c8c94ccc070c07c0084c8cc00400400888c94ccc07c00854ccc070c8cdd799800807244101610033001300f37566024603e6ea801d220101610022323300100100322533302300114c0103d87a800013233322253330243372200e0062a66604866e3c01c00c4cdd2a4000660506ea00092f5c02980103d87a8000133006006001375c60440026eb4c08c004c09c008c0940045288b0991919180218128029bad3020002375c603c002604200460040042c6eacc074004c064dd50008b180d980c1baa001163003301737540022c60320026644646600200200644a666034002297ae013322533301932533301a3370e9001180d9baa00113371e00c6eb8c07cc070dd50008a50300e301b3754601c60366ea80084cc074008cc0100100044cc010010004c070004c074004dd61800980a9baa00f375c6030602a6ea80088c060c064c06400458c058c05cc05cc05cc04cdd5180b180b98099baa3016301337540022c64660020026eb0c018c04cdd500691299980a8008a6103d87a80001332253330143375e6012602c6ea80080304cdd2a40006603000497ae01330040040013017001301800130013756600860226ea8c050c044dd50031180100098008009129998088008a5eb7bdb1804c8cc88c8cc004004cc018018c060014894ccc0580044cc05ccdd81ba9004375000697adef6c6013233322253330173372001000626603666ec0dd48041ba8007005153330173371e01000626603666ec0dd48041ba800700113301b337606ea400cdd4001198030030009bae3015001375a602c002603400460300026eb8c040004dd698088009809800918088008b0b1b874800058c034004c034c038004c024dd50008b1805980600198050011804801180480098021baa00114984d9595cd2ab9d5573caae7d5d02ba15745",
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
