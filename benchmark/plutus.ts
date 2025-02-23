// deno-lint-ignore-file
import {
  applyParamsToScript,
  Data,
  Script,
} from "https://deno.land/x/lucid@0.20.4/mod.ts";

export type ByteArray = string;
export type Int = bigint;
export type PairsByteArrayByteArray = Map<ByteArray, ByteArray>;
export type BenchmarkEmpty = undefined;
export type BenchmarkStorage = { pairs: PairsByteArrayByteArray };

const definitions = {
  "ByteArray": { "dataType": "bytes" },
  "Int": { "dataType": "integer" },
  "Pairs$ByteArray_ByteArray": {
    "title": "Pairs<ByteArray, ByteArray>",
    "dataType": "map",
    "keys": { "$ref": "#/definitions/ByteArray" },
    "values": { "$ref": "#/definitions/ByteArray" },
  },
  "benchmark/Empty": {
    "title": "Empty",
    "anyOf": [{
      "title": "Empty",
      "dataType": "constructor",
      "index": 0,
      "fields": [],
    }],
  },
  "benchmark/Storage": {
    "title": "Storage",
    "anyOf": [{
      "title": "Storage",
      "dataType": "constructor",
      "index": 0,
      "fields": [{
        "title": "pairs",
        "$ref": "#/definitions/Pairs$ByteArray_ByteArray",
      }],
    }],
  },
};

export interface BenchmarkMultiSpend {
  new (policyId: ByteArray, _c: Int, _n: Int, _t: Int): Script;
  _datum: BenchmarkStorage;
  _redeemer: BenchmarkEmpty;
}

export const BenchmarkMultiSpend = Object.assign(
  function (policyId: ByteArray, _c: Int, _n: Int, _t: Int) {
    return {
      type: "PlutusV3",
      script: applyParamsToScript(
        [policyId, _c, _n, _t],
        "590420010100222229800aba2aba1aba0aab9faab9eaab9dab9a9bae0059bad0049bad0039bad0024888888888896600264653001300c00198061806800cdc3a4005300c0024888966002600460186ea800e2653001301100198089809000cdc3a40009112cc004c004c040dd5004456600260226ea802226464b300130033012375400319800980b18099baa0019180b980c000c88c9660026016602a6ea8006297adef6c6089bab30193016375400280a0c8cc00400400c896600200314c0103d87a8000899192cc004cdc8802800c56600266e3c0140062601066036603200497ae08a60103d87a8000405d133004004301d003405c6eb8c05c004c0680050184896600200314bd7044cc05cc050c060004cc008008c06400501624444646464b3001301e0018992cc004c02cc068dd5000c4c8c966002601a60386ea80062b30013371e6eb8c080c074dd50009bae302000489925132330010010032259800800c52f5bded8c1132332233002002325980099b8748010c088dd5000c4c966002602860466ea8006264b30013029001899198008009bab30290022259800800c4dd5981518139baa00489919911980280298170021bae3027001375c6050002605400281422c8130c090dd5000c59022181318119baa0018b20423025302630263022375400244b30010018801c4cc090c094004cc008008c0980050231811801198010011811800a0408a51406d16406c64b30010018a60103d87a8000898061980f9810000a5eb8101e19198008009919800800801912cc004006297ae08998109ba932598009812000c4dd71811800c590211803998051bab300b301f375460440020386600400460460028100896600200314bd7044c8c8cc004004cc010010c09000c8966002003133023375200697ae08992cc004cdd7acc004cdc8002000c5300103d87980008acc004cdc7802000c530103d87a80008a6103d87b800040808101300103d87980008998121ba90040028998121ba900133003003302600240806eb8c0900050221bae3021001407c6466446600400400244b30010018a5eb8226644b30013259800980b18101baa001899b8f006375c604860426ea8006294101f181198101baa30233020375400513302200233004004001899802002000a03c30210013022001407c64660020026eb0c080c074dd5008912cc004006297ae08998101805180f1baa3021001330020023022001407c6eb8c078c06cdd5000c59019180e980f180f180f180d1baa3006301a375400f16406c6002660086eacc014c064dd51802980c9baa00601623002001300100145901119198008009bac30163013375400e44b30010018a60103d87a80008992cc004cdd7980c180a9baa001007898021980b800a5eb8226600600660320048098c05c0050151ba5480022c80922c8078601a6ea800e2c80586018002600e6ea803229344d95900501",
        {
          "shape": {
            "dataType": "list",
            "items": [
              { "$ref": "#/definitions/ByteArray" },
              { "$ref": "#/definitions/Int" },
              { "$ref": "#/definitions/Int" },
              { "$ref": "#/definitions/Int" },
            ],
          },
          definitions,
        } as any,
      ),
    };
  },
  {
    _datum: {
      "shape": { "$ref": "#/definitions/benchmark/Storage" },
      definitions,
    },
  },
  {
    _redeemer: {
      "shape": { "$ref": "#/definitions/benchmark/Empty" },
      definitions,
    },
  },
) as unknown as BenchmarkMultiSpend;

export interface BenchmarkSingleSpend {
  new (_id: ByteArray, _s: Int): Script;
  datum: BenchmarkStorage;
  _redeemer: BenchmarkEmpty;
}

export const BenchmarkSingleSpend = Object.assign(
  function (_id: ByteArray, _s: Int) {
    return {
      type: "PlutusV3",
      script: applyParamsToScript(
        [_id, _s],
        "58910101002229800aba2aba1aab9faab9eaab9dab9a9bae0039bad0024888888896600264653001300900198049805000cc0240092225980099b8748008c024dd500144c8c96600266e1d2000300b375400b15980098061baa0058acc004cdc3a400060166ea8c038c03c00a29462c80522c806a2c8050c034004c028dd50014590080c024004c014dd5004c52689b2b20061",
        {
          "shape": {
            "dataType": "list",
            "items": [{ "$ref": "#/definitions/ByteArray" }, {
              "$ref": "#/definitions/Int",
            }],
          },
          definitions,
        } as any,
      ),
    };
  },
  {
    datum: {
      "shape": { "$ref": "#/definitions/benchmark/Storage" },
      definitions,
    },
  },
  {
    _redeemer: {
      "shape": { "$ref": "#/definitions/benchmark/Empty" },
      definitions,
    },
  },
) as unknown as BenchmarkSingleSpend;

export interface BenchmarkTokenMint {
  new (): Script;
  _redeemer: BenchmarkEmpty;
}

export const BenchmarkTokenMint = Object.assign(
  function () {
    return {
      type: "PlutusV3",
      script:
        "587301010029800aba2aba1aab9faab9eaab9dab9a48888896600264653001300700198039804000cc01c0092225980099b8748000c01cdd500144c96600266e1d20003008375400915980098049baa0048a518b20148b200e375c601460106ea800a2c8030600e00260066ea801e29344d9590011",
    };
  },
  {
    _redeemer: {
      "shape": { "$ref": "#/definitions/benchmark/Empty" },
      definitions,
    },
  },
) as unknown as BenchmarkTokenMint;
