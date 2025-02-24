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
  new (_id: ByteArray, _policyId: ByteArray, _c: Int, _n: Int): Script;
  datum: BenchmarkStorage;
  _redeemer: BenchmarkEmpty;
}

export const BenchmarkMultiSpend = Object.assign(
  function (_id: ByteArray, _policyId: ByteArray, _c: Int, _n: Int) {
    return {
      type: "PlutusV3",
      script: applyParamsToScript(
        [_id, _policyId, _c, _n],
        "589a010100222229800aba2aba1aab9faab9eaab9dab9a9bae0059bae0049bad0039bad002488888888896600264653001300b00198059806000cc02c0092225980099b8748008c02cdd500144c8c96600266e1d2000300d375400b15980098071baa0058acc004cdc3a4000601a6ea8c040c04400a29462c80622c807a2c8060c03c004c030dd500145900a0c02c004c01cdd5005c52689b2b200a1",
        {
          "shape": {
            "dataType": "list",
            "items": [
              { "$ref": "#/definitions/ByteArray" },
              { "$ref": "#/definitions/ByteArray" },
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
