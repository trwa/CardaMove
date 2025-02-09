// deno-lint-ignore-file
import {
  applyParamsToScript,
  Data,
  Script,
} from "https://deno.land/x/lucid@0.20.4/mod.ts";

export type ByteArray = string;
export type Int = bigint;
export type PairsByteArrayInt = Map<ByteArray, Int>;
export type SingleEmpty = undefined;
export type SingleStorage = { pairs: PairsByteArrayInt };
export type SpliltEmpty = undefined;
export type SpliltStorage = { value: Int };

const definitions = {
  "ByteArray": { "dataType": "bytes" },
  "Int": { "dataType": "integer" },
  "Pairs$ByteArray_Int": {
    "title": "Pairs<ByteArray, Int>",
    "dataType": "map",
    "keys": { "$ref": "#/definitions/ByteArray" },
    "values": { "$ref": "#/definitions/Int" },
  },
  "single/Empty": {
    "title": "Empty",
    "anyOf": [{
      "title": "Empty",
      "dataType": "constructor",
      "index": 0,
      "fields": [],
    }],
  },
  "single/Storage": {
    "title": "Storage",
    "anyOf": [{
      "title": "Storage",
      "dataType": "constructor",
      "index": 0,
      "fields": [{
        "title": "pairs",
        "$ref": "#/definitions/Pairs$ByteArray_Int",
      }],
    }],
  },
  "splilt/Empty": {
    "title": "Empty",
    "anyOf": [{
      "title": "Empty",
      "dataType": "constructor",
      "index": 0,
      "fields": [],
    }],
  },
  "splilt/Storage": {
    "title": "Storage",
    "anyOf": [{
      "title": "Storage",
      "dataType": "constructor",
      "index": 0,
      "fields": [{ "title": "value", "$ref": "#/definitions/Int" }],
    }],
  },
};

export interface SingleAccessOneSpend {
  new (_id: ByteArray): Script;
  datum: SingleStorage;
  _redeemer: SingleEmpty;
}

export const SingleAccessOneSpend = Object.assign(
  function (_id: ByteArray) {
    return {
      type: "PlutusV3",
      script: applyParamsToScript(
        [_id],
        "590330010100229800aba2aba1aba0aab9faab9eaab9dab9a9bae0024888888896600264653001300900198049805000cc0240092225980099b8748008c020dd500144ca6002601a003300d300e0019b874800122259800980098061baa0078acc004c034dd5003c4c8c8c9660026466ebccc00566002600a60206ea801a260046eacc050c044dd5180a18089baa0068b201e48901300033001332259800980398091baa0018992cc004c020c04cdd5000c4c96600260320031325980099b8748010c054dd5000c4c9660026016602c6ea8006264b3001301c001899198008009bab301c0022259800800c4c02cdd5980e980d1baa00489919911980280298108021bae301a001375a6036002603a00280da2c80c8c05cdd5000c59015180c980b1baa0018b202830043015375460300031640586466446600400400244b30010018a5eb8226644b3001325980099b8748008c064dd5000c4cdc78031bae301d301a375400314a080c0c070c064dd5180e180c9baa00289980d80119802002000c4cc010010005017180d000980d800a03037586008602a6ea8034dd7180b980a1baa0018b2024301630173017301730133754602c602e60266ea8c058c04cdd5000c590111180a980b180b00099198008009bac30153012375401444b30010018a60103d87a80008992cc004cdd7980b980a1baa00100a899ba548000cc0580052f5c113300300330180024048602c00280a122010130002232330010010032259800800c530103d87a8000899192cc004cdc8802800c56600266e3c014006266e9520003301830160024bd7045300103d87a80004051133004004301a00340506eb8c050004c05c0050154528c5900e118010009800800912cc004006297adef6c608991991191980080099803003180b802912cc00400626602c66ec0dd48021ba80034bd6f7b63044ca60026eb8c0500066eb4c05400660320049112cc004cdc8004001c4cc068cdd81ba9008375000e00b15980099b8f00800389980d19bb037520106ea001c00626603466ec0dd48019ba800233006006001405880b0602e00280a8dd718078009bad30100013012001404116403916402c3009375400516401c300900130043754013149a26cac8011",
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
    datum: { "shape": { "$ref": "#/definitions/single/Storage" }, definitions },
  },
  {
    _redeemer: {
      "shape": { "$ref": "#/definitions/single/Empty" },
      definitions,
    },
  },
) as unknown as SingleAccessOneSpend;

export interface SingleAccessTenSpend {
  new (_id: ByteArray): Script;
  datum: SingleStorage;
  _redeemer: SingleEmpty;
}

export const SingleAccessTenSpend = Object.assign(
  function (_id: ByteArray) {
    return {
      type: "PlutusV3",
      script: applyParamsToScript(
        [_id],
        "590421010100229800aba2aba1aba0aab9faab9eaab9dab9a9bae0024888888896600264653001300900198049805000cc0240092225980099b8748008c020dd500144ca6002601a003300d300e0019b874800122259800980098061baa0078acc004c034dd5003c4c8c8ca6002b30013004300f375400b130013756602660206ea8c04cc040dd5002c5900e4cc8966002600c60226ea8006264b300130073012375400313259800980c000c4c96600266e1d200430143754003132598009805180a9baa0018992cc004c06c006264660020026eacc06c00889660020031300a3756603860326ea801226466446600a00a60400086eb8c064004dd6980d000980e000a0348b2030301637540031640506030602a6ea80062c8098c010c050dd5180b800c5901519199119801001000912cc004006297ae0899912cc004c96600266e1d20023018375400313371e00c6eb8c070c064dd5000c528202e301b30183754603660306ea800a2660340046600800800313300400400140586032002603400280b8dd61802180a1baa00c375c602c60266ea80062c8088c054c058c058c058c048dd5180a980b18091baa30153012375400316404046028602a602a00264660020026eb0c050c044dd5004912cc0040062980103d87a80008992cc004cdd7980b18099baa001009899ba548000cc0540052f5c113300300330170024044602a002809a44646600200200644b30010018a60103d87a8000899192cc004cdc8802800c56600266e3c014006266e9520003301830160024bd7045300103d87a80004051133004004301a00340506eb8c050004c05c0050152444b30013375e660020069110130003300100248810130008acc004cdd799800801a450131003300100248810131008acc004cdd799800801a450132003300100248810133008acc004cdd799800801a450133003300100248810133008acc004cdd799800801a450134003300100248810134008acc004cdd799800801a450135003300100248810135008acc004cdd799800801a450136003300100248810136008acc004cdd799800801a450137003300100248810137008acc004cdd799800801a450138003300100248810138008acc004cdd799800801a450139003300100248810139008a518b20228b20228b20228b20228b20228b20228b20228b20228b20228b2022118010009800800912cc004006297adef6c608991991191980080099803003180b802912cc00400626602c66ec0dd48021ba80034bd6f7b63044ca60026eb8c0500066eb4c05400660320049112cc004cdc8004001c4cc068cdd81ba9008375000e00b15980099b8f00800389980d19bb037520106ea001c00626603466ec0dd48019ba800233006006001405880b0602e00280a8dd718078009bad30100013012001404116403916402c3009375400516401c300900130043754013149a26cac80101",
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
    datum: { "shape": { "$ref": "#/definitions/single/Storage" }, definitions },
  },
  {
    _redeemer: {
      "shape": { "$ref": "#/definitions/single/Empty" },
      definitions,
    },
  },
) as unknown as SingleAccessTenSpend;

export interface SingleBaselineSpend {
  new (_id: ByteArray): Script;
  _d: SingleStorage;
  _r: SingleEmpty;
}

export const SingleBaselineSpend = Object.assign(
  function (_id: ByteArray) {
    return {
      type: "PlutusV3",
      script: applyParamsToScript(
        [_id],
        "587f010100229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264653001300800198041804800cc0200092225980099b8748008c020dd500144c8cc896600266e1d2000300b375400d15980098061baa0068a518b201a8b2014300c001300c300d0013009375400516401c300800130043754011149a26cac80101",
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
  { _d: { "shape": { "$ref": "#/definitions/single/Storage" }, definitions } },
  { _r: { "shape": { "$ref": "#/definitions/single/Empty" }, definitions } },
) as unknown as SingleBaselineSpend;

export interface SingleDoNothingSpend {
  new (_id: ByteArray): Script;
  datum: SingleStorage;
  _redeemer: SingleEmpty;
}

export const SingleDoNothingSpend = Object.assign(
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
    datum: { "shape": { "$ref": "#/definitions/single/Storage" }, definitions },
  },
  {
    _redeemer: {
      "shape": { "$ref": "#/definitions/single/Empty" },
      definitions,
    },
  },
) as unknown as SingleDoNothingSpend;

export interface SpliltBaselineSpend {
  new (_id: ByteArray): Script;
  _d: SpliltStorage;
  _r: SpliltEmpty;
}

export const SpliltBaselineSpend = Object.assign(
  function (_id: ByteArray) {
    return {
      type: "PlutusV3",
      script: applyParamsToScript(
        [_id],
        "587f010100229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264653001300800198041804800cc0200092225980099b8748008c020dd500144c8cc896600266e1d2000300b375400d15980098061baa0068a518b201a8b2014300c001300c300d0013009375400516401c300800130043754011149a26cac80101",
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
  { _d: { "shape": { "$ref": "#/definitions/splilt/Storage" }, definitions } },
  { _r: { "shape": { "$ref": "#/definitions/splilt/Empty" }, definitions } },
) as unknown as SpliltBaselineSpend;

export interface SpliltTokenMint {
  new (_id: ByteArray): Script;
  _r: SpliltEmpty;
}

export const SpliltTokenMint = Object.assign(
  function (_id: ByteArray) {
    return {
      type: "PlutusV3",
      script: applyParamsToScript(
        [_id],
        "5878010100229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264653001300800198041804800cc0200092225980099b8748000c020dd500144c96600266e1d20003009375400915980098051baa0048a518b20168b2010375c601660126ea800a2c8038601000260086ea802229344d95900201",
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
  { _r: { "shape": { "$ref": "#/definitions/splilt/Empty" }, definitions } },
) as unknown as SpliltTokenMint;
