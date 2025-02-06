import { Lucid } from "jsr:@spacebudz/lucid";
import { Blockfrost, Network, Provider } from "jsr:@spacebudz/lucid";
import {
  Codec,
  Data,
  DataJson,
  Exact,
} from "https://deno.land/x/lucid@0.20.4/mod.ts";

export function getLucidInstance(): Lucid {
  function fromBlockfrost(
    url: string,
    projectId: string,
    network: Network,
    seed: string[],
    accountIndex: number = 0,
  ): Lucid {
    const provider: Provider = new Blockfrost(url, projectId);
    const lucid: Lucid = new Lucid({
      provider: provider,
      network: network,
    });
    return lucid.selectWalletFromSeed(seed.join(" "), {
      index: accountIndex,
    });
  }

  return fromBlockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    "previewEtmm3aaiSJlfKpc2YTNfibpVAPHI2WJ2",
    "Preview",
    [
      "timber",
      "gloom",
      "wasp",
      "embody",
      "gravity",
      "deal",
      "reason",
      "load",
      "shoulder",
      "area",
      "soap",
      "domain",
      "involve",
      "chronic",
      "bracket",
      "leaf",
      "blouse",
      "punch",
      "almost",
      "enact",
      "seven",
      "member",
      "silk",
      "middle",
    ],
  );
}

export function stringToHex(str: string) {
  return Array.from(str)
    .map((char) => char.charCodeAt(0).toString(16).padStart(2, "0"))
    .join("");
}

export function serializeDatum<T = Data>(data: Exact<T>, type?: T): string {
  function dataToJson(data: Data): DataJson {
    if (typeof data === "bigint") return { int: data };
    if (typeof data === "string") return { bytes: stringToHex(data) };
    if (data instanceof Array) return { list: data.map(dataToJson) };
    if (data instanceof Map) {
      return {
        map: (() => {
          const map = [];
          for (const [key, value] of data.entries()) {
            map.push({ k: dataToJson(key), v: dataToJson(value) });
          }
          return map;
        })(),
      };
    }
    return { constructor: data.index, fields: data.fields.map(dataToJson) };
  }
  const d = type ? Data.castTo<T>(data, type) : data as Data;
  return Codec.encodeData(dataToJson(d));
}

export function setupBlockfrostQuery(): (txHash: string) => Promise<Response> {
  return (txHash: string) => {
    /*
        curl -L -X GET 'https://cardano-mainnet.blockfrost.io/api/v0/txs/:hash' \
        -H 'Accept: application/json' \
        -H 'project_id: <API_KEY_VALUE>'
        */

    const headers = new Headers();

    headers.set("Accept", "application/json");
    headers.set("project_id", "preprodJhvjyIJRht5PD65tHuPS6TA0Vh06GElp");

    return fetch(
      `https://cardano-preprod.blockfrost.io/api/v0/txs/${txHash}`,
      {
        method: "GET",
        headers: headers,
      },
    );
  };
}
