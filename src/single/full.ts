import { getLucidInstance } from "../common.ts";
import {
  fundAccessOne,
  fundAccessTen,
  fundBaseline,
  waitSeconds,
} from "./fund.ts";
import { runAccessOne, runAccessTen, runBaseline } from "./run.ts";

function makeid(length: number) {
  let result = "";
  const characters =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  const charactersLength = characters.length;
  let counter = 0;
  while (counter < length) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
    counter += 1;
  }
  return result;
}

async function fullBaseline(id: string, storageSize: number) {
  console.log(
    "BASELINE =============================================================================================",
  );
  const lucid = getLucidInstance();
  await fundBaseline(lucid, id, storageSize);
  await waitSeconds(60);
  await runBaseline(lucid, id);
  await waitSeconds(60);
  console.log(
    "======================================================================================================",
  );
}

async function fullAccessOne(id: string, storageSize: number) {
  console.log(
    "ACCESS ONE ===========================================================================================",
  );
  const lucid = getLucidInstance();
  await fundAccessOne(lucid, id, storageSize);
  await waitSeconds(60);
  await runAccessOne(lucid, id);
  await waitSeconds(60);
  console.log(
    "======================================================================================================",
  );
}

async function fullAccessTen(id: string, storageSize: number) {
  console.log(
    "ACCESS TEN ===========================================================================================",
  );
  const lucid = getLucidInstance();
  await fundAccessTen(lucid, id, storageSize);
  await waitSeconds(60);
  await runAccessTen(lucid, id);
  await waitSeconds(60);
}

if (import.meta.main) {
  const id = makeid(32);
  const storageSize = 150;
  await fullAccessTen(id, storageSize);
}
