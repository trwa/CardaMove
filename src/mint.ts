import {getLucidInstance, serializeDatum} from "./common.ts";
import {fromText, Hasher} from "https://deno.land/x/lucid@0.20.4/src/mod.ts";
import {Data, Lucid, Script} from "https://deno.land/x/lucid@0.20.4/mod.ts";
import {makeKey32, makeRandomId, makeStorage, waitSeconds} from "./single.ts";
import {BenchmarkMultiSpend, BenchmarkStorage, BenchmarkTokenMint,} from "../benchmark/plutus.ts";
import {parse, stringify} from "jsr:@std/csv";

function makeAsset(policy: Script, name: string, amount: bigint) {
  const policyId = Hasher.hashScript(policy);
  const unit = policyId + fromText(name);
  return { [unit]: amount };
}

async function mintKeys(lucid: Lucid, nKeys: number) {
  const policy = new BenchmarkTokenMint();
  let tx = lucid
    .newTx()
    .attachScript(policy);
  for (let i = 0; i < nKeys; i++) {
    const name = makeKey32(i);
    const asset = makeAsset(policy, name, 100n);
    tx = tx.mint(asset, Data.void());
  }
  const txComplete = await tx.commit();
  const signedTx = await txComplete.sign().commit();
  const txHash = await signedTx.submit();
  console.log("tx hash: ", txHash);
}

async function fundMulti(
  lucid: Lucid,
  id: string,
  chunkSize: number,
  nChunks: number,
) {
  const policy = new BenchmarkTokenMint();
  const policyId = Hasher.hashScript(policy);
  const scriptSpend = new BenchmarkMultiSpend(
    fromText(id),
    policyId,
    BigInt(chunkSize),
    BigInt(nChunks),
  );
  const address = lucid.utils.scriptToAddress(scriptSpend);

  let tx = lucid
    .newTx();

  for (let i = 0; i < nChunks; i++) {
    const storage: BenchmarkStorage = makeStorage(chunkSize);
    const datum: string = serializeDatum(storage, BenchmarkMultiSpend.datum);
    const key = makeAsset(policy, makeKey32(i), 1n);
    tx = tx.payToContract(
      address,
      { Inline: datum, scriptRef: scriptSpend },
      key,
    );
  }

  const txComplete = await tx.commit();
  const txSigned = await txComplete.sign().commit();

  return await txSigned.submit();
}

/*
async function spend(
  lucid: Lucid,
  scriptMint: Script,
  scriptSpend: Script,
  n: bigint,
) {
  const utxos = await getUtxos(lucid, scriptSpend);

  const redeemer = Data.void();
  const address = lucid.utils.scriptToAddress(scriptSpend);
  let tx = lucid.newTx()
    .collectFrom(utxos, redeemer);

  for (let i = 0; i < n; i++) {
    const sto: SplitStorage = { value: BigInt(i) };
    const dat: string = serializeDatum(sto, SplitBaselineSpend._d);
    tx = tx.payToContract(
      address,
      { Inline: dat, scriptRef: scriptSpend },
      makeAsset(scriptMint, i.toString(), 1n),
    );
  }

  const txComplete = await tx.commit();
  const txSigned = await txComplete.sign().commit();

  const txHash = await txSigned.submit();

  console.log("Script address:", address);
}
*/

const text = `
id,nChunks,chunkSize,hash
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,1,3da02b5c40cd0a39eedef1abd1fd9cf6379a5c86ee517a998e78ee9d8342adc3
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,2,e93194a2eac9fdfdd7ceac0c8b178579ab93e9870b3381f8aebf124e129cb226
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,3,69e93f316a40f1dae1c3840f757985968a9de9534e0e567095726d4f004d3986
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,4,569c41b7ad9299ecb440abb50655bbda3fc961fee38dfebcdce4e8affedbcc8e
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,5,8d5dad2bd51084f5ac2ab01d7a205ae96ce79441b46fb5c6a4679b93e31f59e8
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,6,00e01b66be36d99320f578b92401d8a0901749017f49636fcb8bdc60e0ce1222
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,7,dffdc1d356a9f5ef51907b557808e1c2aa00ba5887ea55a9deeef386e14d14f5
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,8,e585523fa15e3c15bb09c7acf0e80a8b199e12f976cdb9b8cbd810f89eb887ce
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,9,dede1abe682b9feb7c5f56131092594374392fe815c0774c6843c83f6197d4c2
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,10,a9cf5b4acf3aad3d4fa61f9aebe89d2d31da2e802c74b02c27a6d201e59dfb51
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,1,139af21eff85638a40ee5ec40970d2fd9b1f1a6dafe6e8be731c8a2bf72dd31a
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,2,4bdab7585d2e1d72e02605bcb2b813c0ab9ef01952468cc6f2fd240ede639a57
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,3,6deb7ecb48aba55d77b2350649d34d33e3d0344c282d925720938bde388718a9
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,4,49cd200ffd072b3d5ee396cd5187371b88762de3dae1dc6069c5482dfe13d2ae
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,5,47fa1781bc5a6f81e13b60fc4b4000649c7b4cfb0e95d66d3e770bc20fc249ac
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,6,742420c8ce44bb38530614132e7319f71f8eccca16ad24a676943aa74f166f11
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,7,186b9e9c0492b420aaf0cff5801d3e9143d6fe2f080cf0753f37684c7427179d
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,8,47e29f39371bb25a1a7bba95f29de8a63d1540d98adeeb24f6e56896822c9db7
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,9,131dd5bb8e8b67f4044ddf9bfadc4b53d763aa0128409ff6fc759e0ba31b122f
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,10,f81e85bb0d013d9254505925a3afc3f42afa020db3df56dc226d49e40e4f461a
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,1,0307ee8e2080cae04e522dcf4168b73ea2701a95b4f068f4cd83bce9b0e1b7be
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,2,f789020e235766ca954d5ddbf80e601f2f0c9b5ed4f46c2b66f635f4bdfc9b30
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,3,9dd483dee8f0ab302753dd3ea07dc83e61ce1f60819445b37449fa094eea888b
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,4,71eb8174533978e771d2cabe095fe6b30447d040006dd8d854f4e987132254f8
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,5,86e2e778b9c344dcf69d3366e2a10b9fde259dc5138dc2b686fde7c61afa828e
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,6,3e111c3c9b04b25577890de7145a5f98e51532b61ac21be45c7505aa8f63ca6e
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,7,2878e59c024ee884992cd83ac0c38ee7ab7e35f64d594ef3568ab9f5bf11c3d3
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,8,e1c6eea5f8bc4faae135750ebbce7f912845c21644de2f0d63f9952e0423987e
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,9,b7ebfafb6b75c8d76de81dce9fd969620dffc9cf3eb6dd2799aa8928cb4a65cc
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,10,bb4cd405f2e612f5bc3ab9001973bf26a949d91d492d5f6f390e2a09301a4a1f
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,1,a6f442eff3a53953127343a65e1152e673784c8a63d07022ac8c064ad0eff770
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,2,fde4fe5a0a0db88f0ac84111c9399940b0471ad1cccbdc6d9592018ada55b4b5
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,3,b5b32ea42bc86a4c958d6ff1775b54385aa5e65c77a2e7ac7e141064ba949490
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,4,d9b5efe63c4c11e94b70a8276115e717854a2847fd3293081fe5651823c25726
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,5,d43445e3e46c52bb0167ef33c9b94c4d9184f78ca4f085c694151b2d43537a83
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,6,d798f6aef22b2a2f393d67f888555f942610a9d0c89b4556f312426885347837
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,7,5e5142c77f5340289dc137ae7040cdf35693504f03947b72948be2edca537830
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,8,30f0f76ea7e89d93dfe00775f80950edb1a59cf96e2fc61525bc20a47458a0ac
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,9,a2f023b2000bee83e99a359c5ea220f98255a0ca307e407523a14a72b0154bae
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,10,ac50b39e77dee55eb07dff5fd3419e712c844309e49d38f458b5052ab04bccee
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,1,840b0b19827fb5b549a07bd63711ade0a26471d823376d50e9b6cbd5a7ad8782
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,2,7429d23bf62ba03cb0ee623804a2a4a6a0f7eda106b9e681410320439d53ffa0
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,3,b7e067946cd27a068eeed0b8bd086e907046bf5b26e00f6321048f6fad04fb3d
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,4,bfb1e5adf02b59f5be1890a8b288bdce25444053da60fb2340d8c966fa23c558
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,5,defb740faf47c48a58ef1919ba4b9fdab37588a6a566d8df10f98d639402ce18
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,6,239040ebc1a5104fa5800bf72a12edb1e311da06b12416d8300864c7694d708f
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,7,e7628f716c44b098cd5177235a37614b4eb92e47482615346ee7e297e98e3fda
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,8,f324edda943f9b9182e64593568e43dd0aabd87576d36fb1f303aadcaa7be1f3
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,9,95e903cbe905a5b2b122a50f41415363af13500b62c5c99554a01942dd1fba93
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,10,757ae8b158eff176939f5e04b6a6851fe7205394eafd2a8049cdafb76c2a57e8
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,1,aa0e3e3ab3fa6d11ff7703719350741d3c83563d831eedc9a0da0ae43c1c5a95
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,2,1d25a9dc546dad026115a4ec309f7e12748e02ebb52e28deb1293c01c3a02fd0
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,3,b94a80de6591a5e978c7c737fd0b3d8567648b73fc0774def759ad7134d0d459
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,4,a61dfcbcecbc52ef09d1b9d2afd3e1e2f5846a9985616f327c32d53c9cb0d032
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,5,876a94df9f980f12d8ab5810cffd24f9e7ad6a4ddd2e204246b430db8d31c346
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,6,cf24c5f2db09933364411dd6230ccbcf0fb919a8ad2bf66df571cf6224e43ac4
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,7,4f9e0dd93c4aea864a462cb7c4a46025092c3383d0d89be65599edcc0d682a66
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,8,ddaf65a8a8847999fadf2573f2a80360c76b2e57d11b75c6a8193642446600e0
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,9,3dec2c3b5cc25ec6e0e835226ea150a0a03526f35e6e13adc7baaf7bb9ec129d
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,10,2ad40892ee6432f3aa6e202979b7c500a137f14c2a0c842edd0d659fc3c2727e
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,1,5a27580e84df60b0e05467a41fcf1513a740008d4adb16426023bd8d4b7bae48
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,2,4958be92eedc34e75c6095ae9a43d61308fb4f8b03a368e7bc5a3e3ccbd99e10
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,3,7921b10dd2bae92ab755224ce87466feae19b1964a3525d21a05970382930d42
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,4,89c7c9564ed59c3e5172fb1200618895b7fd732b79e9e25ffd0fc3303df8b6ee
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,5,ea629220bc0d46acf1be2e297a63e6bb88f48d453a15d4b75f0eed19586670a5
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,6,f51e42c540beaec58e22bdcf3647c3f39071b831e89607a3ef26fb50c562b173
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,7,ae6ab0e3b14b6986c1e681e83b131e579f75cc144fc4738b3dbd01f6d53febec
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,8,7e1ab13cf96d16b0d62fdcaa68f5151daaf96b55a6e3c0a5acf566e7a7b01cc8
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,9,cc0a7737ae846a455c5ca7e7d8e42a450a9489132c57fbb83eec6d06a55a542c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,10,78747067d93456378cc9a08b8121683d717fe89fffde513d08853cdc6b0847a3
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,1,d3f8022ee5f20f195b142018c8be0603102a24ab490e147ad4cd4158478b7d08
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,2,8ef600662432692646f8e6d1bb597228babb5ac0873d2e76596fa80c5bb5c671
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,3,7992b6baca6521086437b3ee560f55ddc9ec6329ead94ee17d983ad209cf4499
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,4,4a6da148f4f8efcf85523c4bd4891cd8eed668340adb6ebe9cce076033b7e843
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,5,1849ab4e518e74659de01445ffb0b15fb7c56547db33f6825bb977a5a1bf71b5
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,6,3fe1ac93ac57b27b9d36e65829bef908752dc58538fd09f1f05ffb1bce59d0a1
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,7,86bf10698bc12b220f9a364addabf0729dbaba26064a06b46d80f3e53f2a8014
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,8,daf2f5cbe9a6843e208e91c9d6e97b74a64612be0beed2d61dc0225f26925906
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,9,dc58a211916aab6563dc7cda365c8038db4b968dfa4680d8fe6b35ee147a89b7
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,10,3327e9b9feda2d54e53f0169bdaa4f79f7658f710632cc141e1a783987934c51
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,1,ef0f1a319a69685bb718af2393a009e27c4a6b7ba47084baa4322fcc65f3d016
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,2,39f491e30d0a788b10f0bc75bf172f20e3dc9e922feb8addf55122b24f905caf
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,3,3ac0ed9a32ddbc5a2844c03f70ef02622d386a7d3d135576bbfc1dec137fb07a
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,4,fd705e567b08eb58985e557735b23cd38841e25a60d8faa67993cd96b1fe0a6b
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,5,02ae76e30230746ce6555fc030a5c59b04cfabf0271ceed5e0eb6824ecb476ec
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,6,27bede13efe9204a2593410406661967552945b9158879de94c289e09511616c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,7,75a0ab9a99a6a9c1d83ff7716d4451a1605d599b5cf3feba5a5c9dfe4169c59c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,8,855dbdf4be98589b11bba1840749c5023fd45c7102cf1365a5615ce253daa037
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,9,df352bf9b5a89e262428f0d4c21a0f0f1af0714012c7eb6932492b8359b75aed
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,10,48c3015dcdf04f85d48b006709f7ad20ebe1c864d4472f9973aab7455e350b9d
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,1,9365e33175efa6df889ed32ec3c409b991f98dbaeea404a1e89342a6a8eb37d7
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,2,d1d9ed0f2fad7be661a4607ea5a5ffcca7d71f058fad260d1d09ed4a50bf37d1
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,3,add670b3b7d99212943fea4a0c08798410183d7214ba223d636f14105700ba01
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,4,f37eb5043cd54c32a14c95fa37e26ecc44c73a9889938e044de7900c63311820
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,5,9e8c7fc5070f6fbad4c6ea5fb5592b4ba5000370ddff316df289da09c03fb09f
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,6,1e82dd1c804dfff000b2d941e890bed8309139799464cb55b21d3556c24e9dcc
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,7,8607e89fdd62f17f4c86127c61fcaba0ded63f72b7ad7ddb22e3ae838e8f21be
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,8,4d0192f1eaf6cc1f5aceddcdde374a9a90dc3ba80788ee8da573566c38ec39bf
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,9,f779d4a180ba587f4fb643fd2bd7a167a3cce4d54d9703a7a51ed771845e5a4a
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,10,aedd39269202be68309c85ebc81628c199494fda6fd79bcc68615a3a50401578
`;

if (import.meta.main) {
  const lucid = getLucidInstance();
  const id = makeRandomId();
  const delay = 60;

  /*
  const nKeys = 20;
  console.log("minting...");
  await mintKeys(lucid, nKeys);
  await waitSeconds(60);
  */

  const nChunks = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const chunkSize = [
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
  ];

  let nci = 0;
  const fundings = parse(text, {
    columns: ["id", "nChunks", "chunkSize", "hash"],
    skipFirstRow: true,
  });
  console.log(fundings);
  const transactions: Array<
    { id: string; nChunks: number; chunkSize: number; hash: string }
  > = [];
  while (nci < nChunks.length) {
    let csi = 0;
    while (csi < chunkSize.length) {
      try {
        const cs = chunkSize[csi];
        const nc = nChunks[nci];
        const hash = await fundMulti(lucid, id, cs, nc);
        console.log(
          `[nChunks ${nc}] [chunkSize ${cs}] fund`,
        );
        transactions.push({
          id: id,
          nChunks: nc,
          chunkSize: cs,
          hash: hash,
        });
        await waitSeconds(delay);
        csi += 1;
      } catch (_) {
        console.error("retrying...");
        await waitSeconds(delay);
      }
    }
    nci += 1;
  }

  const csv = stringify(transactions, {
    columns: [
      "id",
      "nChunks",
      "chunkSize",
      "hash",
    ],
  });
  console.log(csv);

  /*
  console.log("funding...");
  await fundMulti(lucid, scriptMint, scriptSpend, n);
  await waitSeconds(60);

  console.log("spending...");
  await spend(lucid, scriptMint, scriptSpend, n);
  await waitSeconds(60);
  */
}
