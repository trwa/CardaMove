import {queryTx} from "./common.ts";
import {parse, stringify} from "jsr:@std/csv";
import {waitSeconds} from "./single.ts";

const single_fund = `
id,size,hash
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,200,9d18e7e1d5a3d38f53ecaba9982172d611fc95a4dab68683f65af027b64ba30b
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,190,a2906ae7d69044ea915e3481c62a55f1f08e5677347f8e794ea6f69b2decc09c
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,180,61c33ee6d8c33630846a70de2078be57a997711f53a12630b49ffe3053425c52
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,170,37c73a092dc190d855690f06c445c0f0a323b85ffa5db41e586c314de0bc5f95
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,160,4d76cf75e92b4cffc194d0b28500d0968c04db4b0617f4210ad079cabd248553
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,150,8892589d0378e7595af8cc173911b15b762ba038c7d099b8c742b2fd34e21eb1
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,140,ec26311b06dee5d9288e1aab98a8af1c76d0e4977255b9f2a5c1672f55419daf
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,130,eab564435070bdb2a3ccc1d6951eac83d2d5ad3e8e0095f011e7ffc5bda97660
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,120,b87ec7e39d25980091db74ed913ccc021408f394c6486e2574f3c2d8f8f01e7c
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,110,c53e1b8d962f680f28e6f9512aa94aa7bef07bb94b3e8d181a02bf4da5c5f4ad
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,100,f5ffba886446b52d90f005b8d331b4c996fdce66c132c6da3b6a960e311d9661
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,90,1d9fba62400d27426abe0caa02b64a653acea484e9423474bece260359891092
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,80,e55d7c465f33cac7e9490930a380de86f2c229fb6ca42a3b09c92f483b8ce1b1
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,70,9eba3dda5d4d80b67d3ba637fe640cfc3d80918057fb806ebf0f0fa36fd2fb74
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,60,5cef47102bb9ce055f53f16d4bdc64cb787b750f651772bdd228893db6561f02
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,50,070c10adf65121386c51212ace18e99c75acd4c2b403a32ad25311f49d73a62b
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,40,980425611b807df8987d596ebdb53fb8a2f7f6b7a5b9cdda59f620c82270f1e7
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,30,f91dd62dc30ea8cda935ad8736f03eb55622ca8b2b5b4e9ac270e218c64ed5cb
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,20,afa89b08ddd509f36e15bad49be2609ca2e6c09b17cbfc706540f270d379bb70
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,10,3b58ee3e30a99291a1c6accc63f4f5603e09eb2d1af2f593b7f4b83719044b4c
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,9,37f9375a080a63e2ac9b1777e4b098418588bec41125b1a41df8dc78d19fe1ad
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,8,7eaa6e7f95fd3a00d3455df2754da92afaa1acba6e405734ba2ce063bc46a8f3
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,7,4e30d372d5bb07340e66c50ba136ae4f27539766f2ec7c59f8eb2fc17398b30f
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,6,458de36f86d20b8cc7b0733cb58bf1d8cb4154da0cd5849c1cd822a71de1eefa
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,5,f415387d7c35362b1b1ff2aa5440681210622cb60061e3a7ec2327d2af2fadfa
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,4,e16365e110cb0910a799a8e5b9a29fa7b45bc753ff4c101927ab0033eaccee90
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,3,ca09122034b2ee423efdbc927723350fd432ffbfa976fc6c05fe93bc1f361117
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,2,8e37a3664fa6a8f8ff7993b8789ef24a265ccf1c06aa09d0fafb6e8363ac0fd6
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,1,6a6170e4abb5f4a4c5f8472d79640b6d22ba32435d57486d6ecd71ce2d26ee0e
`;

const single_run = `
id,size,hash
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,200,2b8027789b5b62674084b0a21abde18f59958989d83f1cbf8e3e04f03ecf8a94
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,190,98be96cf23dcd9b3f3497930fdcf1b45e90704e64459f6d92887107c03cf0518
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,180,b838671462c1e8db38a3160d950862f95fadbc8d42f9efc63c1c922c10e8303f
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,170,a75974c0b66f5a0dc5969fc3aaf64f209223d7339f0371cb18b3fb85e556a60c
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,160,839b0014d96aa743d2c70484900955eecb5b9b0cab9b314bfa0c7944f4512ded
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,150,638683fcdc9c8a68e02e6face341ef7d2f5d087675bf6e3536ed6c4c1dd2d42e
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,140,6cb45cd21dac766cf1632c92d4d80350d65b67162fbc5e5e99c4a32dd729bd0d
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,130,d1adf14c6af5bef2a1dbdd4e531111e41d3f9475499cefa385bebc3ff7c8a969
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,120,fcdaa173392fdd7955b60e1a803c9a5f7c90be608a4b4c8618ab99618fb93aa4
Gakje2fqmwOpPGDLwDucvKEAGs31WkO7,110,57e93a13d8f0c211b83bf8e321be43f0128311f9a8dc6a1a7fcf57961cf326e8
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,100,ec5a4dac6e9474ce11ea15de66738e05755825aa81b2c7f40dd66884ad3d5ef4
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,90,d0553f478f780f04c4c6a247bb68dd1a8d77e39728fc0e2b57ff38bc344b0ae7
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,80,1806da65313e328429c4cb509166c27c7c08df5a3804ecdab0b5b9393b598fcc
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,70,c9afea1a282f95f2697724ef68a2c219568eaa17efe3b1244d642ead53ae599b
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,60,6cecba99c151b8610366a982166174584ffc7b54e77a981e05acaa0c1dba532e
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,50,7741d2b28dff1417e97685d6ce8e568c3b93d472b7ee9776fddf730b62c9b82b
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,40,bd82d617ecbf142c020ee0e85be4e3b02bcfe6114ecf511af32cbd6aac4f2dcc
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,30,132512414d324b2fd9b3583b5c4a832870a9fbc3838dbff8cc1ff88ea02e91eb
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,20,ab68deb739a91a305396d56350dae35f7cd73b6c0fcaddf3b71ff0af2029760e
fBjS09mEwASybjCqv5fPBlcKyyDcja4l,10,1a669e1671ea2903ea6deb1b810c1ab9ac1dcd2d5ef34f3c634e025165c50ce8
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,9,f38fa29dd68d9f6187796745309b234ce9a53396cd69334a97418dc780a7966f
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,8,c72b5378c0c4f3cac5dca54c77b74ff5bf12d71f46ea10a58036ff37b76caab2
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,7,5da789118ed91c538f358fb9a740216bb714455cbecb37ade8a4cb3dae4aaff4
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,6,3733683a59c264f1fcfa1dc0ae199542cc91901841caf07e936601ec535ae75c
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,5,64fe78740bc9a5b91ca73e20d80441ec455b5421625017767f96ed2a81988b22
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,4,d0a72c0201d47e4081474caf9935ed072abc12ef31c5ba1fe580fe9be1a49e4d
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,3,07c8839c1204f1bbe5ad515c592f3f02e6e5c6836c14de2b0aa7bb6e23a1ba69
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,2,92de05f6e1d0232dd29a33745a5db4e6e6d2579d6d2539b96325f36542729901
wsK5QseQgHI5Wr4SclWK6nSYgCl4hmej,1,5c059d8c894a16c21b73701383280d4597c49513b7a5d6f98ea2ad18b2ac40b3
`;

const multi_fund = `
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

const multi_run = `
id,nChunks,chunkSize,hash
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,1,c7082b92e1f8b9dc71f3e43a240adf76557b835422fdf33dd05eb8a68ed94402
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,2,d4440e206da7e14c7c7f37a52ac8a7819d8f9029d5cf28ddaa7866bcb354f82f
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,3,c7894afdbaae5388584863ffd95725adda97b3e3ac7d5a3b02fddf54632b846c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,4,4a153dcc0a0821496376bfd9fd56d30e57572474584ae79a0a6e220e4bf82b09
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,5,7d5cf8349b3ba7825c94e4b5cf283d73fdc4dbf08ca2a2234de95b6c85b13e87
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,6,76f2234d594102007166725e245bbd2360b09c7a6374082e44bdfa8682d38a75
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,7,88ee221e8778a8802e6cb4a74a1455850ce10fc20e41238aacc5abfdfe130cbb
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,8,123fb14e3dc73562f76467d53d37265ca57b3713b571e4a0f62403a528d3ed62
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,9,bb0931f1c4a4ed0acde416df7401949295b8b009a774b34714d499e72dd83846
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,1,10,f3dea4d511f45b962be366b7dd1a1e5fc8fb54ddbb254a5591ea84416a91bde9
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,1,7b1fa39c4e8e0b6254513960257d502941f5fd192fbe0b073d28c55a70e5dfe2
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,2,e13405f34fefc5bd3fe6ae1541b62ec4ff00c26645baedf42677125523afccf3
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,3,28ab9bb68c11cf11f77397d4c4dec1547014e0858d9de0d01702b6eeea7ce1b1
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,4,7c360a7a5bbfae9a9c752a40218eaf1fcfe5135bab1b12b66a3ec50168b563cc
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,5,6fbc947fad1a8601a2f324a9b92c12b3820f0d8631a5231d1d213f0c938b1027
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,6,db9c319e336805be8e3d75887cf8b7e9d93ebb46ad5da0a5c43225b9bde3ad5c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,7,cc7248f9063dc136a0cbca126bcb60fc21466cf52f466c4e60a452b0b58d4832
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,8,0411fb6e3d4a9a59e5d7996d82374fefca78a92254aa0774927b1dea22945bd7
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,9,daa9f91db73910dea0d7e35ebedd8562c2377365c3a5e04cdc2765da389897fe
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,2,10,42a489c74b98be5cf14833c7aa131ca3a21fbbd619c426b11ced89e8147936b2
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,1,af72a4f8f17e443bfcc402d8cf7cc51883a5ca87e33843aeca6b6eb6a7f4654a
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,2,5a03f95aca979ab7bf37f44fa1021c38f7ec9b1555a3d843aa1e0a4455faa058
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,3,7c8e8eb8d35c3279d2e38b278fda14402df23675f83c47ff8ed9398ef520e76c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,4,eac48a246e744b3acd93372cd0ef6c216d0ee7a2899518e1d6e81e9772e451c3
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,5,eb69684f0d61dd917657c98c283acb2628c262a62b85403669ce7ca974406546
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,6,260a8b3257c1ef2b3ab5c9b8a499f2bacb0e241791585f13e0df03deff1c163c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,7,678185d6eed233d4bdfdcce579942a564f35f517106ca23767c9e36528300ecd
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,8,b57e889a317ef68be743e50e51d13f9891f112024cb4631b00c6c0b706e7d98b
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,9,312d9038073d2468fea693515b18f8fcfccbdd5a89c83f37880b39f28773751a
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,3,10,28478817ed0e47bd349a1d6f092cb3cd740d53b844381fed9e32c0844e575dfe
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,1,229f982d958bb5d5084491b0b630cddaed70e0f75c54341db683bed2610fef91
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,2,c5cb8bf2da2a5912db8e3930152b338cb77d0ca03aafa511208baee52aa73efe
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,3,61b4f9229bbd9bde29ff1d7b902ca11d727e5262fb5d7b202cbd1589b67956da
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,4,96d1ca7b009ef3f28d5165e6fd42b7f0c12bcd1c193c5b6b8defcd7eda33d645
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,5,13e5853b23c5aa0d7c25a4e1f3a8ec430e9e1de101f43e611999ca3a00b5b406
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,6,052edbdaff18c79fac5ffe2028920192734ef7668e1b55b3506f919d394cdc52
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,7,70322d914121c659f527fc04fbbacddc41e7b73e6ae195d0e00fe6b275fe1e44
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,8,8e813c03daee4ee14a8a36246daeaf94c21527acad63d9241d9ba7756618f6ea
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,9,0a19b300ce68140f917fb2a7bea6043cdb26107e79e4df958c901fd59556c44c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,4,10,91fbd6913bd8f2b06b39f143916eebe5da1d66394c9e0a26361abb69c5009ebe
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,1,3d8685d8ae0c5e69a2bfb02e82b9729860f2d59f56cedccdb66965189758bbf6
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,2,c7d0eefc26ac79b13614afa621debd34167d3344ed0bc0aa5aea225706081a71
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,3,929b29e9139f11d6fc5a9cc5ee69fa9246e4bd98a1d70681480d2a9fcdc344a7
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,4,5868054a6e3d0cb94d15d64b3e6e375f01195502066c6aa38e5ddde816ff8e09
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,5,ac9a4d1f93acd74b9f751f56e1e13b31b3042d3d8294ea105f28cd6ff4386505
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,6,b70fbdad3dad17cfa4458365804387130dc5db226fb3a6e6dc3b4b03ce85980e
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,7,1db9de8abeec0c129d2b2d6b84c823450953f9883041b7869e37275eee13b9ad
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,8,fb37374604245c40b6e595c94d7cb9c7c8a8e6ba9033d97ecf7d59c7e893aee6
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,9,9f7e1430d571c660ee3b4b0ce143d32746328572383ce5adff808ea4fa26275f
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,5,10,f506ffb9f7160f6d6746614e72a3e4ffd9058e19a2ba170c5d9b9d4d61b482bf
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,1,7a6d2d6e69d9d882d1cc722ed8708787383d1cb898a1ac0ac4d43f26a07b4ada
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,2,241b7a1d7a8f7d8e9e6378853edc0dac2e474b8889397830743990adfc1b8d0f
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,3,3436490941af5f67bf9c8a46de8faa22e8e24e3e17ec868b1767ae2ce61f8fad
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,4,5708e12f0bd85a737f3166bd987e1ca56a1a1279cf5c0504570ccef05266376c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,5,aaf3ed0e5390a913c92e4ac108d6a90cb98bd8b4f6543b7f56ec0f793e47af03
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,6,1958c8fd33c9468a3794af02f373298ad9dce249e892d394232614733e5cad1f
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,7,e953df485354d58360da1d6b1c3e4821bdbc0718fc5052c378707a203e6dac41
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,8,6e3394f8a34624a1509cd04d0281ac327c0e8871c28698e68d848262b816835b
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,9,6f5260826bd9c328a696bb6cfa9625fd1ae7ac60b642e3e51658ebce2df8cc44
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,6,10,99b749b25cb42e4365b52d4c5295157360d49d83ed0740cc9a8590a4baad2e01
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,1,08600453249d39d008f82159e2a321e5fc19681c73cb1d5923bba92ab10025a3
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,2,6712bf3bb01933cceb6b4ce90db4adb488e141da0f5297210a08c65f604cd902
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,3,d460eec8e0b07fef3a712633674b021b55c72636d9fe6a2600b8f7f48df7a12a
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,4,9b694f64539e2854d3f12f1ea8b8927205723f8d2a5cca5dcee8e2068ffd8579
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,5,6d615a77872e99e1cab8419505f92f09fb9768e1ba88e78096141861a5067624
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,6,b67a63d979928377defcf07740acf8b193300cd5db30e1876a1c9a057de90c7d
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,7,4c21a81bdb0b5977fd39ff8d7b9a61f719206e4d27c63cb19c091aaaeb514a97
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,8,6f744d30f6ac3c44567de60d7902a8c35ce9ed2f1cd18dfb9f4d0237e6757603
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,9,c40d6a1801f912cf43276956d08b972c3006e7c387c74ee1eed7238cf5958e06
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,7,10,e8a0e5eb28870cca79f4b97cfdc65ce7bec766080e4385d364433b53b6e5ff99
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,1,30c32d4511f09aad05f7f3799aec9039bf7ef9dfee99c3cf34a7416ab9231150
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,2,7dbebd012e07ca3929e3590b15ddf0a86ab419aa0de2864dd4c4f12a5752bee6
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,3,f9515c3e4a417a114aad5b4fbd5f6595aed22687781603f3499a4934b7790500
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,4,9eda972858d5b5ff60d1967afc94334e9d4de945bd18204f21c2ec487b854c54
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,5,3a2910d54c2b19b385b52ef116d9446a6c30c29d0a9cbb0bd744657c0d424981
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,6,c34340be33b8bdb737e33bd55c05c037d3f6cf905d08d8d21b75650c6fac25b3
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,7,199640d36ee48c9d6098ee9ffa70818a7dfb41de330b8649b6c7a42f4053a14c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,8,1eb9ad9018d598ad4f792ed65134a819ea23283b96fd509b7e014674b90fb65e
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,9,9d9ea0c31bb4dcf0292eb96c6a98bd751fc493e09732dfe5f6e31fbe38b1ba13
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,8,10,491dc595b6615280668d1af9b80f9a6d5447a53169da04446164b17dacdb5726
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,1,398b4a6917a41ca1406147e4280fe77412ce578a13233bfb340fe6dbb4e4c6ea
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,2,7da2b351ba7ed3ea6438b7b5c8d777153fa0921836e37f65d25de4dad2af0157
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,3,f73edd8ffd24d0847088a1aea059bc395f6345328a021bf9a817e9f6a255827c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,4,99d846c9c343d13bb1f6ff019d4a8a8d273576aa1250afe45c3b5cb1f6a10f96
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,5,5538b85b8dcf6f9fd4215b7e15475157af677c26b1471566640f867f5a67926d
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,6,1adfaab6a80317f7b1a68bfdc2523c6116e88c0d6d1f00f422094285de1b46df
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,7,42d0d2183ce50b8e7422be99fdcf5e7ba29dfeff93f71dc640ae39f5378e8d67
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,8,9fdc824acad23451167f1fae15aa4a228c183f9507cca920ad611eac9c6ead4d
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,9,edcb8e819aa597ab15e40d27e326dd8cd830d8b563bc3e84991f408ee0dcd550
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,9,10,bc9426ba790545e6e71ce887dc83beed5dec32f7bd3ffda62065ef6c6736db5d
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,1,b86882926f641158fca4db5d3009a161e8e0b0b46887932d79abf7c2e356bc56
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,2,8ece0ed15700c7f8d7606b5c69811fe2214b6be7ac625b3f43f0195e92f285cf
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,3,bb182ed9324d49661c92a4f790942455493b4559db8dc60ea81d265ab1f92763
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,4,4406836c6fbbcdd32b55241014fa99be988f6f17e694b3d8a0e5b8130e007428
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,5,d2e0727ec897d5c4a4d291c9fef20e76c4413195af2e8cfffbf927782608f283
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,6,0f317ab6eb0cf9c8d7c19cc3923f8e999a5a816faf542fabd58f19475784024c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,7,93610c024cf98e2b5ab3116e708a72301ddce64bbb53d7eacde9688eba2b9d13
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,8,87498e414e06ca562eddaf41b7a7857fd798ae19e423114a3f52f6b3b60f80ee
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,9,7f99105ebffceb35d108f1a69617e7b6d6e113c8363003731c83b11bbcdf729c
KwslJ4054LYvm4NYVXPZtvq9hpR8tqFV,10,10,a36b442c5430e8dde0060958d17bd1cae772568724b6201e44624ceecfa9996a
`;

if (import.meta.main) {
  // ===================================================================================================================================================================
  // Single Fund Csv
  // ===================================================================================================================================================================

  const single_fund_csv = parse(single_fund, {
    columns: ["id", "size", "hash"],
    skipFirstRow: true,
  });
  const single_fund_fees = Array<
    { id: string; size: number; bytes: number; fees: number; hash: string }
  >();
  for (const row of single_fund_csv) {
    const id = row.id;
    const size = parseInt(row.size);
    const hash = row.hash;
    const tx = await queryTx(hash);
    const bytes = tx.size;
    const fees = parseInt(tx.fees);
    const record = { id, size, bytes, fees, hash };
    console.log(record);
    single_fund_fees.push(record);
    await waitSeconds(1);
  }
  const single_fund_fees_csv = stringify(single_fund_fees, {
    columns: [
      "id",
      "size",
      "bytes",
      "fees",
      "hash",
    ],
  });
  console.log(single_fund_fees_csv);

  // ===================================================================================================================================================================
  // Single Run Csv
  // ===================================================================================================================================================================
  const single_run_csv = parse(single_run, {
    columns: ["id", "size", "hash"],
    skipFirstRow: true,
  });

  // ===================================================================================================================================================================
  // Multi Fund Csv
  // ===================================================================================================================================================================
  const multi_fund_csv = parse(multi_fund, {
    columns: ["id", "nChunks", "chunkSize", "hash"],
    skipFirstRow: true,
  });

  // ===================================================================================================================================================================
  // Multi Run Csv
  // ===================================================================================================================================================================
  const multi_run_csv = parse(multi_run, {
    columns: ["id", "nChunks", "chunkSize", "hash"],
    skipFirstRow: true,
  });
}
