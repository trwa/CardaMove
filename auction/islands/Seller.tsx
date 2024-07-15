// deno-lint-ignore-file
import { useRef, useState } from "preact/hooks";
//import { Constr, Data, fromText, Lucid } from "lucid/mod.ts";


import { Blockfrost, Constr, Data, fromText, Lucid, networkToId, OutRef , Network, Utils, paymentCredentialOf } from "lucid/mod.ts";

import { Input } from "../components/Input.tsx";
import * as constants from "../utils/constants.ts";
import { Button } from "../components/Button.tsx";
import { applyParams, readValidators } from "../utils/utils.ts";
import { NetworkInfo } from "https://deno.land/x/lucid@0.10.7/src/core/libs/cardano_multiplatform_lib/cardano_multiplatform_lib.generated.js";
export interface SellerProps {
  lucid: Lucid;
}

export default function Seller({ lucid }: SellerProps) {
  const [object, setObject] = useState<string>("");
  const [deadline, setDeadline] = useState<string>("");
  const [bidAmount, setBidAmount] = useState<string>("");
  const [auctionCreateTxHash, setAucCreateTxHash] = useState<string>("");
  const [auctionStartTxHash, setAuctionStartTxHash] = useState<string | null>(
    null,
  );
  const [blockfrostAPIKey, setBlockfrostAPIKey] = useState<string>("");

  const [waitingAuctionTx, setWaitingAuctionTx] = useState<boolean>(false);

  //const [auctionAddress, setAuctionAddress] = useState<string | null>(null);
  const auctionAddressRef = useRef<string | null>(null); // Use a ref for the auction address

  

  const onSubmit = async (e: Event) => {
    e.preventDefault();

    if (!lucid) {
      console.error("Lucid is not initialized");
      return;
    }

    const validators = readValidators();
    console.log("validators:", validators);

    /* const contracts = applyParams(
      object,
      parseInt(deadline),
      validators,
      Lucid,
    );    */

    const scriptHex = {
      type: validators.auction.type,
      script: validators.auction.script,
    };

    const scriptAddress = lucid.utils.validatorToAddress(scriptHex);

    console.log("scriptAddress:", scriptAddress);

    auctionAddressRef.current = scriptAddress; // Set the auction address in the ref

    /* console.log("Auction details submitted:", contracts);
    console.log("SmartContractAddress:", contracts.auctionAddress);   */
    //setAuctionAddress(contracts.auctionAddress);

    //  auctionAddressRef.current = contracts.auctionAddress; // Set the auction address in the ref

    console.log(
      "SmartContractAddressAfterMakeing global:",
      auctionAddressRef.current,
    );

   
    console.log("Creating auction with details:", { object, deadline });

    try {
      const sellerAddressDetails = lucid.utils.getAddressDetails(
        await lucid.wallet.address(),
      );

      console.log("sellerAddressDetails:", sellerAddressDetails);

      const sellerPublicKeyHash = sellerAddressDetails.paymentCredential?.hash;
      const stakeCredential = sellerAddressDetails.stakeCredential?.hash;
      

      console.log("sellerPublicKeyHash:", sellerPublicKeyHash);

     const fff=  paymentCredentialOf(await lucid.wallet.address())
      const walletAddress = await lucid.wallet.address();
      console.log("Wallet Address:", fff);


      /*
      // Convert Wallet Address to Public Key Hash
      const addressDetails = lucid.utils.getAddressDetails(walletAddress);
      const paymentKeyHash = addressDetails.paymentCredential?.hash;
      
      if (!paymentKeyHash) {
        throw new Error("Payment key hash not found in the address details.");
      }
      
      console.log("Payment Key Hash:", paymentKeyHash);
      
      // Convert Public Key Hash back to Wallet Address
      const reconstructedAddress = lucid.utils.credentialToAddress(
        { type: "Key", hash: paymentKeyHash },
      );
      



      console.log("Reconstructed Wallet Address:", reconstructedAddress);
      
      // Ensure the reconstructed address matches the original
      if (walletAddress !== reconstructedAddress) {
        throw new Error("Reconstructed address does not match the original address.");
      }
      
      console.log("Address conversion verified successfully.");


*/


      console.log("seller has:", fromText(await lucid.wallet.address()));
      console.log("object:", fromText(object));
      console.log("deadline:", BigInt(deadline));
      console.log("NOT_STARTED status:",  new Constr(0, []));
      console.log("bidder:", fromText(await lucid.wallet.address()));
      console.log("Starting amount:", BigInt(0));
            
      setWaitingAuctionTx(true);
      const auctionDatum = Data.to(
        new Constr(0, [
          sellerPublicKeyHash, // seller
          fromText(object), // object
          BigInt(deadline), // deadline
          new Constr(0, []), // NOT_STARTED status
          sellerPublicKeyHash, // bidder
          stakeCredential,     
          BigInt(0), // Starting amount
        ]),
      );




      console.log("Auction datum created:", auctionDatum);
      
      const lovelace = Number(2) * 1000000; // Use a minimum of 2 ADA to cover the transaction cost

      const utxos = await lucid.wallet.getUtxos();
      console.log("Fetched UTXOs:", utxos);

      if (!utxos || utxos.length === 0) {
        throw new Error(
          "No UTXOs available in the wallet. Please ensure the wallet has sufficient funds.",
        );
      }

      const utxo = utxos[0];
      console.log("Using UTXO:", utxo);

      const timeOffset = 60 * 1000; // one minute
      const currentTime = new Date().getTime() - timeOffset;

      const tx = await lucid
        .newTx()
        .payToContract(scriptAddress,{ inline: auctionDatum }, {
          lovelace: BigInt(lovelace),
        }).validFrom(currentTime)
        .complete();

      console.log("Transaction constructed:", tx);

      const txSigned = await tx.sign().complete();
      console.log("Transaction signed:", txSigned);

      const txHash = await txSigned.submit();
      console.log("Transaction submitted:", txHash);

      const success = await lucid.awaitTx(txHash);
      console.log("Transaction success:", success);

      setTimeout(() => {
        setWaitingAuctionTx(false);

        if (success) {
          setAucCreateTxHash(txHash);
        }
      }, 3000);
    } catch (error) {
      console.error("Error creating auction:", error);
      setWaitingAuctionTx(false);
    }
  };
  const startAuction = async (e: Event) => {
    e.preventDefault();

    if (!lucid) {
      console.error("Lucid is not initialized");
      return;
    }

    const auctionAddress = auctionAddressRef.current; // Get the auction address from the ref
    if (!auctionAddress) {
      console.error("Auction address is not set");
      return;
    }

    console.log("SmartContractAddress:", auctionAddress);

    setWaitingAuctionTx(true);
    console.log("Starting auction with details:", { object, deadline });

    const validators = readValidators();
    console.log("validators:", validators);
    const contractScript = {
      type: validators.auction.type,
      script: validators.auction.script,
    };

    const sellerAddressDetails = lucid.utils.getAddressDetails(
      await lucid.wallet.address(),
    );
    const sellerPublicKeyHash = sellerAddressDetails.paymentCredential?.hash;
    const stakeCredential = sellerAddressDetails.stakeCredential?.hash;

    



 /*const getAddressFromPaymentKeyHash = (sellerPublicKeyHash: string): string => {
    
    return lucid.utils.credentialToAddress(lucid.utils.keyHashToCredential(sellerPublicKeyHash));
  };


  console.log("address", getAddressFromPaymentKeyHash)   */


    const lovelace = Number(bidAmount) * 1000000;
    console.log("lovelace:", lovelace);
    try {
      const startDatum = Data.to(
        new Constr(0, [
          sellerPublicKeyHash, // seller
          fromText(object), // object
          BigInt(deadline), // deadline
          new Constr(1, []), // STARTED status
          sellerPublicKeyHash, // Initial bidder is seller
          stakeCredential,
          BigInt(lovelace), // Starting amount
        ]),
      );

      const redeemer = Data.to(new Constr(0, [])); // start redeemer

      console.log("Start datum created:", startDatum);

      const utxos = await lucid.wallet.getUtxos();
      console.log("Fetched UTXOs:", utxos);

      if (!utxos || utxos.length === 0) {
        throw new Error(
          "No UTXOs available in the wallet. Please ensure the wallet has sufficient funds.",
        );
      }

      const utxo = utxos[0];
      console.log("Using UTXO:", utxo);

      console.log("WalletAddress:", await lucid.wallet.address());


      const utxo_ref: OutRef = { txHash: auctionCreateTxHash, outputIndex: 0 };
      console.log("utxo_ref", utxo_ref)
      const utxor_ref = await lucid.utxosByOutRef([utxo_ref]);
      console.log("utxor_ref",utxor_ref )
      const utxor = utxor_ref[0];
      console.log("utxor_ref",utxor )
      
     /*   const auctionUtxos = await lucid.utxosAt(auctionCreateTxHash);
      console.log("auctionUtxos:", auctionUtxos);
      
      if (!auctionUtxos || auctionUtxos.length === 0) {
        throw new Error("No UTXOs found at the auction address.");
      }

      const auctionUtxo = auctionUtxos[0]; // Assuming there's only one UTXO at the auction address initially
      console.log("auctionUtxo:", auctionUtxo);  */

      const timeOffset = 60 * 1000; // one minute
      const currentTime = new Date().getTime() - timeOffset; 
      console.log("timeOffset",timeOffset )
      console.log("currentTime",currentTime )

       // Extract lovelace from the UTXO's assets
       const lovelacetoPayback = utxor_ref[0].assets;

       console.log("lovelacetoPayback",lovelacetoPayback )
     
      const tx = await lucid
        .newTx()
        .collectFrom([utxor_ref[0]], redeemer)
        .addSigner(await lucid.wallet.address())
        .payToContract(auctionAddress, {inline:startDatum }, { lovelace: BigInt(lovelace) })
        .attachSpendingValidator(contractScript)
        .validFrom(currentTime)
        .payToAddress(await lucid.wallet.address(),lovelacetoPayback
        )
        .complete();


/*
      // Fetch the UTXO from the auction address after the transaction is confirmed
      const auctionUtxos = await lucid.utxosAt(auctionCreateTxHash);

      console.log("auctionUtxos:", auctionUtxos);

      if (!auctionUtxos || auctionUtxos.length === 0) {
        throw new Error("No UTXOs found at the auction address.");
      }

      const auctionUtxo = auctionUtxos[0]; // Assuming there's only one UTXO at the auction address initially

      console.log("auctionUtxo:", auctionUtxo);

      // const redeemer2 = Data.to(new Constr(2, [])); // start redeemer

      const tx2 = await lucid
        .newTx()
        .collectFrom([auctionUtxo], redeemer) // Collect the auction UTXO
        .payToAddress(await lucid.wallet.address(), {
          lovelace: BigInt(2000000),
        }).attachSpendingValidator(contractScript)
        .complete();   */

      console.log("Transaction constructed:", tx);

      const txSigned = await tx.sign().complete();
    //  const txSigned2 = await tx2.sign().complete();
      console.log("Transaction signed:", txSigned);

      const txHash = await txSigned.submit();
     // const txHash2 = await txSigned2.submit();
      console.log("Transaction submitted:", txHash);

      const success = await lucid.awaitTx(txHash);
     // const success2 = await lucid.awaitTx(txHash2);
      console.log("Transaction success:", success);

      setTimeout(() => {
        setWaitingAuctionTx(false);

        if (success) {
          setAuctionStartTxHash(txHash);
        }
      }, 3000);
    } catch (error) {
      console.error("Error starting auction:", error);
      setWaitingAuctionTx(false);
    }
  };

  return (
    <div>
      <form class="mt-10 grid grid-cols-1 gap-y-8" onSubmit={onSubmit}>
        <Input
          type="text"
          id="object"
          value={object}
          onInput={(e) => setObject(e.currentTarget.value)}
        >
          Auction Object
        </Input>

        <Input
          type="text"
          id="deadline"
          value={deadline}
          onInput={(e) => setDeadline(e.currentTarget.value)}
        >
          Deadline (POSIX time)
        </Input>

        <Input
          type="number"
          id="bidAmount"
          value={bidAmount}
          onInput={(e) => setBidAmount(e.currentTarget.value)}
        >
          Starting Bid Amount (ADA)
        </Input>

        <Button
          type="submit"
          disabled={waitingAuctionTx || !!auctionCreateTxHash}
        >
          {waitingAuctionTx ? "Waiting for Tx..." : "Create Auction"}
        </Button>

        {auctionCreateTxHash && (
          <>
            <h3 class="mt-4 mb-2">Auction Created</h3>
            <a
              class="mb-2"
              target="_blank"
              href={`https://preview.cardanoscan.io/transaction/${auctionCreateTxHash}`}
            >
              {auctionCreateTxHash}
            </a>
          </>
        )}

        {auctionAddressRef.current &&
          (
            <>
              <h3 class="mt-4 mb-2">Auction Address</h3>
              <a
                class="mb-2"
                target="_blank"
                href={`https://preview.cardanoscan.io/address/${auctionAddressRef.current}`}
              >
                {auctionAddressRef.current}
              </a>
            </>
          )}
      </form>

      {auctionCreateTxHash && (
        <Button
          onClick={startAuction}
        >
          {waitingAuctionTx ? "Waiting for Tx..." : "Start Auction"}
        </Button>
      )}

      {auctionStartTxHash && (
        <>
          <h3 class="mt-4 mb-2">Auction Started</h3>
          <a
            class="mb-2"
            target="_blank"
            href={`https://preview.cardanoscan.io/transaction/${auctionStartTxHash}`}
          >
            {auctionStartTxHash}
          </a>
        </>
      )}
    </div>
  );
}
