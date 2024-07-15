import { useRef, useState } from "preact/hooks";
import {
  Constr,
  Data,
  fromHex,
  fromText,
  Lucid,
  OutRef,
  paymentCredentialOf,
  toHex,
  toText,
} from "lucid/mod.ts";
import { Input } from "../components/Input.tsx";
import { Button } from "../components/Button.tsx";
import { readValidators } from "../utils/utils.ts";


import { addressFromPaymentCredential} from "../utils/conversion.ts";
import { Network } from "https://deno.land/x/lucid@0.10.7/src/mod.ts";

interface BidderProps {
  lucid: Lucid;
}

export default function Bidder({ lucid }: BidderProps) {
  const [auctionAddress, setAuctionAddress] = useState<string>("");
  const [auctionDetails, setAuctionDetails] = useState<any | null>(null);
  const [bidAmount, setBidAmount] = useState<string>("");
  const [bidderAddr, setBidderAddr] = useState<string>("");
  const [bidderAddrStack, setBidderAddrStack] = useState<string>("");
  const [prevAmount, setprevAmount] = useState<string>("");
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string | null>(null);
  const auctionAddressRef = useRef<string | null>(null); // Use a ref for the auction address
  const [bidTxHash, setBidxHash] = useState<string>("");

  const fetchAuctionDetails = async (e: Event) => {
    e.preventDefault();
    setLoading(true);
    setError(null);

    try {

      /*const utxos: OutRef = { txHash: auctionAddress, outputIndex: 0 };
      console.log("utxos", utxos)
      const auctionUtxos = await lucid.utxosByOutRef([utxos]);
      console.log("auctionUtxos",auctionUtxos )
      const auctionUtxo = auctionUtxos[0];
      console.log("utxo",auctionUtxo)  */


      const utxos = await lucid.utxosAt(auctionAddress);
      console.log("UTXO Fetch From Address", utxos);
      if (utxos.length === 0) {
        setError("No UTXOs available at the given address.");
        setLoading(false);
        return;
      }
  
      // Get the last UTXO (most recent)
      const auctionUtxo = utxos[0];
     console.log("the latest UTXo from the list", auctionUtxo);

      const datum = await lucid.datumOf(auctionUtxo);
      console.log("datum", datum);
      const auctionDatum = Data.from(datum) as Constr;
      console.log("auctionDatum", auctionDatum);

      // Decode the bidder field from hex (assumed to be a Verification Key Hash)
     // const bidderHex = auctionDatum.fields[4].toString();
     const bidderHex = auctionDatum.fields[4];
      console.log("bidderHex", bidderHex);
      //const bidder = toText(bidderHex);
     // console.log("bidder", bidder);
      setBidderAddr(bidderHex);
      setBidderAddrStack(auctionDatum.fields[5]);

      const details = {
        object: new TextDecoder().decode(
          fromHex(auctionDatum.fields[1].toString()),
        ),
        deadline: auctionDatum.fields[2].toString(),
        status: auctionDatum.fields[3] as Constr,
        bidder: bidderHex,
        bidderStack: auctionDatum.fields[5],
        amount: auctionDatum.fields[6].toString(),
      };

      setprevAmount(details.amount);

      console.log("details fetch from auction ", details);
      console.log("Object", details.object);

      setAuctionDetails(details);
    } catch (err) {
      setError(`Error fetching auction details: ${err.message}`);
    } finally {
      setLoading(false);
    }
    
    const walletAddress = await lucid.wallet.address();
    console.log("Wallet Address:", walletAddress);
    const hesh = fromText(walletAddress);
    console.log("hex of Wallet Address:", hesh);

    const walletagain= toText(hesh);

    console.log("walletagain Wallet Address:", walletagain);

    const fff=  paymentCredentialOf(await lucid.wallet.address())
      
      console.log("paymentCredentialOf:", fff);

      const bidderAddressDetails = lucid.utils.getAddressDetails(
        await lucid.wallet.address(),
      );

      console.log("bidderAddressDetails:", bidderAddressDetails);

      const biddrePublicKeyHash = bidderAddressDetails.paymentCredential?.hash;

      console.log("biddrePublicKeyHash:", biddrePublicKeyHash);
      const stakeCredential = bidderAddressDetails.stakeCredential;

      const paymentCredential: Credential = {
        type: "Key",
        hash: biddrePublicKeyHash,
      };
      
      const network: Network = "Preview";
      
      const address = addressFromPaymentCredential(paymentCredential, network,stakeCredential);
      console.log("Address:", address);
      const walletAddr = await lucid.wallet.address();
      console.log("Address:", walletAddr);

  };

  const placeBid = async (e: Event) => {
    e.preventDefault();

    if (!lucid || !auctionAddress) {
      console.error("Lucid or auction address is not initialized");
      return;
    }

    try {
     // const utxos: OutRef = { txHash: auctionAddress, outputIndex: 0 };
     // const auctionUtxos = await lucid.utxosByOutRef([utxos]);
     const auctionUtxos = await lucid.utxosAt(auctionAddress);
      if (!auctionUtxos || auctionUtxos.length === 0) {
        throw new Error("No UTXOs available at the auction address.");
      }
      // Get the last UTXO (most recent)
      const auctionUtxo = auctionUtxos[0];
      console.log("auctionUTXO", auctionUtxo);
      const datum = await lucid.datumOf(auctionUtxo);
      console.log("datum", datum);
      const auctionDatum = Data.from(datum) as Constr;
      console.log("auctionDatum", auctionDatum);

      const newBidAmount = BigInt(Number(bidAmount) * 1000000);
      console.log("newBidAmount", newBidAmount);

      const redeemer = Data.to(new Constr(1, [])); // Bid redeemer
      console.log("BidRedeemer", redeemer);

      const bidderAddressDetails = lucid.utils.getAddressDetails(
        await lucid.wallet.address(),
      );
      const bidderPublicKeyHash = bidderAddressDetails.paymentCredential?.hash;
      const bidderStackKeyHash = bidderAddressDetails.stakeCredential?.hash;

      const bidDatum = Data.to(
        new Constr(0, [
          auctionDatum.fields[0], // seller
          auctionDatum.fields[1], // object
          auctionDatum.fields[2], // deadline
          new Constr(1, []), // STARTED status
          bidderPublicKeyHash, // bidder
          bidderStackKeyHash,
          newBidAmount, // new amount
        ]),
      );

      console.log("newAuctionDatum", bidDatum);
      const auctionDatumcheck = Data.from(bidDatum) as Constr;
      console.log("auctionDatum", auctionDatumcheck);



      // The one we pass to the contract to let the old bidder withdraw their amount
const withdraw_datum = Data.to(
  new Constr(0, [
    auctionDatum.fields[0], // seller
    auctionDatum.fields[1], // object
    auctionDatum.fields[2], // deadline
    new Constr(2, []),       // Status OUTBID 
    bidderAddr, 
    bidderAddrStack,           // old bidder
    BigInt(prevAmount), // new amount
  ]),
);

console.log("withdraw_datum", withdraw_datum);

const withdraw_datumchech = Data.from(withdraw_datum) as Constr;
      console.log("auctionDatum", withdraw_datumchech);

      const walletUtxos = await lucid.wallet.getUtxos();
      console.log("Fetched UTXOs:", walletUtxos);

      if (!walletUtxos || walletUtxos.length === 0) {
        throw new Error(
          "No UTXOs available in the wallet. Please ensure the wallet has sufficient funds.",
        );
      }

      const walletUtxo = walletUtxos[0];
      console.log("Using UTXO:", walletUtxo);

      const validators = readValidators();
      const contractScript = {
        type: validators.auction.type,
        script: validators.auction.script,
      };

      const scriptAddress = lucid.utils.validatorToAddress(contractScript);

      console.log("scriptAddress:", scriptAddress);
  
      auctionAddressRef.current = scriptAddress; // Set the auction address in the ref

      const timeOffset = 60 * 1000; // one minute
      const currentTime = new Date().getTime() - timeOffset;

      const paymentCredential: Credential = {
        type: "Key",
        hash: bidderAddr,
      };
      
      const stackCredential: Credential = {
        type: "Key",
        hash: bidderAddrStack,
      };
      

      const network: Network = "Preview";
      
      const addressPrev = addressFromPaymentCredential(paymentCredential, network,stackCredential);
      console.log("addressPrevious:", addressPrev);
      
      
      const tx = await lucid
      .newTx()
      .collectFrom([auctionUtxo], redeemer)
      .addSigner(await lucid.wallet.address())
      .payToContract(scriptAddress, { inline: bidDatum }, { lovelace: newBidAmount })
      .payToContract(addressPrev, { inline: withdraw_datum}, { lovelace: BigInt(prevAmount) })
      .attachSpendingValidator(contractScript)
      .validFrom(currentTime)
      .payToAddress(addressPrev, { lovelace: BigInt(prevAmount) })
      .complete();

    

      const txSigned = await tx.sign().complete();
      const txHash = await txSigned.submit();
      await lucid.awaitTx(txHash);
      console.log("Bid placed with tx hash:", txHash);


      setBidxHash(txHash)
      // Refresh auction details
      const updatedUtxosRef: OutRef = { txHash: txHash, outputIndex: 0 };
      console.log("updatedUtxos", updatedUtxosRef)
      const updatedUtxos = await lucid.utxosByOutRef([updatedUtxosRef]);
      console.log("updatedUtxos", updatedUtxos)
      const updatedAuctionUtxo = updatedUtxos[0];
      console.log("updatedAuctionUtxo", updatedAuctionUtxo)
      const updatedDatum = await lucid.datumOf(updatedAuctionUtxo);
      console.log("updatedDatum", updatedDatum)

      const updatedAuctionDatum = Data.from(updatedDatum) as Constr;
      console.log("updatedAuctionDatum", updatedAuctionDatum)

      const bidderHex = updatedAuctionDatum.fields[4];
      console.log("bidderHex", bidderHex)
      const bidderStack = updatedAuctionDatum.fields[5];
      console.log("bidderStack", bidderStack)

      const oldBid = updatedAuctionDatum.fields[6];
      console.log("oldBid", oldBid)
      setprevAmount(oldBid);

     // const bidder = toText(bidderHex);
      setBidderAddr(bidderHex);
      setBidderAddrStack(bidderStack);
      const updatedDetails = {
        object: new TextDecoder().decode(
          fromHex(updatedAuctionDatum.fields[1].toString()),
        ),
        deadline: updatedAuctionDatum.fields[2].toString(),
        status: updatedAuctionDatum.fields[3] as Constr,
        bidder: bidderAddr,
        bidderStack: bidderAddrStack,
        amount: updatedAuctionDatum.fields[6].toString(),
      };

      setAuctionDetails(updatedDetails);
    } catch (error) {
      console.error("Error placing bid:", error);
      setError(`Error placing bid: ${error.message}`);
    }
  };

 const withDrawBid = async (e: Event) => {
    e.preventDefault();

    if (!lucid || !auctionAddress) {
      console.error("Lucid or auction address is not initialized");
      return;
    }

    try {
     
      const utxo_ref: OutRef = { txHash: bidTxHash,outputIndex: 1 }; // note the outputIndex set to 1
      const [utxo] = await lucid.utxosByOutRef([utxo_ref]);

      const redeemer = Data.to(new Constr(2, []));

      const timeOffset = 60 * 1000; // one minute
    const currentTime = new Date().getTime() - timeOffset;

    const validators = readValidators();
      const contractScript = {
        type: validators.auction.type,
        script: validators.auction.script,
      };

      const scriptAddress = lucid.utils.validatorToAddress(contractScript);



    const tx = await lucid
    .newTx()
    .collectFrom([utxo], redeemer)
    .addSigner(await lucid.wallet.address())
    .attachSpendingValidator(contractScript)
    .validFrom(currentTime)
    .complete();
     
      const txSigned = await tx.sign().complete();
      const txHash = await txSigned.submit();
      await lucid.awaitTx(txHash);

      console.log("Bid withdraw with tx hash:", txHash);
    } catch (error) {
      console.error("Error withdraw bid:", error);
      setError(`Error placing bid: ${error.message}`);
    }
  };

  return (
    <div>
      <form
        class="mt-10 grid grid-cols-1 gap-y-8"
        onSubmit={fetchAuctionDetails}
      >
        <Input
          type="text"
          id="auctionAddress"
          value={auctionAddress}
          onInput={(e) => setAuctionAddress(e.currentTarget.value)}
        >
          Auction Address
        </Input>

        <Button type="submit">Fetch Auction Details</Button>
      </form>

      {loading ? <p>Loading auction details...</p> : auctionDetails
        ? (
          <div>
            <h2>Auction Details</h2>
            <p>Object: {auctionDetails.object}</p>
            <p>
              Deadline: {new Date(Number(auctionDetails.deadline) * 1000)
                .toLocaleString()}
            </p>

            <p>
              Current Bid Amount: {Number(auctionDetails.amount) / 1000000} ADA
            </p>

            <form onSubmit={placeBid}>
              <Input
                type="number"
                id="bidAmount"
                value={bidAmount}
                onInput={(e) => setBidAmount(e.currentTarget.value)}
              >
                Your Bid Amount (ADA)
              </Input>
              <Button type="submit">Place Bid</Button>
            </form>

            <form onSubmit={withDrawBid}>
              <Button type="submit">WithDraw Bid</Button>
            </form>
          </div>
        )
        : (
          !loading && <p>No auction details available.</p>
        )}
      {error && <p>{error}</p>}
    </div>
  );
}
