use aiken/hash.{Blake2b_256, Hash, blake2b_256}
use aiken/transaction.{Transaction}

// The txbody is expected to be a Hex encoded string submitted by the off-chain code.
pub fn txbody_hash(txbody: ByteArray) -> Hash<Blake2b_256, Transaction> {
    blake2b_256(txbody)
}
