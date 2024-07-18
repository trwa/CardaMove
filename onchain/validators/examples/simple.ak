use aiken/transaction.{ScriptContext}
use cardamove/cardano/contract.{
  ContractDatum, ContractRedeemer, contract_validate,
}
use cardamove/move/storage.{MoveState, return}

// =====================================================================================================================
// Storage
// =====================================================================================================================

type StorageUtxo {
  StorageUtxo
}

type StorageGlobal {
  StorageGlobal
}

fn storage_global_aggregate(
  _: List<ContractDatum<StorageUtxo>>,
) -> StorageGlobal {
  StorageGlobal
}

// Questo va qui perche' non si possono expectare tipi generici.
fn datum_downcast(data: Data) -> ContractDatum<StorageUtxo> {
  expect datum: ContractDatum<StorageUtxo> = data
  datum
}

// =====================================================================================================================
// Script
// =====================================================================================================================

type Dummy {
  Dummy
}

fn script(_: Dummy) -> MoveState<StorageGlobal, Void> {
  return(Void)
}

validator {
  fn run(
    datum: ContractDatum<StorageUtxo>,
    redeemer: ContractRedeemer<Dummy>,
    context: ScriptContext,
  ) {
    contract_validate(
      datum,
      redeemer,
      context,
      script,
      storage_global_aggregate,
      datum_downcast,
    )
  }
}
