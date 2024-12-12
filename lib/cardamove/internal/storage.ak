use aiken/collection/dict
use cardamove/primitive.{Mv_Address}

pub type Storage<t> =
  dict.Dict<Mv_Address, t>

pub fn has_key(storage: Storage<t>, address: Mv_Address) -> Bool {
  dict.has_key(storage, address)
}

pub fn get(storage: Storage<t>, address: Mv_Address) -> Option<t> {
  dict.get(storage, address)
}

pub fn insert(storage: Storage<t>, address: Mv_Address, value: t) -> Storage<t> {
  dict.insert(storage, address, value)
}

pub fn delete(storage: Storage<t>, address: Mv_Address) -> Storage<t> {
  dict.delete(storage, address)
}

pub fn from_pairs(pairs: Pairs<Mv_Address, t>) -> Storage<t> {
  dict.from_pairs(pairs)
}
