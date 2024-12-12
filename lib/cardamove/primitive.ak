pub type Mv_Unit =
  Void

pub fn mv_unit() -> Mv_Unit {
  Void
}

pub type Mv_Bool =
  Bool

pub type Mv_Int =
  Int

pub type Mv_Address =
  ByteArray

pub type Mv_Signer =
  ByteArray

pub type Mv_Vector<t> =
  List<t>

pub type Mv_Reference<t> {
  address: Mv_Address,
  value: t,
}

pub fn dereference(a: Mv_Reference<t>) -> t {
  a.value
}
