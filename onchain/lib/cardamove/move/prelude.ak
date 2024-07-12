pub type Unit {
  Unit
}

// TODO: Make it parametric over the currency
pub type Coin {
  ada: Int,
}

pub fn zero() -> Coin {
  Coin { ada: 0 }
}

pub fn gt(lhs: Coin, rhs: Coin) -> Bool {
  lhs.ada > rhs.ada
}

pub fn ge(lhs: Coin, rhs: Coin) -> Bool {
  lhs.ada >= rhs.ada
}

pub fn lt(lhs: Coin, rhs: Coin) -> Bool {
  lhs.ada < rhs.ada
}

pub fn le(lhs: Coin, rhs: Coin) -> Bool {
  lhs.ada <= rhs.ada
}
