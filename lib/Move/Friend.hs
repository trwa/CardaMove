module Move.Friend (Friend) where

import Move.Address (Address)
import Move.Identifier (Identifier)

data Friend = Friend Address Identifier
  deriving (Eq, Show)
