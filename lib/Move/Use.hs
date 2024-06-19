module Move.Use (Use) where

import Move.Address (Address)
import Move.Identifier (Identifier)

data Use = Use Address Identifier
  deriving (Eq, Show)