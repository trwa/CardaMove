module Move.Address (Address (..)) where

import Data.WideWord (Word128)

data Address = Named String | Literal Word128
  deriving (Eq, Show)