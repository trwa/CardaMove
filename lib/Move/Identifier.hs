module Move.Identifier (Identifier) where

newtype Identifier = Identifier String
  deriving (Eq, Show)