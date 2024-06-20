module Move.Module
  ( Module (..),
  )
where

import Move.Term (Address, Identifier)

-- | Friend declaration.
data Friend = Friend Address Identifier
  deriving (Eq, Show)

-- | Use declaration.
data Use = Use Address Identifier
  deriving (Eq, Show)

{-
https://move-language.github.io/move/modules-and-scripts.html
module <address>::<identifier> {
    (<use> | <friend> | <type> | <function> | <constant>)*
}
-}
data Module = Module
  { moduleAddress :: Address,
    moduleIdentifier :: Identifier,
    moduleUses :: [Use],
    moduleFriends :: [Friend]
    -- moduleTypes :: [Type],
    -- moduleFunctions :: [Function],
    -- moduleConstants :: [Constant]
  }
  deriving (Eq, Show)
