module Move.Module (Module (..)) where

import Move.Address (Address)
-- import Constant (Constant)
import Move.Friend (Friend)
-- import Function (Function)
import Move.Identifier (Identifier)
-- import Type (Type)
import Move.Use (Use)

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
