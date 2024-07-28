module Translator (translate) where

import Aiken.AST qualified as Aiken
import Move.AST qualified as Move

translate :: Move.Module -> Aiken.Module
translate m =
  Aiken.Module
    { Aiken.moduleTopLevels = map translateTopLevel $ Move.moduleTopLevels m
    }

translateTopLevel :: Move.TopLevel -> Aiken.TopLevel
translateTopLevel (Move.TopLevelStruct s) = Aiken.TopLevelTypeDefinition td
  where
    td = translateStruct s

translateStruct :: Move.Struct -> Aiken.TypeDefinition
translateStruct s =
  Aiken.TypeDefinition
    { Aiken.typeDefinitionIdentifier = Move.structIdentifier s,
      Aiken.typeDefinitionDataConstructors =
        [ Aiken.DataConstructor
            { Aiken.dataConstructorIdentifier = Move.structIdentifier s,
              Aiken.dataConstructorFields = map translateField $ Move.structFields s
            }
        ]
    }

translateField :: Move.Field -> Aiken.Field
translateField f =
  Aiken.Field
    { Aiken.fieldIdentifier = Move.fieldIdentifier f,
      Aiken.fieldType = Move.fieldType f
    }