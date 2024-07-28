module Aiken.AST where

data TypeDefinition
  = TypeDefinition
  { typeDefinitionIdentifier :: String,
    typeDefinitionDataConstructors :: [DataConstructor]
  }
  deriving (Eq, Show)

data DataConstructor
  = DataConstructor
  { dataConstructorIdentifier :: String,
    dataConstructorFields :: [Field]
  }
  deriving (Eq, Show)

data Field
  = Field
  { fieldName :: String,
    fieldType :: String
  }
  deriving (Eq, Show)
