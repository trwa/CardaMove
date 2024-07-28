module Aiken.Writer where

import Aiken.AST

class Write a where
  write :: a -> String

instance Write TypeDefinition where
  write :: TypeDefinition -> String
  write (TypeDefinition identifier dataConstructors) =
    "type " ++ identifier ++ " { " ++ concatMap write dataConstructors ++ "}"

instance Write DataConstructor where
  write :: DataConstructor -> String
  write (DataConstructor identifier fields) =
    identifier ++ " { " ++ concatMap write fields ++ " },"

instance Write Field where
  write :: Field -> String
  write (Field name typ) = name ++ ": " ++ typ ++ ", "
