module Move.Ast where

import Data.Char (chr)
import Data.Text (Text)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | And
  | Or
  | BitAnd
  | BitOr
  deriving (Show, Eq)

data UnOp
  = Neg
  | Not
  deriving (Show, Eq)

data Expr
  = LitAddr Int
  | LitBool Bool
  | LitChar Char
  | LitInt Int
  | LitStr Text
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  | Call Text [Expr]
  | Var Text
  | Access Expr Expr
  | -- | *pointer
    Deref Expr
  | -- | &expression
    Addr Expr
  | Assign Expr Expr
  | Sizeof Type
  | -- | used for dangling if's
    Noexpr
  deriving (Show, Eq)