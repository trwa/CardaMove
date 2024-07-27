{
module Move.Parser (
  Module(..),
  Address(..),
  parseMove,
  parseError
  ) where
import Move.Lexer
}

%name parseMove
%tokentype { Token }
%error { parseError }

%token
  -- Separators
  '('       { TokenSeparatorLParen      }
  ')'       { TokenSeparatorRParen      }
  '{'       { TokenSeparatorLBrace      }
  '}'       { TokenSeparatorRBrace      }
  ','       { TokenSeparatorComma       }
  ':'       { TokenSeparatorColon       }
  ';'       { TokenSeparatorSemiColon   }
  '::'      { TokenSeparatorDColon      }
  -- Literals
  int       { TokenLiteralIntDec $$     }
  hex       { TokenLiteralIntHex $$     }
  string    { TokenLiteralString $$     }
  true      { TokenLiteralBool True     }
  false     { TokenLiteralBool False    }
  -- Identifiers
  symbol    { TokenIdentifier $$        }
  -- Keywords: Module, Script
  const     { TokenKeywordConst         }
  friend    { TokenKeywordFriend        }
  fun       { TokenKeywordFun           }
  module    { TokenKeywordModule        }
  script    { TokenKeywordScript        }
  use       { TokenKeywordUse           }
  -- Keywords: Structs
  struct    { TokenKeywordStruct        }
  has       { TokenKeywordHas           }
  key       { TokenKeywordKey           }
  store     { TokenKeywordStore         }
  drop      { TokenKeywordDrop          }
  copy      { TokenKeywordCopy          }
  -- Keywords: Control flow
  if        { TokenKeywordIf            }
  else      { TokenKeywordElse          }
  while     { TokenKeywordWhile         }
  loop      { TokenKeywordLoop          }
  break     { TokenKeywordBreak         }
  continue  { TokenKeywordContinue      }
  -- Keywords: Let binding
  let       { TokenKeywordLet           }
  in        { TokenKeywordIn            }
  -- Operators
  '+'       { TokenOperatorPlus         }
  '-'       { TokenOperatorMinus        }

%right in

%%

Module :: { Module }
  : module int '::' symbol '{' '}' { Module (AddressInt $2) $4 }
  | module hex '::' symbol '{' '}' { Module (AddressHex $2) $4 }
  | module symbol '::' symbol '{' '}' { Module (AddressSym $2) $4 }

{-}
Uses :: { [Use] }
  : Use { [$1] }
  | Uses Use { $2 : $1 }

Use :: { Use }
  : use symbol '::' symbol { Use (AddressNamed $2) (Identifier $4) }

Constants :: { [Constant] }
  : Constant { [$1] }
  | Constants Constant { $2 : $1 }

Constant :: { Constant }
  : const symbol ':' symbol '=' Expr { Constant (Identifier $2) (Type $4) $6 }

Expr :: { Expr }
  : symbol { Var (Identifier $1) }
  | let symbol '=' Expr in Expr { Let (Identifier $2) $4 $6 }

Stmts :: { [Stmt] }
  : Expr { [Stmt $1] }
  | Stmts ';' Expr { (Stmt $3) : $1 }

Function :: { Function }
  : fun symbol '(' Args ')' ':' symbol '{' Stmts '}' { Function (Identifier $2) $4 (Type $7) $9 }

Args :: { [(Identifier, Type)] }
  : {- empty -} { [] }
  | Arg { [$1] }
  | Args ',' Arg { $3 : $1 }

Arg :: { (Identifier, Type) }
  : symbol ':' symbol { (Identifier $1, Type $3) }
-}

{
data Module
  = Module Address String {-[Use] [Friend] [Struct] [Function] [Constant] Expr-}
  deriving (Eq, Show)

data Address 
  = AddressInt Int
  | AddressHex String
  | AddressSym String
  deriving (Eq, Show)

{-
newtype Identifier = Identifier String
  deriving (Eq, Show)

newtype Type = Type String
  deriving (Eq, Show)

data Use
  = Use Address Identifier
  deriving (Eq, Show)

data Constant
  = Constant Identifier Type Expr
  deriving (Eq, Show)

data Function
  = Function Identifier [(Identifier, Type)] Type [Stmt]
  deriving (Eq, Show)

data Expr
  = Var Identifier
  | Let Identifier Expr Expr
  deriving (Eq, Show)

newtype Stmt = Stmt Expr
  deriving (Eq, Show)

-- | friend <address>::<module>
data Friend
  = Friend String String
  deriving (Eq, Show)

-- | drop, copy, store, key
data Ability
  = Drop
  | Copy
  | Store
  | Key
  deriving (Eq, Show)

-- | struct <name> { <record: type>* } has <ability>
data Struct
  = Struct String [(String, String)] [Ability]
  deriving (Eq, Show)
-}

parseError :: [Token] -> a
parseError _ = error "Parse error"
}