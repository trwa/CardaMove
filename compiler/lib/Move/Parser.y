{
module Move.Parser (
  Contract(..),
  Constant(..),
  Use(..),
  parseMove,
  parseError
  ) where
import Move.Lexer
}

%name parseMove
%tokentype { Token }
%error { parseError }

%token
  -- Special characters
  '('       { TokenLParen     }
  ')'       { TokenRParen     }
  '{'       { TokenLBrace     }
  '}'       { TokenRBrace     }
  ','       { TokenComma      }
  ':'       { TokenColon      }
  ';'       { TokenSemiColon  }
  '::'      { TokenDColon     }
  -- Top level
  address   { TokenAddress    }
  const     { TokenConst      }
  friend    { TokenFriend     }
  fun       { TokenFun        }
  script    { TokenScript     }
  module    { TokenModule     }
  use       { TokenUse        }
  -- Structs
  struct    { TokenStruct     }
  has       { TokenHas        }
  key       { TokenKey        }
  store     { TokenStore      }
  drop      { TokenDrop       }
  copy      { TokenCopy       }
  -- Control flow
  if        { TokenIf         }
  else      { TokenElse       }
  while     { TokenWhile      }
  loop      { TokenLoop       }
  break     { TokenBreak      }
  continue  { TokenContinue   }
  -- Let/bind
  let       { TokenLet        }
  in        { TokenIn         }
  '='       { TokenBind       }
  -- Identifiers
  symbol    { TokenIdent $$   }

%right in

%%

Contract :: { Contract }
  : script '{' Uses Constants '}' { Script $3 $4 }

Uses :: { [Use] }
  : Use { [$1] }
  | Uses Use { $2 : $1 }

Use :: { Use }
  : use symbol '::' symbol { Use (Address $2) (Identifier $4) }

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

{
newtype Address = Address String
  deriving (Eq, Show)

newtype Identifier = Identifier String
  deriving (Eq, Show)

newtype Type = Type String
  deriving (Eq, Show)

data Contract
  = Script [Use] [Constant]
  -- | Module String String [Use] [Friend] [Struct] [Function] [Constant]
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


parseError :: [Token] -> a
parseError _ = error "Parse error"
}