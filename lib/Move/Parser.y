{
module Move.Parser (
  Expr(..),
  Module(..),
  Term(..),
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
  ident     { TokenIdent $$   }

%right in

%%

term :: { Term }
  : mod                          { TermMod $1   }
  | expr                         { TermExpr $1  }
  | stmts                        { TermStmts $1 }


expr :: { Expr }
  : let ident '=' expr in expr  { Let $2 $4 $6  }
  | ident                       { Var $1        }

stmts :: { [Expr] }
  : stmts ';' expr               { $3 : $1   }

mod :: { Module }
  : mod ident '{' stmts '}'      { Module $2 $4  }

{
data Term 
  = TermMod Module
  | TermExpr Expr
  | TermStmts [Expr]
  deriving (Eq, Show)

data Expr
  = Let String Expr Expr
  | Var String
  deriving (Eq, Show)

data Module
  = Module String [Expr]
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"
}