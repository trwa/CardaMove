{
module Parser (
  Exp(..),
  parseMove,
  parseError
  ) where
import Lexer
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

Exp : let ident '=' Exp in Exp { Let $2 $4 $6 }
    | ident                    { Var $1       }

Smt : Exp ';' Smt            { $1 }
    | Exp                    { $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp 
  = Let String Exp Exp
  | Var String
  deriving (Eq, Show)
}