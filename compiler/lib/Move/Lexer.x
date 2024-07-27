-- Simple lexer for arithmetic expressions
-- https://nadia-polikarpova.github.io/cse130-web/lectures/06-parsing.html
{
module Move.Lexer (
  Token (..),
  scanTokens
  ) where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
$hex = [0-9a-fA-F]

tokens :-
  -- Ignored tokens
  $white+                       ; -- skip white space
  "//".*                        ; -- skip comments
  -- Special characters
  \(                            { \_ -> TokenLParen       }
  \)                            { \_ -> TokenRParen       }
  \{                            { \_ -> TokenLBrace       }
  \}                            { \_ -> TokenRBrace       }
  \,                            { \_ -> TokenComma        }
  \:                            { \_ -> TokenColon        }
  \;                            { \_ -> TokenSemiColon    }
  \:\:                          { \_ -> TokenDColon       }
  -- Top level
  address                       { \_ -> TokenAddress      }
  const                         { \_ -> TokenConst        }
  friend                        { \_ -> TokenFriend       }
  fun                           { \_ -> TokenFun          }
  script                        { \_ -> TokenScript       }
  module                        { \_ -> TokenModule       }
  use                           { \_ -> TokenUse          }
  -- Structs
  struct                        { \_ -> TokenStruct       }
  has                           { \_ -> TokenHas          }
  key                           { \_ -> TokenKey          }
  store                         { \_ -> TokenStore        }
  drop                          { \_ -> TokenDrop         }
  copy                          { \_ -> TokenCopy         }
  -- Control flow
  if                            { \_ -> TokenIf           }
  else                          { \_ -> TokenElse         }
  while                         { \_ -> TokenWhile        }
  loop                          { \_ -> TokenLoop         }
  break                         { \_ -> TokenBreak        }
  continue                      { \_ -> TokenContinue     }
  -- Let/bind
  let                           { \_ -> TokenLet          }
  in                            { \_ -> TokenIn           }
  \=                            { \_ -> TokenBind         }
  -- Identifiers
  $alpha($alpha | $digit)*      { \s -> TokenIdent s      }
  -- Numbers
  0x$hex+                       { \s -> TokenHex s        }
  $digit+                       { \s -> TokenDec (read s) }


{
data Token 
  = TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  | TokenSemiColon
  | TokenComma
  | TokenColon
  | TokenDColon
  | TokenAddress
  | TokenConst
  | TokenFriend
  | TokenFun
  | TokenScript
  | TokenModule
  | TokenUse
  | TokenStruct
  | TokenHas
  | TokenKey
  | TokenStore
  | TokenDrop
  | TokenCopy
  | TokenIf
  | TokenElse
  | TokenWhile
  | TokenLoop
  | TokenBreak
  | TokenContinue
  | TokenLet
  | TokenIn
  | TokenBind
  | TokenIdent String
  | TokenDec Int
  | TokenHex String
  deriving (Eq,Show)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}