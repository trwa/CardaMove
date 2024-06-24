-- Simple lexer for arithmetic expressions
-- https://nadia-polikarpova.github.io/cse130-web/lectures/06-parsing.html
{
module Lexer (
  Token (..),
  scanTokens
  ) where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
  -- Ignored tokens
  $white+                       ; -- skip white space
  "//".*                        ; -- skip comments
  -- Special characters
  \(                            { \_ -> TokenLParen       }
  \)                            { \_ -> TokenRParen       }
  \{                            { \_ -> TokenLBrace       }
  \}                            { \_ -> TokenRBrace       }
  \;                            { \_ -> TokenSemiColon    }
  \,                            { \_ -> TokenComma        }
  \:                            { \_ -> TokenColon        }
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
  -- Operators
  let                           { \_ -> TokenLet          }
  in                            { \_ -> TokenIn           }
  $digit+                       { \s -> TokenInt (read s) }
  \=                            { \_ -> TokenEq           }
  \+                            { \_ -> TokenPlus         }
  \-                            { \_ -> TokenMinus        }
  \*                            { \_ -> TokenTimes        }
  \/                            { \_ -> TokenDiv          }
  
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s        }

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
  | TokenInt Int
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenSym String

  deriving (Eq,Show)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}