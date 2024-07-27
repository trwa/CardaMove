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
  \(                            { \_ -> TokenSeparatorLParen        }
  \)                            { \_ -> TokenSeparatorRParen        }
  \{                            { \_ -> TokenSeparatorLBrace        }
  \}                            { \_ -> TokenSeparatorRBrace        }
  \,                            { \_ -> TokenSeparatorComma         }
  \:                            { \_ -> TokenSeparatorColon         }
  \;                            { \_ -> TokenSeparatorSemiColon     }
  \:\:                          { \_ -> TokenSeparatorDColon        }
  -- Literals
  $digit+                       { \s -> TokenLiteralIntDec (read s) }
  0x$hex+                       { \s -> TokenLiteralIntHex s        }
  \"($digit|$alpha)*\"          { \s -> TokenLiteralString s        }
  true                          { \_ -> TokenLiteralBool True       }
  false                         { \_ -> TokenLiteralBool False      }
  @0x\$$hex+                     { \s -> TokenLiteralAddrExprHex s  }
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
  0x$hex+                       { \s -> TokenLiteralIntHex s        }
  $digit+                       { \s -> TokenLiteralIntDec (read s)      }


{
data Token 
  -- Separators
  = TokenSeparatorLParen
  | TokenSeparatorRParen
  | TokenSeparatorLBrace
  | TokenSeparatorRBrace
  | TokenSeparatorSemiColon
  | TokenSeparatorComma
  | TokenSeparatorColon
  | TokenSeparatorDColon
  -- Literals
  | TokenLiteralIntDec Int
  | TokenLiteralIntHex String
  | TokenLiteralString String
  | TokenLiteralBool Bool
  | TokenLiteralAddrExprInt Int
  | TokenLiteralAddrExprHex String
  | TokenLiteralAddrInt Int
  | TokenLiteralAddrHex String
  -- Keywords
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
  deriving (Eq,Show)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}