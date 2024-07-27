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
  -- Separators
  \(                            { \_ -> TokenSeparatorLParen              }
  \)                            { \_ -> TokenSeparatorRParen              }
  \{                            { \_ -> TokenSeparatorLBrace              }
  \}                            { \_ -> TokenSeparatorRBrace              }
  \,                            { \_ -> TokenSeparatorComma               }
  \:                            { \_ -> TokenSeparatorColon               }
  \;                            { \_ -> TokenSeparatorSemiColon           }
  \:\:                          { \_ -> TokenSeparatorDColon              }
  -- Literals
  $digit+                       { \s -> TokenLiteralIntDec (read s)       }
  0x$hex+                       { \s -> TokenLiteralIntHex s              }
  \"($digit|$alpha)*\"          { \s -> TokenLiteralString s              }
  true                          { \_ -> TokenLiteralBool True             } 
  false                         { \_ -> TokenLiteralBool False            }
  -- Identifiers
  $alpha($alpha | $digit)*      { \s -> TokenIdentifier s                 }
  -- Keywords: Module, Script
  const                         { \_ -> TokenKeywordConst                 }
  friend                        { \_ -> TokenKeywordFriend                }
  fun                           { \_ -> TokenKeywordFun                   }
  module                        { \_ -> TokenKeywordModule                }
  script                        { \_ -> TokenKeywordScript                }
  use                           { \_ -> TokenKeywordUse                   }
  -- Keywords: Structs
  struct                        { \_ -> TokenKeywordStruct                }
  has                           { \_ -> TokenKeywordHas                   }
  key                           { \_ -> TokenKeywordKey                   }
  store                         { \_ -> TokenKeywordStore                 }
  drop                          { \_ -> TokenKeywordDrop                  }
  copy                          { \_ -> TokenKeywordCopy                  }
  -- Keywords: Control flow
  if                            { \_ -> TokenKeywordIf                    }
  else                          { \_ -> TokenKeywordElse                  }
  while                         { \_ -> TokenKeywordWhile                 }
  loop                          { \_ -> TokenKeywordLoop                  }
  break                         { \_ -> TokenKeywordBreak                 }
  continue                      { \_ -> TokenKeywordContinue              }
  -- Keywords: Let binding
  let                           { \_ -> TokenKeywordLet                   }
  in                            { \_ -> TokenKeywordIn                    }
  -- Operators
  \+                            { \_ -> TokenOperatorPlus                 }
  \-                            { \_ -> TokenOperatorMinus                }
  \*                            { \_ -> TokenOperatorTimes                }
  \/                            { \_ -> TokenOperatorDiv                  }
  \%                            { \_ -> TokenOperatorMod                  }
  \=\=                          { \_ -> TokenOperatorEq                   }
  \!\=                          { \_ -> TokenOperatorNeq                  }
  \<                            { \_ -> TokenOperatorLt                   }
  \<\=                          { \_ -> TokenOperatorLeq                  }
  \>                            { \_ -> TokenOperatorGt                   }
  \>\=                          { \_ -> TokenOperatorGeq                  }
  \&\&                          { \_ -> TokenOperatorAnd                  }
  \|\|                          { \_ -> TokenOperatorOr                   }
  \!                            { \_ -> TokenOperatorNot                  }
  \=                            { \_ -> TokenOperatorAssign               }
  \&                            { \_ -> TokenOperatorRef                  }
  \.                            { \_ -> TokenOperatorDot                  }
  \@                            { \_ -> TokenOperatorAt                   }

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
  -- Keywords: Module, Script
  | TokenKeywordConst
  | TokenKeywordFriend
  | TokenKeywordFun
  | TokenKeywordModule
  | TokenKeywordScript
  | TokenKeywordUse
  -- Keywords: Structs
  | TokenKeywordStruct
  | TokenKeywordHas
  | TokenKeywordKey
  | TokenKeywordStore
  | TokenKeywordDrop
  | TokenKeywordCopy
  -- Keywords: Control flow
  | TokenKeywordIf
  | TokenKeywordElse
  | TokenKeywordWhile
  | TokenKeywordLoop
  | TokenKeywordBreak
  | TokenKeywordContinue
  -- Keywords: Let binding
  | TokenKeywordLet
  | TokenKeywordIn
  -- Operators
  | TokenOperatorPlus
  | TokenOperatorMinus
  | TokenOperatorTimes
  | TokenOperatorDiv
  | TokenOperatorMod
  | TokenOperatorEq
  | TokenOperatorNeq
  | TokenOperatorLt
  | TokenOperatorLeq
  | TokenOperatorGt
  | TokenOperatorGeq
  | TokenOperatorAnd
  | TokenOperatorOr
  | TokenOperatorNot
  | TokenOperatorAssign
  | TokenOperatorRef
  | TokenOperatorDot
  | TokenOperatorAt
  -- Identifiers
  | TokenIdentifier String
  deriving (Eq,Show)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}