-- Simple lexer for arithmetic expressions
-- https://nadia-polikarpova.github.io/cse130-web/lectures/06-parsing.html
{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  let                           { \s -> TokenLet          }
  in                            { \s -> TokenIn           }
  $digit+                       { \s -> TokenInt (read s) }
  \=                            { \s -> TokenEq           }
  \+                            { \s -> TokenPlus         }
  \-                            { \s -> TokenMinus        }
  \*                            { \s -> TokenTimes        }
  \/                            { \s -> TokenDiv          }
  \(                            { \s -> TokenLParen       }
  \)                            { \s -> TokenRParen       }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s        }

{
data Token 
  = TokenLet
  | TokenIn
  | TokenInt Int
  | TokenSym String
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenLParen
  | TokenRParen
  deriving (Eq,Show)

scanTokens = alexScanTokens
}