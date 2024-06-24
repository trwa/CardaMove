{
module Parser (
  Exp(..),
  parseCalc,
  parseError
  ) where
import Lexer
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
  '('   { TokenLParen }
  ')'   { TokenRParen }
  '{'   { TokenLBrace }
  '}'   { TokenRBrace }
  ';'   { TokenSemiColon }
  ':'   { TokenColon }
  '::'  { TokenDColon }
  ','   { TokenComma }
  if    { TokenIf }
  else  { TokenElse }
  let   { TokenLet }
  in    { TokenIn }
  int   { TokenInt $$ }
  var   { TokenSym $$ }
  '='   { TokenEq }
  '+'   { TokenPlus }
  '-'   { TokenMinus }
  '*'   { TokenTimes }
  '/'   { TokenDiv }

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

Exp : let var '=' Exp in Exp { Let $2 $4 $6 }
    | Exp '+' Exp            { Plus $1 $3 }
    | Exp '-' Exp            { Minus $1 $3 }
    | Exp '*' Exp            { Times $1 $3 }
    | Exp '/' Exp            { Div $1 $3 }
    | '(' Exp ')'            { $2 }
    | '-' Exp %prec NEG      { Negate $2 }
    | int                    { Int $1 }
    | var                    { Var $1 }

Smt : Exp ';' Smt            { $1 }
    | Exp                    { $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp 
  = Let String Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Negate Exp
  | Brack Exp
  | Int Int
  | Var String
  deriving Show
}