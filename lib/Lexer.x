{
module Lexer
  ( Alex,
    AlexPosn (..), 
    alexGetInput,
    alexError,
    runAlex,
    alexMonadScan,
    
    Range (..),
    RangedToken (..),
    Token (..)
  ) where
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = [0-9]

tokens :-

<0> $white+ ;

{
data AlexUserState = AlexUserState {}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn,
    stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token,
    rtRange :: Range
  } deriving (Eq, Show)

data Token = EOF
  deriving (Eq, Show)
}