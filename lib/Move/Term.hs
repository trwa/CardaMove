{-# LANGUAGE OverloadedStrings #-}

module Move.Term
  ( LiteralU8 (..),
    LiteralU16,
    LiteralU32,
    LiteralU64,
    LiteralU128,
    LiteralU256,
    LiteralBool,
    parse,
    Parser,
    Address,
    Identifier,
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Data.WideWord (Word128, Word256)
import Data.Word (Word16, Word32, Word64, Word8)
import Text.Megaparsec (Parsec, choice, (<|>))
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

newtype LiteralU8 = LiteralU8 Word8
  deriving (Eq, Show)

newtype LiteralU16 = LiteralU16 Word16
  deriving (Eq, Show)

newtype LiteralU32 = LiteralU32 Word32
  deriving (Eq, Show)

newtype LiteralU64 = LiteralU64 Word64
  deriving (Eq, Show)

newtype LiteralU128 = LiteralU128 Word128
  deriving (Eq, Show)

newtype LiteralU256 = LiteralU256 Word256
  deriving (Eq, Show)

newtype LiteralBool = LiteralBool Bool
  deriving (Eq, Show)

data Address = AddressIdentifier Identifier | AddressLiteral LiteralU128
  deriving (Eq, Show)

-- | Identifier in the Move language.
newtype Identifier = Identifier String
  deriving (Eq, Show)

type Parser = Parsec Void Text

class Parse a where
  parse :: Parser a

parseNum :: (Num a) => Parser a
parseNum = (C.char '0' >> C.char' 'x' >> L.hexadecimal) <|> L.decimal

instance Parse LiteralU8 where
  parse = LiteralU8 <$> parseNum

instance Parse LiteralU16 where
  parse = LiteralU16 <$> parseNum

instance Parse LiteralU32 where
  parse = LiteralU32 <$> L.decimal

instance Parse LiteralU64 where
  parse = LiteralU64 <$> (C.char '0' >> C.char' 'x' >> L.hexadecimal)

instance Parse LiteralU128 where
  parse = LiteralU128 <$> (C.char '0' >> C.char' 'x' >> L.hexadecimal)

instance Parse LiteralU256 where
  parse = LiteralU256 <$> (C.char '0' >> C.char' 'x' >> L.hexadecimal)

instance Parse LiteralBool where
  parse = LiteralBool <$> choice [True <$ C.string "true", False <$ C.string "false"]
