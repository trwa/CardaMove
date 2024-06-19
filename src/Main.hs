{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Move.Address

type Parser = Parsec Void Text

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

-- | scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
pScheme :: Parser Scheme
pScheme =
  choice
    [ SchemeData <$ string "data",
      SchemeFile <$ string "file",
      SchemeFtp <$ string "ftp",
      SchemeHttp <$ string "http",
      SchemeHttps <$ string "https",
      SchemeIrc <$ string "irc",
      SchemeMailto <$ string "mailto"
    ]

data Uri = Uri
  { uriScheme :: Scheme,
    uriAuthority :: Maybe Authority
  }
  deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text), -- (user, password)
    authHost :: Text,
    authPort :: Maybe Int
  }
  deriving (Eq, Show)

pUri :: Parser Uri
pUri = do
  r <- pScheme
  _ <- char ':'
  return (Uri {uriScheme = r, uriAuthority = Nothing})

main :: IO ()
main = do
  parseTest pUri "mailto:"
