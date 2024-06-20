{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Move.Term (LiteralBool, LiteralU8, LiteralU256, Parser, parse)
import Text.Megaparsec (parseTest)

main :: IO ()
main = do
  parseTest (parse :: Parser LiteralU8) "0x01"
