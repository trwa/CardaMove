{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Move.Parser
import Move.Lexer

main :: IO ()
main = do
  let script = "script { fun foo ( x : u8 ) : u8 { let x = 6 in x } }"
  let tokens = scanTokens script
  --let parsed = parseMove tokens
  print tokens