{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Move.Parser
import Move.Lexer
import Move.AST
import Move.Token

main :: IO ()
main = do
  let script = "script { fun foo ( x : u8 ) : u8 { let x = 6 in x } }"
  let tokens = scan script
  --let parsed = parseMove tokens
  print tokens