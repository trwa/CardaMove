{-# LANGUAGE OverloadedStrings #-}

module Move.ParserSpec (spec) where

import Move.Lexer
import Move.Parser
import Test.Hspec

testScan :: String -> Module -> SpecWith ()
testScan str ast = it str $ parseMove (scanTokens str) `shouldBe` ast

testParseModule :: Spec
testParseModule = describe "Parse module" $ do
  testScan "module 12 :: culo {}" $  Module (AddressInt 12) "culo"

{-
testParseLetIn :: Spec
testParseLetIn = describe "Parse let in expression" $ do
  testScan "module { let x = s in x }" $ Module "_" "_" (Let (Identifier "x") (Var (Identifier "s")) (Var (Identifier "x")))
-}

{-
testParseFakeModule :: Spec
testParseFakeModule = describe "Parse fake module" $ do
  it "parses a fake module" $
    parseMove (scanTokens "module M { let x = s in x }") `shouldBe` TermMod (Module "M" [Let "x" (Var "s") (Var "x")])
-}

spec :: Spec
spec = do
  testParseModule