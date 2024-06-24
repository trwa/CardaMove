{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import Move.Lexer (scanTokens)
import Move.Parser (Exp (..), parseMove)
import Test.Hspec
  ( Spec,
    anyException,
    describe,
    hspec,
    it,
    shouldBe,
    shouldNotBe,
    shouldThrow,
  )
import Test.QuickCheck (Testable (property))
import Test.QuickCheck.Test (test)

testParseLetIn :: Spec
testParseLetIn = describe "Parse let in expression" $ do
  it "parses a let in expression" $
    parseMove (scanTokens "let x = s in x") `shouldBe` Let "x" (Var "s") (Var "x")

main :: IO ()
main = hspec $ do
  testParseLetIn