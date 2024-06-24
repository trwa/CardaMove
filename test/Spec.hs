{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import Lexer (scanTokens)
import Move.Term (LiteralU8 (..), parse)
import Parser (Exp (..), parseCalc)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Test (test)
import Text.Megaparsec (runParser)

testPreludeHead :: Spec
testPreludeHead = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [23 ..] `shouldBe` (23 :: Int)

  it "returns the first element of an *arbitrary* list" $
    property $
      \x xs -> head (x : xs) == (x :: Int)

  it "throws an exception if used with an empty list" $ do
    evaluate (head []) `shouldThrow` anyException

testParseLiteralU8 :: Spec
testParseLiteralU8 = describe "Parse LiteralU8" $ do
  it "parses a decimal number" $
    runParser parse "" "42" `shouldBe` Right (LiteralU8 42)

  it "parses a hexadecimal number" $
    runParser parse "" "0x2a" `shouldBe` Right (LiteralU8 42)

  it "fails to parse a hexadecimal number" $
    runParser parse "" "0x2aq" `shouldNotBe` Right (LiteralU8 42)

testParseLetIn :: Spec
testParseLetIn = describe "Parse let in expression" $ do
  it "parses a let in expression" $
    parseCalc (scanTokens "let x = 42 in x") `shouldBe` Let "x" (Int 42) (Var "x")

main :: IO ()
main = hspec $ do
  testParseLetIn