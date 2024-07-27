module Move.LexerSpec (spec) where

import Move.Lexer
import Test.Hspec

testScan :: String -> [Token] -> SpecWith ()
testScan str toks = it str $ scanTokens str `shouldBe` toks

testScanBraces :: Spec
testScanBraces = describe "Parse {}" $ do
  testScan "{}" [TokenLBrace, TokenRBrace]
  testScan "{ }" [TokenLBrace, TokenRBrace]
  testScan "{ } " [TokenLBrace, TokenRBrace]
  testScan " { }" [TokenLBrace, TokenRBrace]

testScanModule :: Spec
testScanModule = describe "Parse module" $ do
  testScan "module" [TokenModule]
  testScan "module " [TokenModule]
  testScan "module {" [TokenModule, TokenLBrace]
  testScan "module { }" [TokenModule, TokenLBrace, TokenRBrace]
  testScan "module abc::def {}" [TokenModule, TokenIdent "abc", TokenDColon, TokenIdent "def", TokenLBrace, TokenRBrace]
  testScan "module 0xABCD::culo {}" [TokenModule, TokenHex "0xABCD", TokenDColon, TokenIdent "culo", TokenLBrace, TokenRBrace]

testScanDecimal :: Spec
testScanDecimal = describe "Parse decimals" $ do
  testScan "0" [TokenDec 0]
  testScan "1" [TokenDec 1]
  testScan "123" [TokenDec 123]

testScanHex :: Spec
testScanHex = describe "Parse hexadecimals" $ do
  testScan "0x0" [TokenHex "0x0"]
  testScan "0x1" [TokenHex "0x1"]
  testScan "0x123" [TokenHex "0x123"]
  testScan "0xFF2E" [TokenHex "0xFF2E"]

testScanString :: Spec
testScanString = describe "Parse strings" $ do
  testScan "\"\"" [TokenString "\"\""]
  testScan "\"hello\"" [TokenString "\"hello\""]

spec :: Spec
spec = do
  testScanBraces
  testScanModule
  testScanDecimal
  testScanHex
  testScanString
