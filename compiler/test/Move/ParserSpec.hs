{-# LANGUAGE OverloadedStrings #-}

module Move.ParserSpec (spec) where

import Move.AST
import Move.Lexer
import Move.Parser
import Test.Hspec

testScan :: String -> Module -> SpecWith ()
testScan str ast = it str $ parseMove (scanTokens str) `shouldBe` ast

testParseEmptyModule :: Spec
testParseEmptyModule = describe "Parse an empty module" $ do
  testScan "module foo::bar {}" $
    Module
      { moduleAddress = "foo",
        moduleIdentifier = "bar",
        moduleTopLevels = []
      }

testParseModuleOneEmptyStruct :: Spec
testParseModuleOneEmptyStruct = describe "Parse module with one empty struct" $ do
  testScan "module foo::baz { struct A {} } " $
    Module
      { moduleAddress= "foo",
        moduleIdentifier = "baz",
        moduleTopLevels =
          [ TopLevelStruct $ Struct
              { structIdentifier = "A",
                structFields = [],
                structAbilities = []
              }
          ]
      }

spec :: Spec
spec = do
  testParseEmptyModule
  testParseModuleOneEmptyStruct
