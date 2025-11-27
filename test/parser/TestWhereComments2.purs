module Test.Parser.TestWhereComments2 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Where Comments Test 2 ==="
  
  -- Test 1: No comments
  let input1 = """foo x = bar
  where
    bar = x + 1"""
  testParse "No comments" input1
  
  -- Test 2: Comment before where binding
  let input2 = """foo x = bar
  where
    -- comment
    bar = x + 1"""
  testParse "Comment before binding" input2
  
  -- Test 3: Comment inside case
  let input3 = """foo x = case y of
      Just z
        -- comment
        | test -> x
      _ -> y
  where
    bar = x"""
  testParse "Comment in case guard" input3

testParse :: String -> String -> Effect Unit
testParse name input = do
  let tokens = tokenize input
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> log $ "✓ " <> name
    Left err -> log $ "✗ " <> name <> ": " <> err

