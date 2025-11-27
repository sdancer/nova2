module Test.Parser.TestLetRecordBind2 where

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
  log "=== Let Record Bind Test 2 ==="
  
  -- Test 1: Simple let
  let input1 = """foo x =
  let y = x
  in y"""
  testParse "Simple let" input1
  
  -- Test 2: Let with multiple bindings
  let input2 = """foo x =
  let y = x
      z = y
  in z"""
  testParse "Multiple let bindings" input2
  
  -- Test 3: Let with record pattern
  let input3 = """foo x =
  let { a: b, c: d } = x
  in b"""
  testParse "Let with record pattern" input3

testParse :: String -> String -> Effect Unit
testParse name input = do
  let tokens = tokenize input
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> log $ "✓ " <> name
    Left err -> log $ "✗ " <> name <> ": " <> err

