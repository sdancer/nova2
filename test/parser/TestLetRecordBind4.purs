module Test.Parser.TestLetRecordBind4 where

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
  log "=== Let Mixed Bindings Test ==="
  
  -- Test: Simple then record
  let input1 = """foo x =
  let a = x
      { b: c } = x
  in c"""
  testParse "Simple then record" input1
  
  -- Test: Record then simple
  let input2 = """foo x =
  let { b: c } = x
      a = x
  in a"""
  testParse "Record then simple" input2

testParse :: String -> String -> Effect Unit
testParse name input = do
  let tokens = tokenize input
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> log $ "✓ " <> name
    Left err -> log $ "✗ " <> name <> ": " <> err

