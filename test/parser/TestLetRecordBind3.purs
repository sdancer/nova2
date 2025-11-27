module Test.Parser.TestLetRecordBind3 where

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
  log "=== Let Record Bind Test 3 ==="
  
  -- Test the actual pattern
  let input1 = """foo tokens =
  let tokens' = skipNewlines tokens
      { init: before, rest: after } = Array.span bar tokens'
  in after"""
  testParse "Actual pattern" input1

testParse :: String -> String -> Effect Unit
testParse name input = do
  let tokens = tokenize input
  log $ "Tokens: " <> show (Array.length tokens)
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> name <> " - function: " <> fun.name
      log $ "  Rest: " <> show (Array.length (P.skipNewlines rest)) <> " tokens"
    Left err -> log $ "✗ " <> name <> ": " <> err

