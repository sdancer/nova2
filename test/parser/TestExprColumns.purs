module Test.Parser.TestExprColumns where

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
  log "=== Column Test ==="
  
  -- Simple case - { at column 1
  let input1 = """x
{ b: c }"""
  let tokens1 = tokenize input1
  log "Input 1 (no leading spaces):"
  logTokens tokens1
  log ""
  case P.parseExpression tokens1 of
    Right (Tuple _ rest) -> log $ "Rest: " <> show (Array.length rest) <> " tokens"
    Left err -> log $ "Failed: " <> err
  
  log ""
  log "---"
  log ""
  
  -- Let case - { at column 7 (indented)
  let input2 = """x
      { b: c }"""
  let tokens2 = tokenize input2
  log "Input 2 (with leading spaces):"
  logTokens tokens2
  log ""
  case P.parseExpression tokens2 of
    Right (Tuple _ rest) -> log $ "Rest: " <> show (Array.length rest) <> " tokens"
    Left err -> log $ "Failed: " <> err

logTokens :: Array Token -> Effect Unit
logTokens tokens = do
  let fmt t = t.value <> " L" <> show t.line <> "C" <> show t.column
  void $ Array.foldl (\acc t -> acc *> log ("  " <> fmt t)) (pure unit) tokens

