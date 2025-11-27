module Test.Parser.TestCaseGuardComment where

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
  log "=== Case Guard Comment Test ==="
  
  -- Test: Comment before guard
  let input = """case y of
  Just z
    -- comment
    | test -> x
  _ -> y"""
  let tokens = tokenize input
  log $ "Tokens:"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> show t.value)) (pure unit) tokens
  
  log ""
  log "parseExpression:"
  case P.parseExpression tokens of
    Right (Tuple expr rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
    Left err -> log $ "✗ " <> err

