module Test.Parser.TestJustClause where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Just Clause Test ==="
  
  -- Just the Just clause with guards
  let input = """Just t
        -- comment
        | guard -> b"""
  let tokens = tokenize input
  
  log "Tokens:"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> t.value)) (pure unit) tokens
  
  log ""
  log "parseCaseClause:"
  case P.parseCaseClause tokens of
    Right (Tuple clause rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
    Left err -> log $ "✗ " <> err

