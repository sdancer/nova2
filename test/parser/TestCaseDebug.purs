module Test.Parser.TestCaseDebug where

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
  log "=== Case Debug Test ==="
  
  let input = """case x of
      Nothing -> a
      Just t
        -- comment
        | guard -> b"""
  let tokens = tokenize input
  
  -- Skip to "of" and get first clause tokens
  let afterOf = Array.dropWhile (\t -> t.value /= "of") tokens
  let clauseTokens = Array.drop 1 afterOf
  let clauseTokens' = P.skipNewlines clauseTokens
  
  log "Tokens after 'of' (skipping newlines):"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> t.value)) (pure unit) (Array.take 10 clauseTokens')
  
  case Array.head clauseTokens' of
    Just firstTok -> do
      log $ ""
      log $ "Expected indent for clauses: " <> show firstTok.column
      
      -- Parse first clause
      log ""
      log "parseCaseClause (first):"
      case P.parseCaseClause clauseTokens' of
        Right (Tuple clause rest) -> do
          log "✓ first clause parsed"
          let rest' = P.skipNewlines rest
          log $ "Rest after first clause:"
          void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> t.value)) (pure unit) (Array.take 5 rest')
          case Array.head rest' of
            Just t -> log $ "Next token column: " <> show t.column
            Nothing -> log "No more tokens"
        Left err -> log $ "✗ " <> err
    Nothing -> log "No tokens after 'of'"

