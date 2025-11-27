module Test.Parser.TestCaseClausesAt where

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
  log "=== Case Clauses At Test ==="
  
  let input = """case x of
      Nothing -> a
      Just t
        -- comment
        | guard -> b"""
  let tokens = tokenize input
  
  -- Skip to after "of"
  let afterOf = Array.dropWhile (\t -> t.value /= "of") tokens
  let clauseTokens = P.skipNewlines (Array.drop 1 afterOf)
  
  case Array.head clauseTokens of
    Just firstTok -> do
      log $ "Starting parseCaseClausesAt with indent = " <> show firstTok.column
      
      case P.parseCaseClausesAt clauseTokens firstTok.column [] of
        Right (Tuple clauses rest) -> do
          log $ "✓ Parsed " <> show (Array.length clauses) <> " clauses"
          let rest' = P.skipNewlines rest
          log $ "Rest: " <> show (Array.length rest') <> " tokens"
          when (Array.length rest' > 0) $ do
            log "First rest tokens:"
            void $ Array.foldl (\acc t -> acc *> log ("  " <> t.value)) (pure unit) (Array.take 5 rest')
        Left err -> log $ "✗ " <> err
    Nothing -> log "No tokens"

