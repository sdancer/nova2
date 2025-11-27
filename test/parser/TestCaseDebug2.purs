module Test.Parser.TestCaseDebug2 where

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
  log "=== Case Debug Test 2 ==="
  
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
      log $ "First clause at column: " <> show firstTok.column
      
      -- Parse first clause
      case P.parseCaseClause clauseTokens of
        Right (Tuple clause rest) -> do
          log "First clause OK"
          
          -- Check what's next
          let rest' = P.skipNewlines rest
          case Array.head rest' of
            Just t -> do
              log $ "After first clause, next token: '" <> t.value <> "' at C" <> show t.column
              log $ "Same column as first? " <> show (t.column == firstTok.column)
              
              -- Try parsing second clause
              case P.parseCaseClause rest' of
                Right (Tuple clause2 rest2) -> do
                  log "Second clause OK"
                  let rest2' = P.skipNewlines rest2
                  log $ "After second clause: " <> show (Array.length rest2') <> " tokens"
                Left err -> log $ "Second clause failed: " <> err
            Nothing -> log "No more tokens after first clause"
        Left err -> log $ "First clause failed: " <> err
    Nothing -> log "No tokens"

