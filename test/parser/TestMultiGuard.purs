module Test.Parser.TestMultiGuard where

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
  log "=== Multi Guard Test ==="
  
  -- Just t with multiple guards
  let input = """Just t
        -- comment1
        | guard1 -> body1
        -- comment2
        | guard2 -> body2
        -- comment3
        | guard3 -> body3"""
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
      when (Array.length rest' > 0) $ do
        log "First rest tokens:"
        void $ Array.foldl (\acc t -> acc *> log ("  " <> t.value)) (pure unit) (Array.take 5 rest')
    Left err -> log $ "✗ " <> err

