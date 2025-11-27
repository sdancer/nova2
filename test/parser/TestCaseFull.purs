module Test.Parser.TestCaseFull where

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
  log "=== Case Full Test ==="
  
  let input = """case x of
      Nothing -> a
      Just t
        -- comment
        | guard -> b"""
  let tokens = tokenize input
  
  log "parseCaseExpression:"
  case P.parseCaseExpression tokens of
    Right (Tuple expr rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
      when (Array.length rest' > 0) $ do
        log "First rest tokens:"
        void $ Array.foldl (\acc t -> acc *> log ("  " <> t.value)) (pure unit) (Array.take 5 rest')
    Left err -> log $ "✗ " <> err

