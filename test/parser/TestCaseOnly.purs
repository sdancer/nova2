module Test.Parser.TestCaseOnly where

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
  log "=== Case Only Test ==="
  
  -- Just the case expression
  let input = """case Array.head toks of
      Nothing -> Tuple acc toks
      Just t
        -- comment
        | guard1 -> body1
        -- comment  
        | guard2 -> body2"""
  let tokens = tokenize input
  
  log "Tokens:"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> t.value)) (pure unit) tokens
  
  log ""
  log "parseExpression:"
  case P.parseExpression tokens of
    Right (Tuple expr rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
      when (Array.length rest' > 0) $ do
        log "First rest tokens:"
        void $ Array.foldl (\acc t -> acc *> log ("  " <> t.value)) (pure unit) (Array.take 5 rest')
    Left err -> log $ "✗ " <> err

