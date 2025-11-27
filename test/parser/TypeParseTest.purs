module Test.Parser.TypeParseTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Type Parse Test ==="

  -- This is what happens in derive instance: after `derive instance eqType ::` we parse type
  test "Eq Type"

  -- This is the problematic scenario - more tokens after
  test "Eq Type\n\ntInt :: Type"

  -- Show token details
  log "Token details for problematic case:"
  let tokens = tokenize "Eq Type\n\ntInt :: Type"
  log $ show (map (\t -> { val: t.value, col: t.column, line: t.line }) tokens)

  log "=== Done ==="

test :: String -> Effect Unit
test input = do
  log $ "Input: " <> show input
  let tokens = tokenize input
  log $ "Tokens: " <> show (map _.value (Array.filter (\t -> t.tokenType /= TokNewline) tokens))
  case P.parseType tokens of
    Right (Tuple ty rest) -> do
      let restVals = map _.value (Array.filter (\t -> t.tokenType /= TokNewline) rest)
      log $ "  ✓ Parsed, remaining: " <> show restVals
    Left err -> do
      log $ "  ✗ Error: " <> err
  log ""
