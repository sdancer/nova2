module Test.Parser.DeriveTest where

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
  log "=== Derive Instance Tests ==="

  -- The problematic case
  test "derive instance eqType :: Eq Type"

  -- Simple instance
  test "instance showInt :: Show Int where\n  show x = x"

  log "=== Done ==="

test :: String -> Effect Unit
test input = do
  log $ "Input: " <> input
  let tokens = tokenize input
  log $ "First tokens: " <> show (map _.value (Array.take 3 tokens))
  case P.parseTypeClassInstance tokens of
    Right (Tuple _ rest) -> do
      log "  ✓ Parsed OK"
    Left err -> do
      log $ "  ✗ Error: " <> err
  log ""
