module Test.Parser.FuncTest where

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
  log "=== Function Body Tests ==="

  -- The problematic case from Types.purs
  testFunc "mkTVar id name = { id, name }"

  -- Simple record return
  testFunc "foo = { x: 1 }"

  -- Record with shorthand
  testFunc "bar x y = { x, y }"

  log "=== Done ==="

testFunc :: String -> Effect Unit
testFunc input = do
  log $ "Input: " <> input
  let tokens = tokenize input
  let tokVals = map _.value $ Array.filter (\t -> t.tokenType /= TokNewline) tokens
  log $ "Tokens: " <> show tokVals
  case P.parseFunctionDeclaration tokens of
    Right (Tuple decl rest) -> do
      log "  ✓ Parsed OK"
    Left err -> do
      log $ "  ✗ Error: " <> err
  log ""
