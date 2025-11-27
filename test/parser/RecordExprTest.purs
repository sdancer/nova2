module Test.Parser.RecordExprTest where

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
  log "=== Record Expression Test ==="

  testExpr "simple" "{ x: 1 }"
  testExpr "string val" """{ name: "hello" }"""
  testExpr "two fields" """{ name: "hello", value: 42 }"""
  testExpr "string concat" """{ name: "A" <> "B" }"""
  testExpr "app with record" """TyCon { name: "hello" }"""
  testExpr "complex" """TyCon { name: "Tuple" <> show x, args: ts }"""

  log "=== Done ==="

testExpr :: String -> String -> Effect Unit
testExpr name input = do
  log $ name <> ": " <> input
  let tokens = tokenize input
  log $ "Tokens: " <> show (map _.value tokens)
  case P.parseExpression tokens of
    Right (Tuple _ rest) -> do
      let restVals = map _.value (Array.filter (\t -> t.tokenType /= TokNewline) rest)
      log $ "  ✓ Parsed, remaining: " <> show restVals
    Left err -> do
      log $ "  ✗ Error: " <> err
  log ""
