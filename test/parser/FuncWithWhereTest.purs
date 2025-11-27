module Test.Parser.FuncWithWhereTest where

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
  log "=== Function with Where Test ==="

  test "simple" "tTuple ts = foo"
  test "with string concat" "tTuple ts = name <> show ts"
  test "with where" """tTuple ts = TyCon { name: "Tuple" <> show (length ts), args: ts }
  where
    length arr = 0"""

  test "parens in concat" """f x = TyCon { name: "A" <> show (g x) }"""

  log "=== Done ==="

test :: String -> String -> Effect Unit
test name input = do
  log $ name <> ": " <> show input
  let tokens = tokenize input
  log $ "Tokens: " <> show (map _.value (Array.take 15 (Array.filter (\t -> t.tokenType /= TokNewline) tokens)))
  case P.parseFunctionDeclaration tokens of
    Right (Tuple _ rest) -> do
      let restVals = map _.value (Array.filter (\t -> t.tokenType /= TokNewline) rest)
      log $ "  ✓ Parsed, remaining: " <> show (Array.length restVals)
    Left err -> do
      log $ "  ✗ Error: " <> err
  log ""
