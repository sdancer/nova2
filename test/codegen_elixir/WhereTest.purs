module Test.CodeGenElixir.WhereTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser (parseModule)
import Nova.Compiler.CodeGen (genModule)

main :: Effect Unit
main = do
  log "=== Where Clause Code Generation Tests ==="

  -- Test 1: Simple where clause with helper function
  testCodeGen "simple_where" """
module Test where

factorial n = go n 1
  where
    go 0 acc = acc
    go n acc = go (n - 1) (n * acc)
"""

  -- Test 2: Where clause with multiple helpers
  testCodeGen "multiple_helpers" """
module Test where

processValue x = helper1 (helper2 x)
  where
    helper1 y = y + 1
    helper2 z = z * 2
"""

  -- Test 3: Recursive helper in where
  testCodeGen "recursive_where" """
module Test where

sumList xs = go xs 0
  where
    go [] acc = acc
    go (h : t) acc = go t (acc + h)
"""

  -- Test 4: Where with pattern matching
  testCodeGen "where_pattern" """
module Test where

getFirst xs = helper xs
  where
    helper [] = 0
    helper (x : _) = x
"""

  log "\n=== Where Clause Tests Complete ==="

testCodeGen :: String -> String -> Effect Unit
testCodeGen name source = do
  log $ "\n--- " <> name <> " ---"
  let tokens = tokenize source
  case parseModule tokens of
    Left err -> log $ "Parse error: " <> err
    Right (Tuple mod _) -> do
      log "Parsed successfully"
      let elixir = genModule mod
      log "Generated Elixir:"
      log elixir
      -- Check for obvious issues
      if String.contains (String.Pattern "undefined") elixir
        then log "WARNING: Contains 'undefined'"
        else pure unit
      if String.contains (String.Pattern ".()") elixir
        then log "WARNING: Contains problematic '.()', indicates local function not inlined"
        else pure unit
