module Test.Parser.OperatorPrecedenceTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Nova.Compiler.CstPipeline as Pipeline
import Nova.Compiler.CodeGen as CodeGen
import Data.String as String

main :: Effect Unit
main = do
  log "\n=== Operator Precedence Tests ==="

  -- Test 1: && with /= should respect precedence
  testCodegen "Logical AND with not-equal"
    "module T where\ntest x = x == 1 && x /= 2"
    "(x == 1) and (x != 2)"

  -- Test 2: || with && should respect precedence
  testCodegen "Logical OR with AND"
    "module T where\ntest x = x == 1 || x == 2 && x == 3"
    "(x == 1) or ((x == 2) and (x == 3))"

  -- Test 3: Arithmetic precedence
  testCodegen "Arithmetic precedence"
    "module T where\ntest x = x + 1 * 2"
    "(x + (1 * 2))"

  -- Test 4: Multiple comparisons with &&
  testCodegen "Multiple comparisons"
    "module T where\ntest x = x > 0 && x < 10 && x /= 5"
    "((x > 0) and (x < 10)) and (x != 5)"

  log "\n=== Tests Complete ==="

testCodegen :: String -> String -> String -> Effect Unit
testCodegen name code expected = do
  case Pipeline.parseModuleCst code of
    Left err -> log $ "✗ " <> name <> ": Parse error - " <> err
    Right mod -> do
      let generated = CodeGen.genModule mod
      if String.contains (String.Pattern expected) generated
        then log $ "✓ " <> name
        else do
          log $ "✗ " <> name
          log $ "  Expected to contain: " <> expected
          log $ "  Generated: " <> generated
