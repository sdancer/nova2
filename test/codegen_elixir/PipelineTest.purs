module Test.CodeGenElixir.PipelineTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.String as String
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser (parseModule)
import Nova.Compiler.TypeChecker (checkModule, TCError)
import Nova.Compiler.Types (emptyEnv)
import Nova.Compiler.CodeGen (genModule)
import Nova.Compiler.Ast (Module)

foreign import runElixirCode :: String -> Effect String

main :: Effect Unit
main = do
  log "=== Full Pipeline Tests ==="
  log "Source -> Tokenize -> Parse -> TypeCheck -> CodeGen -> Elixir\n"

  -- Test 0: Simplest possible - just a literal
  testPipeline "just_literal" """
module Test.Literal where

main = 42
""" "Test.Literal.main()" "42"

  -- Test 1: Simple function
  testPipeline "simple_add" """
module Test.SimpleAdd where

add x y = x + y

main = add 2 3
""" "Test.SimpleAdd.main()" "5"

  -- Test 2: Factorial
  testPipeline "factorial" """
module Test.Fact where

fact n = if n == 0 then 1 else n * fact (n - 1)

main = fact 5
""" "Test.Fact.main()" "120"

  -- Test 3: List sum with case
  testPipeline "list_sum" """
module Test.ListSum where

sum xs = case xs of
  [] -> 0
  (h : t) -> h + sum t

main = sum [1, 2, 3, 4, 5]
""" "Test.ListSum.main()" "15"

  -- Test 4: Record operations
  testPipeline "record_ops" """
module Test.RecOps where

getX r = r.x

main = getX { x: 42, y: 0 }
""" "Test.RecOps.main()" "42"

  -- Test 5: Higher-order function
  testPipeline "higher_order" """
module Test.HigherOrder where

applyFn f x = f x

double x = x * 2

main = applyFn double 21
""" "Test.HigherOrder.main()" "42"

  -- Test 6: Lambda
  testPipeline "lambda_test" """
module Test.LambdaTest where

main = (\x -> x + 1) 41
""" "Test.LambdaTest.main()" "42"

  -- Test 7: Nested let
  testPipeline "nested_let" """
module Test.NestedLet where

main = let x = 10 in let y = 20 in let z = 12 in x + y + z
""" "Test.NestedLet.main()" "42"

  -- Test 8: Boolean logic
  testPipeline "bool_logic" """
module Test.BoolLogic where

main = if (5 > 3) && (2 < 4) then 1 else 0
""" "Test.BoolLogic.main()" "1"

  log "\n=== Pipeline Tests Complete ==="

testPipeline :: String -> String -> String -> String -> Effect Unit
testPipeline name source expr expected = do
  case compilePipeline source of
    Left err -> log $ "FAIL: " <> name <> " - " <> err <> "\n  source:\n" <> source
    Right elixirCode -> do
      let fullCode = elixirCode <> "\nIO.inspect(" <> expr <> ")"
      output <- runElixirCode fullCode
      let trimmed = String.trim output
      if trimmed == expected
        then log $ "PASS: " <> name
        else log $ "FAIL: " <> name <> "\n  expected: " <> expected <> "\n  got: " <> trimmed <> "\n  elixir:\n" <> elixirCode

compilePipeline :: String -> Either String String
compilePipeline source = do
  -- Tokenize
  let tokens = tokenize source
  -- Parse
  case parseModule tokens of
    Left parseErr -> Left $ "Parse error: " <> parseErr
    Right (Tuple mod _rest) -> do
      -- Type check
      case checkModule emptyEnv mod.declarations of
        Left tcErr -> Left $ "Type error: " <> show tcErr
        Right _env -> do
          -- Generate Elixir
          Right (genModule mod)
