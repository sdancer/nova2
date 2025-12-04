-- | Test parameterized type alias expansion in the type checker
-- | This is a feature test for the self-hosting requirement
module Test.TypeCheck.TypeAliasTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.TypeChecker (checkModule)
import Nova.Compiler.Types (emptyEnv)

main :: Effect Unit
main = do
  log "=== Parameterized Type Alias Tests ==="

  -- Test 1: Simple parameterized type alias
  log "\n-- Test 1: Simple Wrapped type alias --"
  testModule "wrapped_alias" """
module Test where

type Wrapped a = { open :: String, value :: a, close :: String }

unwrap :: Wrapped Int -> Int
unwrap w = w.value

test :: Int
test = unwrap { open: "(", value: 42, close: ")" }
"""

  -- Test 2: Nested parameterized type alias
  log "\n-- Test 2: Labeled type alias --"
  testModule "labeled_alias" """
module Test where

type Labeled a b = { label :: a, separator :: String, value :: b }

getLabel :: Labeled String Int -> String
getLabel l = l.label

test :: String
test = getLabel { label: "x", separator: "::", value: 1 }
"""

  -- Test 3: Type alias with function type parameter
  log "\n-- Test 3: Type alias in function signature --"
  testModule "alias_in_signature" """
module Test where

type Container a = { item :: a }

extract :: Container (Int -> Int) -> Int -> Int
extract c x = (c.item) x
"""

  -- Test 4: Nested type aliases
  log "\n-- Test 4: Nested type aliases --"
  testModule "nested_aliases" """
module Test where

type Wrapper a = { wrapped :: a }
type DoubleWrapper a = Wrapper (Wrapper a)

unwrapDouble :: DoubleWrapper Int -> Int
unwrapDouble d = d.wrapped.wrapped
"""

  log "\n=== Type Alias Tests Complete ==="

testModule :: String -> String -> Effect Unit
testModule name source =
  let tokens = tokenize source
  in case P.parseModule tokens of
    Left err -> log $ "PARSE FAIL: " <> name <> " - " <> err
    Right (Tuple mod _) ->
      case checkModule emptyEnv (Array.fromFoldable mod.declarations) of
        Left err -> log $ "TYPE FAIL: " <> name <> " - " <> show err
        Right _ -> log $ "PASS: " <> name
