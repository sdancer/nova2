module Test.TypeCheck.ParseAnyDebug where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Types (emptyEnv)
import Nova.Compiler.TypeChecker (checkModule)

main :: Effect Unit
main = do
  log "=== parseAny Isolation Test ==="

  -- Test 1: Just parseAny alone
  log "\n-- Test 1: parseAny alone --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Array as Array

type ParseResult a = Either String (Tuple a (Array Int))

parseAny :: forall a. Array (Array Int -> ParseResult a) -> Array Int -> ParseResult a
parseAny parsers tokens = go parsers
  where
    go ps = case Array.head ps of
      Nothing -> Left "No parser succeeded"
      Just p -> case p tokens of
        Right result -> Right result
        Left _ -> go (Array.drop 1 ps)
"""

  -- Test 2: parseAny + success
  log "\n-- Test 2: parseAny + success --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Array as Array
import Data.Tuple (Tuple(..))

type ParseResult a = Either String (Tuple a (Array Int))

success :: forall a. a -> Array Int -> ParseResult a
success v rest = Right (Tuple v rest)

parseAny :: forall a. Array (Array Int -> ParseResult a) -> Array Int -> ParseResult a
parseAny parsers tokens = go parsers
  where
    go ps = case Array.head ps of
      Nothing -> Left "No parser succeeded"
      Just p -> case p tokens of
        Right result -> Right result
        Left _ -> go (Array.drop 1 ps)
"""

  -- Test 3: Minimal higher-order case
  log "\n-- Test 3: Minimal higher-order --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Array as Array

test :: forall a. Array (Int -> Either String a) -> Int -> Either String a
test parsers n = go parsers
  where
    go ps = case Array.head ps of
      Nothing -> Left "None"
      Just p -> case p n of
        Right r -> Right r
        Left _ -> go (Array.drop 1 ps)
"""

  -- Test 4: Without forall
  log "\n-- Test 4: Without forall --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Array as Array

test :: Array (Int -> Either String Int) -> Int -> Either String Int
test parsers n = go parsers
  where
    go ps = case Array.head ps of
      Nothing -> Left "None"
      Just p -> case p n of
        Right r -> Right r
        Left _ -> go (Array.drop 1 ps)
"""

  -- Test 5: With explicit type signature on where function
  log "\n-- Test 5: With explicit type sig on helper --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Array as Array

test :: forall a. Array (Int -> Either String a) -> Int -> Either String a
test parsers n = go parsers
  where
    go :: Array (Int -> Either String a) -> Either String a
    go ps = case Array.head ps of
      Nothing -> Left "None"
      Just p -> case p n of
        Right r -> Right r
        Left _ -> go (Array.drop 1 ps)
"""

  log "\n=== Done ==="

testCode :: String -> Effect Unit
testCode src = do
  let tokens = tokenize src
  case P.parseModule tokens of
    Left err -> log $ "  Parse error: " <> err
    Right (Tuple m _) -> do
      case checkModule emptyEnv m.declarations of
        Left err -> log $ "  FAIL: " <> show err
        Right _ -> log $ "  OK"
