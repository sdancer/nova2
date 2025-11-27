module Test.TypeCheck.OccursCheckDebug where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast (Declaration(..), Expr(..))
import Nova.Compiler.Types (emptyEnv, Env, lookupEnv, Type(..), Scheme, applySubst, freeTypeVars, mkScheme)
import Nova.Compiler.TypeChecker (checkModule, checkDecl, checkFunction, TCError, infer)

main :: Effect Unit
main = do
  log "=== Occurs Check Debugging ==="

  -- The minimal failing case
  log "\n-- Minimal failing case --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Either String Int
testFn m = helper 0 "x"
  where
    helper sub k = case Map.lookup k m of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Let's break it down step by step

  -- Step 1: Does `Right` work in where?
  log "\n-- Step 1: Right in where --"
  testCode """
module Test where
import Data.Either (Either(..))

testFn :: Int -> Either String Int
testFn x = helper x
  where
    helper n = Right n
"""

  -- Step 2: Does case with Right work in where?
  log "\n-- Step 2: case + Right in where --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx = helper 0
  where
    helper sub = case mx of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Step 3: Does Map.lookup work in where without Right?
  log "\n-- Step 3: Map.lookup in where, no Right --"
  testCode """
module Test where
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Int
testFn m = helper 0 "x"
  where
    helper sub k = case Map.lookup k m of
      Just n -> n
      Nothing -> sub
"""

  -- Step 4: Two params + case + Right (no Map.lookup)
  log "\n-- Step 4: Two params, case Maybe, Right --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx = helper 0 "x"
  where
    helper sub k = case mx of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Step 5: What if we use Just instead of Map.lookup?
  log "\n-- Step 5: Just x instead of Map.lookup --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Int -> Either String Int
testFn x = helper 0 "k"
  where
    helper sub k = case Just x of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Step 6: Using String as key type
  log "\n-- Step 6: Map.lookup with String key --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Either String Int
testFn m = helper 0 "x"
  where
    helper sub k = Right sub
"""

  -- Step 7: Case on Map.lookup, return type is Int
  log "\n-- Step 7: Map.lookup, return Int --"
  testCode """
module Test where
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Int
testFn m = helper 0 "x"
  where
    helper sub k = case Map.lookup k m of
      Just n -> n
      Nothing -> sub
"""

  -- Step 8: Case on Map.lookup, wrap in Just (not Right)
  log "\n-- Step 8: Map.lookup, return Maybe --"
  testCode """
module Test where
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Maybe Int
testFn m = helper 0 "x"
  where
    helper sub k = case Map.lookup k m of
      Just n -> Just n
      Nothing -> Just sub
"""

  -- Step 9: Direct - no where clause
  log "\n-- Step 9: Direct, no where --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Int -> String -> Either String Int
testFn m sub k = case Map.lookup k m of
  Just n -> Right n
  Nothing -> Right sub
"""

  -- Step 10: Inline lambda
  log "\n-- Step 10: Inline lambda --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Either String Int
testFn m = (\\sub k -> case Map.lookup k m of
    Just n -> Right n
    Nothing -> Right sub) 0 "x"
"""

  -- Step 11: Is it the two-argument pattern?
  log "\n-- Step 11: Single arg helper --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Either String Int
testFn m = helper "x"
  where
    helper k = case Map.lookup k m of
      Just n -> Right n
      Nothing -> Right 0
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
