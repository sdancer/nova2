module Test.TypeCheck.ParseAnyDebug3 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Types (emptyEnv)
import Nova.Compiler.TypeChecker (checkModule)

main :: Effect Unit
main = do
  log "=== Isolating the case-in-case pattern ==="

  -- N1: M3 simplified - no recursion
  log "\n-- N1: No recursion --"
  testCode """
module Test where
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: Array (Int -> Either String Int) -> Int -> Either String Int
test fs n = go fs
  where
    go ps = case Array.head ps of
      Nothing -> Left "None"
      Just f -> case f n of
        Right r -> Right r
        Left _ -> Left "fail"
"""

  -- N2: Recursion but no inner case
  log "\n-- N2: Recursion, no inner case --"
  testCode """
module Test where
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: Array (Int -> Either String Int) -> Int -> Either String Int
test fs n = go fs
  where
    go ps = case Array.head ps of
      Nothing -> Left "None"
      Just f -> go (Array.drop 1 ps)
"""

  -- N3: Inner case but different pattern
  log "\n-- N3: Inner case on Maybe --"
  testCode """
module Test where
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: Array (Int -> Maybe Int) -> Int -> Either String Int
test fs n = go fs
  where
    go ps = case Array.head ps of
      Nothing -> Left "None"
      Just f -> case f n of
        Just r -> Right r
        Nothing -> go (Array.drop 1 ps)
"""

  -- N4: Like M3 but return type is Maybe not Either
  log "\n-- N4: Return Maybe (not Either) --"
  testCode """
module Test where
import Data.Array as Array
import Data.Maybe (Maybe(..))

test :: Array (Int -> Maybe Int) -> Int -> Maybe Int
test fs n = go fs
  where
    go ps = case Array.head ps of
      Nothing -> Nothing
      Just f -> case f n of
        Just r -> Just r
        Nothing -> go (Array.drop 1 ps)
"""

  -- N5: Like M3 exactly
  log "\n-- N5: M3 exactly --"
  testCode """
module Test where
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: Array (Int -> Either String Int) -> Int -> Either String Int
test fs n = go fs
  where
    go ps = case Array.head ps of
      Nothing -> Left "None"
      Just f -> case f n of
        Right r -> Right r
        Left _ -> go (Array.drop 1 ps)
"""

  -- N6: Without f as function from array
  log "\n-- N6: Without array of functions --"
  testCode """
module Test where
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Array Int -> Either String Int
test f ns = go ns
  where
    go ps = case Array.head ps of
      Nothing -> Left "None"
      Just n -> case f n of
        Right r -> Right r
        Left _ -> go (Array.drop 1 ps)
"""

  -- N7: With Maybe argument instead of function array
  log "\n-- N7: Simple Maybe recursion --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Int -> Either String Int
test f n = go (Just n)
  where
    go mx = case mx of
      Nothing -> Left "None"
      Just x -> case f x of
        Right r -> Right r
        Left _ -> go Nothing
"""

  -- N8: Is it the function application in case?
  log "\n-- N8: Function application in nested case --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Maybe Int -> Either String Int
test f mx = helper mx
  where
    helper m = case m of
      Nothing -> Left "None"
      Just n -> case f n of
        Right r -> Right r
        Left _ -> Left "inner fail"
"""

  -- N9: Recursion + function app in nested case
  log "\n-- N9: Recursion + function app --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Maybe Int -> Either String Int
test f mx = helper mx
  where
    helper m = case m of
      Nothing -> Left "None"
      Just n -> case f n of
        Right r -> Right r
        Left _ -> helper Nothing
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
