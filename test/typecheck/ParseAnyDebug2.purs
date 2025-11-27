module Test.TypeCheck.ParseAnyDebug2 where

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
  log "=== Simplifying parseAny issue ==="

  -- Minimal test: recursive function with higher-order function in case
  log "\n-- M1: Simplest recursion --"
  testCode """
module Test where
import Data.Array as Array
import Data.Maybe (Maybe(..))

test :: Array Int -> Int
test xs = go xs
  where
    go ps = case Array.head ps of
      Nothing -> 0
      Just n -> n + go (Array.drop 1 ps)
"""

  -- M2: With function argument
  log "\n-- M2: Function argument --"
  testCode """
module Test where
import Data.Array as Array
import Data.Maybe (Maybe(..))

test :: Array (Int -> Int) -> Int -> Int
test fs n = go fs
  where
    go ps = case Array.head ps of
      Nothing -> 0
      Just f -> f n + go (Array.drop 1 ps)
"""

  -- M3: With Either
  log "\n-- M3: With Either --"
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

  -- M4: Case in case (but simpler)
  log "\n-- M4: Nested case simpler --"
  testCode """
module Test where
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: Array Int -> Either String Int
test xs = go xs
  where
    go ps = case Array.head ps of
      Nothing -> Left "None"
      Just n -> case n > 0 of
        true -> Right n
        false -> go (Array.drop 1 ps)
"""

  -- M5: Maybe in recursive where
  log "\n-- M5: Maybe pattern in where --"
  testCode """
module Test where
import Data.Array as Array
import Data.Maybe (Maybe(..))

test :: Array Int -> Maybe Int
test xs = go xs
  where
    go ps = case Array.head ps of
      Nothing -> Nothing
      Just n -> if n > 0 then Just n else go (Array.drop 1 ps)
"""

  -- M6: Either result from nested case
  log "\n-- M6: Either from nested case --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: Maybe Int -> Either String Int
test mx = case mx of
  Nothing -> Left "None"
  Just n -> Right n
"""

  -- M7: Where with nested case returning Either
  log "\n-- M7: Where with nested case Either --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: Maybe Int -> Either String Int
test mx = helper mx
  where
    helper m = case m of
      Nothing -> Left "None"
      Just n -> Right n
"""

  -- M8: Where recursive with nested case Either
  log "\n-- M8: Recursive where with nested case Either --"
  testCode """
module Test where
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: Array Int -> Either String Int
test xs = go xs
  where
    go ps = case Array.head ps of
      Nothing -> Left "None"
      Just n -> Right n
"""

  -- M9: Adding recursion to M8
  log "\n-- M9: M8 + recursion --"
  testCode """
module Test where
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: Array Int -> Either String Int
test xs = go xs
  where
    go ps = case Array.head ps of
      Nothing -> Left "None"
      Just n -> if n > 0 then Right n else go (Array.drop 1 ps)
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
