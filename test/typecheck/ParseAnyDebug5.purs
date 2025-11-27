module Test.TypeCheck.ParseAnyDebug5 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Types (emptyEnv)
import Nova.Compiler.TypeChecker (checkModule)

main :: Effect Unit
main = do
  log "=== P4 variations ==="

  -- Q1: P4 exactly
  log "\n-- Q1: P4 exactly --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Maybe Int -> Either String Int
test f mx = case mx of
  Nothing -> Left "None"
  Just n -> case f n of
    Right r -> Right r
    Left _ -> Left "fail"
"""

  -- Q2: Without outer case
  log "\n-- Q2: Without outer case --"
  testCode """
module Test where
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Int -> Either String Int
test f n = case f n of
  Right r -> Right r
  Left _ -> Left "fail"
"""

  -- Q3: Without inner case destructure
  log "\n-- Q3: Without inner case destructure --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Maybe Int -> Either String Int
test f mx = case mx of
  Nothing -> Left "None"
  Just n -> f n
"""

  -- Q4: With simpler function type
  log "\n-- Q4: Simpler function type --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))

test :: (Int -> Maybe Int) -> Maybe Int -> Maybe Int
test f mx = case mx of
  Nothing -> Nothing
  Just n -> case f n of
    Just r -> Just r
    Nothing -> Nothing
"""

  -- Q5: Function returns Int, not wrapped
  log "\n-- Q5: Function returns Int --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: (Int -> Int) -> Maybe Int -> Either String Int
test f mx = case mx of
  Nothing -> Left "None"
  Just n -> Right (f n)
"""

  -- Q6: No function arg at all
  log "\n-- Q6: No function arg --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: Maybe (Either String Int) -> Either String Int
test mx = case mx of
  Nothing -> Left "None"
  Just ei -> case ei of
    Right r -> Right r
    Left _ -> Left "fail"
"""

  -- Q7: Function returns same type as final
  log "\n-- Q7: Function returns same type --"
  testCode """
module Test where
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Either String Int -> Either String Int
test f ei = case ei of
  Left e -> Left e
  Right n -> case f n of
    Right r -> Right r
    Left _ -> Left "fail"
"""

  -- Q8: Q7 but return directly
  log "\n-- Q8: Q7 return f n directly --"
  testCode """
module Test where
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Either String Int -> Either String Int
test f ei = case ei of
  Left e -> Left e
  Right n -> f n
"""

  -- Q9: Using either instead of case
  log "\n-- Q9: Using either combinator --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Maybe Int -> Either String Int
test f mx = case mx of
  Nothing -> Left "None"
  Just n -> either (\\_ -> Left "fail") Right (f n)
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
