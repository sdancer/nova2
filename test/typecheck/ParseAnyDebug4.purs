module Test.TypeCheck.ParseAnyDebug4 where

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
  log "=== Super minimal case ==="

  -- P1: Absolute minimum - no where
  log "\n-- P1: No where clause --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Int -> Either String Int
test f n = case f n of
  Right r -> Right r
  Left _ -> Left "fail"
"""

  -- P2: With where, no nested case
  log "\n-- P2: With where, no nested case --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Int -> Either String Int
test f n = helper
  where
    helper = case f n of
      Right r -> Right r
      Left _ -> Left "fail"
"""

  -- P3: With where, one param
  log "\n-- P3: With where, one param --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Int -> Either String Int
test f n = helper n
  where
    helper x = case f x of
      Right r -> Right r
      Left _ -> Left "fail"
"""

  -- P4: Outer case without where
  log "\n-- P4: Outer case without where --"
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

  -- P5: Outer case WITH where
  log "\n-- P5: Outer case WITH where --"
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
        Left _ -> Left "fail"
"""

  -- P6: Simpler - no f arg, literal Either
  log "\n-- P6: No function arg, literal Either --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: Maybe Int -> Either String Int
test mx = helper mx
  where
    helper m = case m of
      Nothing -> Left "None"
      Just n -> case Right n of
        Right r -> Right r
        Left _ -> Left "fail"
"""

  -- P7: Same but with Just in inner
  log "\n-- P7: Just instead of Right --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))

test :: Maybe Int -> Maybe Int
test mx = helper mx
  where
    helper m = case m of
      Nothing -> Nothing
      Just n -> case Just n of
        Just r -> Just r
        Nothing -> Nothing
"""

  -- P8: Pure function app in inner case
  log "\n-- P8: identity function in inner case --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: Maybe Int -> Either String Int
test mx = helper mx
  where
    helper m = case m of
      Nothing -> Left "None"
      Just n -> case identity (Right n) of
        Right r -> Right r
        Left _ -> Left "fail"
"""

  -- P9: Without f closure
  log "\n-- P9: No closure over f --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

test :: (Int -> Either String Int) -> Maybe Int -> Either String Int
test f mx = helper f mx
  where
    helper g m = case m of
      Nothing -> Left "None"
      Just n -> case g n of
        Right r -> Right r
        Left _ -> Left "fail"
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
