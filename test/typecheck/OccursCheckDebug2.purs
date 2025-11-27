module Test.TypeCheck.OccursCheckDebug2 where

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
  log "=== Narrowing Occurs Check Bug ==="

  -- Single param with Right works
  log "\n-- A1: Single param, Right --"
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

  -- Two params without case or Right works
  log "\n-- A2: Two params, simple body --"
  testCode """
module Test where
import Data.Either (Either(..))

testFn :: Int -> Either String Int
testFn x = helper 0 "k"
  where
    helper sub k = Right sub
"""

  -- Two params, case without Right
  log "\n-- A3: Two params, case no Right --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Int
testFn mx = helper 0 "k"
  where
    helper sub k = case mx of
      Just n -> n
      Nothing -> sub
"""

  -- Two params, no case, just Right
  log "\n-- A4: Two params, no case, just Right --"
  testCode """
module Test where
import Data.Either (Either(..))

testFn :: Int -> Either String Int
testFn x = helper 0 "k"
  where
    helper sub k = Right sub
"""

  -- Two params, case + Right (MINIMAL FAILURE)
  log "\n-- A5: Two params, case + Right (FAILS) --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx = helper 0 "k"
  where
    helper sub k = case mx of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Does the second param type matter?
  log "\n-- B1: Two Int params, case + Right --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx = helper 0 1
  where
    helper sub k = case mx of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- What about Bool?
  log "\n-- B2: Int + Bool params --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx = helper 0 true
  where
    helper sub b = case mx of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Let's check if using Left instead of Right matters
  log "\n-- C1: Two params, case + Left --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx = helper 0 "k"
  where
    helper sub k = case mx of
      Just n -> Left k
      Nothing -> Left k
"""

  -- Both branches use Left
  log "\n-- C2: Return Left k with types --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx = helper 0 "k"
  where
    helper sub k = Left k
"""

  -- What if we use the second param in Right?
  log "\n-- D1: Two params, Right uses second --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe String -> Either String String
testFn mx = helper "a" "k"
  where
    helper sub k = case mx of
      Just n -> Right n
      Nothing -> Right k
"""

  -- Three params
  log "\n-- E1: Three params, case + Right --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx = helper 0 "k" true
  where
    helper sub k b = case mx of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- What about using a different wrapper (Just instead of Right)?
  log "\n-- F1: Two params, case + Just --"
  testCode """
module Test where
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Maybe Int
testFn mx = helper 0 "k"
  where
    helper sub k = case mx of
      Just n -> Just n
      Nothing -> Just sub
"""

  -- What if we use pure/identity on the result?
  log "\n-- G1: Single param, case + Right, no closure --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Either String Int
testFn = helper 0
  where
    helper sub = case Just 1 of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Two params, no closure
  log "\n-- G2: Two params, case + Right, no closure --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Either String Int
testFn = helper 0 "k"
  where
    helper sub k = case Just 1 of
      Just n -> Right n
      Nothing -> Right sub
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
