module Test.TypeCheck.OccursCheckDebug3 where

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
  log "=== Testing unused parameter hypothesis ==="

  -- Unused second param
  log "\n-- H1: Two params, k unused (FAILS?) --"
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

  -- Second param used (Right k instead of Right sub)
  log "\n-- H2: Two params, k used --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx = helper 0 "k"
  where
    helper sub k = case mx of
      Just n -> Right n
      Nothing -> Left k
"""

  -- Both params used
  log "\n-- H3: Two params, both used --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx = helper 0 "k"
  where
    helper sub k = case mx of
      Just n -> Right (n + sub)
      Nothing -> Left k
"""

  -- Use k in the Just branch too
  log "\n-- H4: k used in both branches with Right --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either Int Int
testFn mx = helper 0 1
  where
    helper sub k = case mx of
      Just n -> Right (n + k)
      Nothing -> Right (sub + k)
"""

  -- Let me test if the problem is with ExprLambda or with where binding
  log "\n-- I1: let binding, single param --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx =
  let helper sub = case mx of
        Just n -> Right n
        Nothing -> Right sub
  in helper 0
"""

  -- let binding, two params
  log "\n-- I2: let binding, two params --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx =
  let helper sub k = case mx of
        Just n -> Right n
        Nothing -> Right sub
  in helper 0 "k"
"""

  -- what if we don't use a let, just apply a lambda?
  log "\n-- I3: direct lambda application, two params --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx = (\\sub -> (\\k -> case mx of
    Just n -> Right n
    Nothing -> Right sub)) 0 "k"
"""

  -- try with explicit type annotation on let binding
  log "\n-- I4: let binding, two params, type annotation --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx =
  let helper :: Int -> String -> Either String Int
      helper sub k = case mx of
        Just n -> Right n
        Nothing -> Right sub
  in helper 0 "k"
"""

  -- What about partially applied?
  log "\n-- I5: partially applied, step by step --"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Maybe Int -> Either String Int
testFn mx =
  let helper1 sub =
        let helper2 k = case mx of
              Just n -> Right n
              Nothing -> Right sub
        in helper2
  in helper1 0 "k"
"""

  -- Minimal test with Right not in case
  log "\n-- J1: Two lambdas, Right at inner level --"
  testCode """
module Test where
import Data.Either (Either(..))

testFn :: Either String Int
testFn = helper 0 "k"
  where
    helper sub k = Right sub
"""

  -- With if instead of case
  log "\n-- J2: if instead of case, with Right --"
  testCode """
module Test where
import Data.Either (Either(..))

testFn :: Bool -> Either String Int
testFn b = helper 0 "k"
  where
    helper sub k = if b then Right 1 else Right sub
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
