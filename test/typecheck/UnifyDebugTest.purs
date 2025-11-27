module Test.TypeCheck.UnifyDebugTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast (Declaration(..), Expr(..))
import Nova.Compiler.Types (emptyEnv, Env, lookupEnv)
import Nova.Compiler.TypeChecker (checkModule, checkDecl, checkFunction, TCError, addFunctionPlaceholders, processNonFunctions, checkFunctionBodies)

main :: Effect Unit
main = do
  log "=== Unify.purs Debug Test ==="

  -- First test a simple wildcard case
  log "-- Testing wildcards --"
  testWildcard

  log ""

  content <- readTextFile UTF8 "src/Nova/Compiler/Unify.purs"
  let tokens = tokenize content
  case P.parseModule tokens of
    Left parseErr -> log $ "Parse error: " <> parseErr
    Right (Tuple m _) -> do
      log $ "Parsed " <> show (Array.length m.declarations) <> " declarations"

      -- List all function declarations
      log "\n-- Function declarations found: --"
      listFunctions m.declarations

      -- Try to check the first function
      log "\n-- Checking individual functions: --"
      checkFunctionsIndividually emptyEnv m.declarations

      -- Now try the module-level check step by step
      log "\n-- Module-level check step by step: --"
      let env1 = processNonFunctions emptyEnv m.declarations
      log $ "  Pass 1 (non-functions): OK"

      let env2 = addFunctionPlaceholders env1 m.declarations
      log $ "  Pass 2 (placeholders): OK"

      -- Check what's in the env now
      log $ "  Environment contains:"
      checkEnvContains env2 ["occurs", "bindVar", "unify", "unifyMany", "unifyRecords"]

      -- Try pass 3 - check each function with the enriched env
      log "\n-- Pass 3 (check function bodies with enriched env): --"
      checkFunctionsWithEnv env2 m.declarations

listFunctions :: Array Declaration -> Effect Unit
listFunctions decls = do
  let funcs = Array.mapMaybe getFunc decls
  void $ Array.foldM (\_ name -> log $ "  " <> name) unit funcs
  where
    getFunc (DeclFunction f) = Just f.name
    getFunc _ = Nothing

checkFunctionsIndividually :: Env -> Array Declaration -> Effect Unit
checkFunctionsIndividually env decls = do
  let funcs = Array.mapMaybe getFunc decls
  void $ Array.foldM (\_ f -> checkOneFunc f) unit funcs
  where
    getFunc (DeclFunction f) = Just f
    getFunc _ = Nothing

    checkOneFunc f = do
      case checkFunction env f of
        Left err -> log $ "  ✗ " <> f.name <> ": " <> show err
        Right _ -> log $ "  ✓ " <> f.name

checkEnvContains :: Env -> Array String -> Effect Unit
checkEnvContains env names = do
  void $ Array.foldM checkOne unit names
  where
    checkOne _ name = case lookupEnv env name of
      Just _ -> log $ "    ✓ " <> name
      Nothing -> log $ "    ✗ " <> name <> " NOT FOUND"

checkFunctionsWithEnv :: Env -> Array Declaration -> Effect Unit
checkFunctionsWithEnv env decls = do
  let funcs = Array.mapMaybe getFunc decls
  void $ Array.foldM (\_ f -> checkOneFunc f) unit funcs
  where
    getFunc (DeclFunction f) = Just f
    getFunc _ = Nothing

    checkOneFunc f = do
      case checkFunction env f of
        Left err -> log $ "  ✗ " <> f.name <> ": " <> show err
        Right _ -> log $ "  ✓ " <> f.name

-- Test wildcard handling
testWildcard :: Effect Unit
testWildcard = do
  -- Test 1: Single wildcard
  log "  Test 1 (single wildcard):"
  testCode """
module Test where
foo :: { x :: _ } -> Int
foo r = 1
"""

  -- Test 2: Two wildcards in same signature
  log "  Test 2 (two wildcards):"
  testCode """
module Test where
bar :: { x :: _ } -> { y :: _ } -> Int
bar a b = 1
"""

  -- Test 3: Like unifyRecords simple
  log "  Test 3 (like unifyRecords simple):"
  testCode """
module Test where
import Data.Map as Map
unifyRecs :: { fields :: Map.Map String Int, row :: _ }
          -> { fields :: Map.Map String Int, row :: _ }
          -> Int
unifyRecs r1 r2 = 1
"""

  -- Test 4: With field access
  log "  Test 4 (with field access):"
  testCode """
module Test where
import Data.Map as Map
unifyRecs :: { fields :: Map.Map String Int, row :: _ }
          -> { fields :: Map.Map String Int, row :: _ }
          -> Int
unifyRecs r1 r2 =
  let keys = Map.keys r1.fields
  in 1
"""

  -- Test 5a: Simple foldM
  log "  Test 5a (simple foldM):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Foldable (foldM)

test :: Either String Int
test = foldM (\\sub k -> Right sub) 0 [1, 2, 3]
"""

  -- Test 5b: foldM with where
  log "  Test 5b (foldM with where):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Foldable (foldM)

test :: Either String Int
test = foldM helper 0 [1, 2, 3]
  where
    helper sub k = Right sub
"""

  -- Test 5c: where clause with closure
  log "  Test 5c (where with closure):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Foldable (foldM)

test :: Int -> Either String Int
test x = foldM helper 0 [1, 2, 3]
  where
    helper sub k = Right (sub + x)
"""

  -- Test 5d: Like unifyRecords signature (without case)
  log "  Test 5d (like unifyRecords without case):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Foldable (foldM)

unifyRecords :: { fields :: Map.Map String Int, row :: _ }
             -> { fields :: Map.Map String Int, row :: _ }
             -> Either String Int
unifyRecords r1 r2 = foldM helper 0 (Map.keys r1.fields)
  where
    helper sub k =
      let v1 = Map.lookup k r1.fields
      in Right sub
"""

  -- Test 5e: With case expression
  log "  Test 5e (with case):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

test :: Int -> Either String Int
test x = case Just x of
  Just n -> Right n
  Nothing -> Right 0
"""

  -- Test 5f: With Tuple pattern
  log "  Test 5f (with Tuple pattern):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

test :: Int -> Int -> Either String Int
test x y = case Tuple (Just x) (Just y) of
  Tuple (Just a) (Just b) -> Right (a + b)
  _ -> Right 0
"""

  -- Test 5g: Simpler case in where with record access
  log "  Test 5g (case with record access in where):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))

unifyRecords :: { fields :: Map.Map String Int, row :: _ }
             -> Either String Int
unifyRecords r1 = foldM helper 0 (Map.keys r1.fields)
  where
    helper sub k = case Map.lookup k r1.fields of
      Just t1 -> Right t1
      Nothing -> Right sub
"""

  -- Test 5h: Same but without foldM
  log "  Test 5h (case with closure, no foldM):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: { fields :: Map.Map String Int, row :: _ } -> Either String Int
testFn r1 = helper 0 "x"
  where
    helper sub k = case Map.lookup k r1.fields of
      Just t1 -> Right t1
      Nothing -> Right sub
"""

  -- Test 5i: Simpler - without wildcard row
  log "  Test 5i (without wildcard row):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: { fields :: Map.Map String Int } -> Either String Int
testFn r1 = helper 0 "x"
  where
    helper sub k = case Map.lookup k r1.fields of
      Just t1 -> Right t1
      Nothing -> Right sub
"""

  -- Test 5j: Simpler - just record access in where
  log "  Test 5j (record access in where):"
  testCode """
module Test where

testFn :: { x :: Int } -> Int
testFn r = helper 0
  where
    helper n = r.x + n
"""

  -- Test 5k: With wildcard row
  log "  Test 5k (record with wildcard in where):"
  testCode """
module Test where

testFn :: { x :: Int, row :: _ } -> Int
testFn r = helper 0
  where
    helper n = r.x + n
"""

  -- Test 5l: Map.lookup without where
  log "  Test 5l (Map.lookup without where):"
  testCode """
module Test where
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: { fields :: Map.Map String Int } -> Int
testFn r1 = case Map.lookup "x" r1.fields of
  Just n -> n
  Nothing -> 0
"""

  -- Test 5m: Map.lookup in where but no closure
  log "  Test 5m (Map.lookup in where, no closure):"
  testCode """
module Test where
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Int
testFn m = helper "x"
  where
    helper k = case Map.lookup k m of
      Just n -> n
      Nothing -> 0
"""

  -- Test 5n: Record field in where with Map.lookup
  log "  Test 5n (record.field in where with Map.lookup):"
  testCode """
module Test where
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: { fields :: Map.Map String Int } -> Int
testFn r1 = helper "x"
  where
    helper k = case Map.lookup k r1.fields of
      Just n -> n
      Nothing -> 0
"""

  -- Test 5o: Two params in where helper
  log "  Test 5o (two params in where helper):"
  testCode """
module Test where
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: { fields :: Map.Map String Int } -> Int
testFn r1 = helper 0 "x"
  where
    helper sub k = case Map.lookup k r1.fields of
      Just n -> n
      Nothing -> sub
"""

  -- Test 5p: Two params but simpler body
  log "  Test 5p (two params, simple body):"
  testCode """
module Test where
import Data.Map as Map

testFn :: { fields :: Map.Map String Int } -> Int
testFn r1 = helper 0 "x"
  where
    helper sub k = sub
"""

  -- Test 5q: Two params with record access
  log "  Test 5q (two params, record access):"
  testCode """
module Test where

testFn :: { x :: Int } -> Int
testFn r1 = helper 0 "x"
  where
    helper sub k = r1.x + sub
"""

  -- Test 5r: With Either return
  log "  Test 5r (with Either return):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: { fields :: Map.Map String Int } -> Either String Int
testFn r1 = helper 0 "x"
  where
    helper sub k = case Map.lookup k r1.fields of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Test 5s: With Either AND wildcard row
  log "  Test 5s (Either + wildcard row):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: { fields :: Map.Map String Int, row :: _ } -> Either String Int
testFn r1 = helper 0 "x"
  where
    helper sub k = case Map.lookup k r1.fields of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Test 5t: Either without case
  log "  Test 5t (Either without case):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map

testFn :: { fields :: Map.Map String Int } -> Either String Int
testFn r1 = helper 0 "x"
  where
    helper sub k = Right sub
"""

  -- Test 5u: Either with simple closure
  log "  Test 5u (Either with simple closure):"
  testCode """
module Test where
import Data.Either (Either(..))

testFn :: { x :: Int } -> Either String Int
testFn r1 = helper 0
  where
    helper sub = Right (r1.x + sub)
"""

  -- Test 5v: Case + Right without record access
  log "  Test 5v (case+Right without record):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: Int -> Either String Int
testFn x = helper 0
  where
    helper sub = case Just x of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Test 5w: Case + Right WITH record access
  log "  Test 5w (case+Right WITH record access):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

testFn :: { x :: Int } -> Either String Int
testFn r1 = helper 0
  where
    helper sub = case Just r1.x of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Test 5x: Map.lookup + Right in where
  log "  Test 5x (Map.lookup+Right in where):"
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

  -- Test 5y: Same but with record.fields
  log "  Test 5y (Map.lookup on record.fields):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: { fields :: Map.Map String Int } -> Either String Int
testFn r = helper 0 "x"
  where
    helper sub k = case Map.lookup k r.fields of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Test 5z: Map.lookup + Right WITHOUT where
  log "  Test 5z (Map.lookup+Right, no where):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Int -> Either String Int
testFn m sub = case Map.lookup "x" m of
  Just n -> Right n
  Nothing -> Right sub
"""

  -- Test 6a: Map.lookup in where, returns Int (not Either)
  log "  Test 6a (Map.lookup in where, returns Int):"
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

  -- Test 6b: Just case on Maybe without Map.lookup
  log "  Test 6b (Just case without Map.lookup):"
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

  -- Test 6c: Same pattern with explicit type
  log "  Test 6c (same with explicit type annotation):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Either String Int
testFn m = helper 0 "x"
  where
    helper :: Int -> String -> Either String Int
    helper sub k = case Map.lookup k m of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Test 6d: Simplified - remove closure, pass m as arg
  log "  Test 6d (no closure, pass m):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Either String Int
testFn m = helper m 0 "x"
  where
    helper theMap sub k = case Map.lookup k theMap of
      Just n -> Right n
      Nothing -> Right sub
"""

  -- Test 6e: Using let instead of where
  log "  Test 6e (let instead of where):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

testFn :: Map.Map String Int -> Either String Int
testFn m =
  let helper sub k = case Map.lookup k m of
        Just n -> Right n
        Nothing -> Right sub
  in helper 0 "x"
"""

  -- Test 6f: Top level function (not nested)
  log "  Test 6f (top level, not nested):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))

helper :: Map.Map String Int -> Int -> String -> Either String Int
helper m sub k = case Map.lookup k m of
  Just n -> Right n
  Nothing -> Right sub

testFn :: Map.Map String Int -> Either String Int
testFn m = helper m 0 "x"
"""

  -- Test 6g: Without case, use maybe
  log "  Test 6g (using maybe instead of case):"
  testCode """
module Test where
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (maybe)

testFn :: Map.Map String Int -> Either String Int
testFn m = maybe (Right 0) Right (Map.lookup "x" m)
"""

testCode :: String -> Effect Unit
testCode src = do
  let tokens = tokenize src
  case P.parseModule tokens of
    Left err -> log $ "    Parse error: " <> err
    Right (Tuple m _) -> do
      case checkModule emptyEnv m.declarations of
        Left err -> log $ "    FAIL: " <> show err
        Right _ -> log $ "    OK"
