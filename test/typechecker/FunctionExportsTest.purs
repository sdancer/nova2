module Test.TypeChecker.FunctionExportsTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Map as Map
import Nova.Compiler.CstPipeline as CstPipeline
import Nova.Compiler.TypeChecker as TC
import Nova.Compiler.Types as Types

-- | Test that all forms of function declarations are exported to env.bindings
main :: Effect Unit
main = do
  log "=== Function Exports Tests ==="
  log "Testing that all function forms end up in env.bindings"
  log ""

  -- Test 1: Simple single-clause function
  testFuncExported "Single-clause function" singleClauseModule ["simpleFunc"]

  -- Test 2: Multi-clause function (pattern matching)
  testFuncExported "Multi-clause function" multiClauseModule ["multiClause"]

  -- Test 3: Function with guards
  testFuncExported "Function with guards" guardedModule ["guardedFunc"]

  -- Test 4: Function with where clause
  testFuncExported "Function with where clause" whereClauseModule ["withWhere"]

  -- Test 5: Function with type signature
  testFuncExported "Function with type signature" typeSigModule ["typedFunc"]

  -- Test 6: Multiple functions mixed
  testFuncExported "Multiple mixed functions" mixedModule
    ["simple", "multiMatch", "guarded", "withHelper"]

  -- Test 7: Unify-like module (complex structure)
  testFuncExported "Unify-like module" unifyLikeModule
    [ "showType", "occurs", "bindVar", "isRecordTypeAlias"
    , "unifyWithAliases", "unify", "unifyMany", "unifyRecords"
    ]

  -- Test 8: Test with dependency concatenation (simulating regenerate.js behavior)
  testFuncExportedWithDeps "With deps (simulating regenerate)" unifyLikeModule
    ["showType"]
    [typeSigModule]  -- simulate having another module with declarations

  log ""
  log "=== Function Exports Tests Complete ==="

-- | Test that specific functions are exported to env.bindings
testFuncExported :: String -> String -> Array String -> Effect Unit
testFuncExported name source funcNames = do
  log $ "Test: " <> name
  case checkAndGetBindings source of
    Left err -> log $ "  FAIL: " <> err
    Right env -> do
      let results = map (\fn -> { name: fn, found: isJust (Types.lookupEnv env fn) }) funcNames
      let allFound = Array.all _.found results
      if allFound
        then log $ "  PASS: All " <> show (Array.length funcNames) <> " functions exported"
        else do
          log "  FAIL: Missing functions:"
          for_ (Array.filter (not <<< _.found) results) \r ->
            log $ "    - " <> r.name

-- | Test with concatenated dependency declarations (simulating regenerate.js)
testFuncExportedWithDeps :: String -> String -> Array String -> Array String -> Effect Unit
testFuncExportedWithDeps name source funcNames depSources = do
  log $ "Test: " <> name
  case checkAndGetBindingsWithDeps source depSources of
    Left err -> log $ "  FAIL: " <> err
    Right env -> do
      let results = map (\fn -> { name: fn, found: isJust (Types.lookupEnv env fn) }) funcNames
      let allFound = Array.all _.found results
      if allFound
        then log $ "  PASS: All " <> show (Array.length funcNames) <> " functions exported"
        else do
          log "  FAIL: Missing functions:"
          for_ (Array.filter (not <<< _.found) results) \r ->
            log $ "    - " <> r.name

-- | Parse and type check with dependency declarations concatenated (like regenerate.js)
checkAndGetBindingsWithDeps :: String -> Array String -> Either String Types.Env
checkAndGetBindingsWithDeps source depSources =
  case CstPipeline.parseModuleCst source of
    Left err -> Left $ "Parse error: " <> err
    Right mod ->
      let modDecls = Array.fromFoldable mod.declarations
          -- Parse all dependency modules and concatenate their declarations
          parseDepModule src = case CstPipeline.parseModuleCst src of
            Left _ -> []
            Right depMod -> Array.fromFoldable depMod.declarations
          depDecls = Array.concatMap parseDepModule depSources
          -- Concatenate dep decls + module decls (like regenerate.js)
          allDecls = depDecls <> modDecls
      in case TC.checkModule Types.defaultRegistry Types.emptyEnv allDecls of
        Left err -> Left $ "Type error: " <> show err
        Right env -> Right env

-- | Parse and type check source, return the environment
checkAndGetBindings :: String -> Either String Types.Env
checkAndGetBindings source =
  case CstPipeline.parseModuleCst source of
    Left err -> Left $ "Parse error: " <> err
    Right mod ->
      let decls = Array.fromFoldable mod.declarations
      in case TC.checkModule Types.defaultRegistry Types.emptyEnv decls of
        Left err -> Left $ "Type error: " <> show err
        Right env -> Right env

-- Test modules

singleClauseModule :: String
singleClauseModule = """
module Test where

simpleFunc x = x + 1
"""

multiClauseModule :: String
multiClauseModule = """
module Test where

import Data.Maybe (Maybe(..))

multiClause Nothing = 0
multiClause (Just x) = x
"""

guardedModule :: String
guardedModule = """
module Test where

guardedFunc :: Int -> String
guardedFunc x
  | x > 0 = "positive"
  | x < 0 = "negative"
  | otherwise = "zero"
"""

whereClauseModule :: String
whereClauseModule = """
module Test where

withWhere x = helper x
  where
    helper y = y * 2
"""

typeSigModule :: String
typeSigModule = """
module Test where

typedFunc :: Int -> Int
typedFunc x = x + 1
"""

mixedModule :: String
mixedModule = """
module Test where

import Data.Maybe (Maybe(..))

simple x = x

multiMatch Nothing = 0
multiMatch (Just x) = x

guarded x
  | x > 0 = 1
  | otherwise = 0

withHelper x = helper x
  where
    helper y = y + 1
"""

-- Test module that mimics Unify.purs structure:
-- - Type class instance that references a function defined later
-- - Functions with `where` blocks
unifyLikeModule :: String
unifyLikeModule = """
module Test where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- Data type
data UnifyError
  = TypeMismatch String String
  | OccursCheck String

-- Instance that uses showType (defined below)
instance showUnifyError :: Show UnifyError where
  show (TypeMismatch t1 t2) = "Type mismatch: " <> showType t1 <> " vs " <> showType t2
  show (OccursCheck v) = "Occurs check: " <> v

-- Function referenced by instance above
showType :: String -> String
showType t = "Type(" <> t <> ")"

-- Simple functions
occurs :: String -> Boolean
occurs v = v == ""

bindVar :: String -> String -> Either UnifyError String
bindVar v t
  | v == t = Right t
  | occurs v = Left (OccursCheck v)
  | otherwise = Right t

-- Function with where clause
isRecordTypeAlias :: String -> Boolean
isRecordTypeAlias name = helper name
  where
    helper n = n /= ""

-- Multi-clause functions
unifyWithAliases :: String -> String -> String -> Either UnifyError String
unifyWithAliases aliases t1 t2 = Right (t1 <> t2)

unify :: String -> String -> Either UnifyError String
unify = unifyWithAliases ""

unifyMany :: Array String -> Array String -> Either UnifyError String
unifyMany t1s t2s = Right ""

unifyRecords :: String -> String -> Either UnifyError String
unifyRecords r1 r2 = Right (r1 <> r2)
"""
