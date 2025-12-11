module Test.TypeCheck.DebugCollectTypeNames where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Foldable (foldM)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir)
import Data.String as String
import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.TypeChecker (checkModule, extractExports, addValuesToExports)
import Nova.Compiler.Types (emptyEnv, defaultRegistry, registerModule, ModuleRegistry)

main :: Effect Unit
main = do
  log "=== Debug collectTypeNames Type Check ==="
  log ""

  -- Build a minimal registry with just Data.Set
  log "1. Building registry with Data.Set..."
  contentSet <- readTextFile UTF8 "lib/Data/Set.purs"
  case parseModuleCst contentSet of
    Left err -> log $ "  Parse error: " <> err
    Right cstSet -> do
      let declsSet = Array.fromFoldable cstSet.declarations
      let exportsSet = extractExports declsSet
      case checkModule defaultRegistry emptyEnv declsSet of
        Left err -> log $ "  FAIL Data.Set: " <> show err
        Right envSet -> do
          log "  OK Data.Set"
          let exportsSet' = addValuesToExports exportsSet envSet declsSet
          let reg1 = registerModule defaultRegistry "Data.Set" exportsSet'

          -- Test with just Data.Set (skip Types for now)
          log ""
          log "2. Testing minimal source with Set.Set String..."
          testMinimalSource reg1

testMinimalSource :: ModuleRegistry -> Effect Unit
testMinimalSource registry = do
  -- Test 1: Just Set.Set String
  let src1 = """module Test.Mini1 where

import Data.Set as Set

testFn :: Int -> Set.Set String
testFn _ = Set.empty
"""

  case parseModuleCst src1 of
    Left err -> log $ "  Parse error: " <> err
    Right cst -> do
      let decls = Array.fromFoldable cst.declarations
      case checkModule registry emptyEnv decls of
        Left err -> log $ "  FAIL test1 (just Set): " <> show err
        Right _ -> do
          log "  OK test1: Set.Set String works alone"

          -- Test 2: Add Scheme type alias and use it
          log ""
          log "3. Testing with local Scheme alias..."
          testWithScheme registry

testWithScheme :: ModuleRegistry -> Effect Unit
testWithScheme registry = do
  let src = """module Test.Mini2 where

import Data.Set as Set

type Scheme = { vars :: Array String, ty :: String }

-- Use Scheme in a function
mkScheme :: Scheme -> Scheme
mkScheme s = s

-- Also use Set
testSet :: Set.Set String
testSet = Set.empty
"""

  case parseModuleCst src of
    Left err -> log $ "  Parse error: " <> err
    Right cst -> do
      let decls = Array.fromFoldable cst.declarations
      case checkModule registry emptyEnv decls of
        Left err -> log $ "  FAIL test2 (Set + Scheme): " <> show err
        Right _ -> do
          log "  OK test2: Set + Scheme together works!"

          -- Test 3: Same pattern as collectTypeNames - use both in same function via pattern matching
          log ""
          log "4. Testing Set inside Scheme-like record pattern..."
          testPatternMatch registry

testPatternMatch :: ModuleRegistry -> Effect Unit
testPatternMatch registry = do
  let src = """module Test.Mini3 where

import Data.Set as Set

type Scheme = { vars :: Array String, ty :: String }

-- This mimics how instantiate works - destructure scheme, return Set
processScheme :: Scheme -> Set.Set String
processScheme scheme =
  let { ty, vars } = scheme
  in Set.empty
"""

  case parseModuleCst src of
    Left err -> log $ "  Parse error: " <> err
    Right cst -> do
      let decls = Array.fromFoldable cst.declarations
      case checkModule registry emptyEnv decls of
        Left err -> log $ "  FAIL test3 (pattern match): " <> show err
        Right _ -> log "  OK test3: Scheme pattern + Set return works!"
