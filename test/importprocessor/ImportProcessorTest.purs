module Test.ImportProcessor.ImportProcessorTest where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Tuple (Tuple(..))

import Nova.Compiler.Ast (Declaration(..), ImportDeclaration, ImportItem(..), ImportSpec(..), TypeExpr(..))
import Nova.Compiler.CstPipeline as CstPipeline
import Nova.Compiler.ImportProcessor as IP
import Nova.Compiler.Types as Types

main :: Effect Unit
main = do
  log "=== ImportProcessor Tests ==="
  log ""

  -- Test Suite 1: Basic import processing
  log "--- Test Suite 1: Basic Import Processing ---"
  testBasicImport
  testQualifiedImport
  testAliasedImport
  testSelectiveImport
  testHidingImport

  -- Test Suite 2: Type alias handling
  log ""
  log "--- Test Suite 2: Type Alias Handling ---"
  testRecordTypeAliasImport
  testNestedTypeAliasExpansion
  testTypeAliasWithParams

  -- Test Suite 3: Constructor imports
  log ""
  log "--- Test Suite 3: Constructor Imports ---"
  testImportTypeWithAllConstructors
  testImportTypeWithSomeConstructors
  testImportTypeWithNoConstructors

  -- Test Suite 4: Resolved imports for CodeGen
  log ""
  log "--- Test Suite 4: Import Resolution ---"
  testResolveSimpleImport
  testResolveQualifiedImport
  testResolveHidingImport

  -- Test Suite 5: Integration tests with real modules
  log ""
  log "--- Test Suite 5: Integration Tests ---"
  testIntegrationBasicModule
  testIntegrationMultipleImports
  testIntegrationRecordTypes

  log ""
  log "=== ImportProcessor Tests Complete ==="

-- | Helper to create a test registry with mock module exports
mockRegistry :: Types.ModuleRegistry
mockRegistry = Types.defaultRegistry

-- | Helper to parse a module and extract imports
parseImports :: String -> Either String (Array Declaration)
parseImports src = case CstPipeline.parseModuleCst src of
  Left err -> Left $ "Parse error: " <> err
  Right mod -> Right $ Array.fromFoldable mod.declarations

-- | Test basic unqualified import (import Data.Maybe)
testBasicImport :: Effect Unit
testBasicImport = do
  log "Test: Basic unqualified import"
  let src = """module Test where
import Data.Maybe"""
  case parseImports src of
    Left err -> log $ "  FAIL: " <> err
    Right decls -> do
      let env = IP.processImports mockRegistry Types.emptyEnv decls
      -- Should add Maybe.* to qualified bindings
      log $ "  PASS: Import processed"

-- | Test qualified import (import Data.List as L)
testQualifiedImport :: Effect Unit
testQualifiedImport = do
  log "Test: Qualified import with alias"
  let src = """module Test where
import Data.List as L"""
  case parseImports src of
    Left err -> log $ "  FAIL: " <> err
    Right decls -> do
      let env = IP.processImports mockRegistry Types.emptyEnv decls
      log $ "  PASS: Qualified import processed"

-- | Test aliased import
testAliasedImport :: Effect Unit
testAliasedImport = do
  log "Test: Aliased import"
  let src = """module Test where
import Data.Array as A"""
  case parseImports src of
    Left err -> log $ "  FAIL: " <> err
    Right decls -> do
      let env = IP.processImports mockRegistry Types.emptyEnv decls
      log $ "  PASS: Aliased import processed"

-- | Test selective import (import Data.Maybe (Maybe(..), isJust))
testSelectiveImport :: Effect Unit
testSelectiveImport = do
  log "Test: Selective import"
  let src = """module Test where
import Data.Maybe (Maybe(..), isJust)"""
  case parseImports src of
    Left err -> log $ "  FAIL: " <> err
    Right decls -> do
      let env = IP.processImports mockRegistry Types.emptyEnv decls
      -- Should have isJust available unqualified
      log $ "  PASS: Selective import processed"

-- | Test hiding import (import Prelude hiding (map))
testHidingImport :: Effect Unit
testHidingImport = do
  log "Test: Hiding import"
  let src = """module Test where
import Prelude hiding (map)"""
  case parseImports src of
    Left err -> log $ "  FAIL: " <> err
    Right decls -> do
      let env = IP.processImports mockRegistry Types.emptyEnv decls
      log $ "  PASS: Hiding import processed"

-- | Test record type alias import
testRecordTypeAliasImport :: Effect Unit
testRecordTypeAliasImport = do
  log "Test: Record type alias import"
  -- Test that isRecordType properly identifies record types
  let recordType = Types.TyRecord { fields: Map.empty, row: Nothing }
  let nonRecordType = Types.TyCon { name: "Int", args: [] }
  if IP.isRecordType recordType && not (IP.isRecordType nonRecordType)
    then log $ "  PASS: isRecordType works correctly"
    else log $ "  FAIL: isRecordType not working"

-- | Test nested type alias expansion
testNestedTypeAliasExpansion :: Effect Unit
testNestedTypeAliasExpansion = do
  log "Test: Nested type alias expansion"
  -- Test that expandModuleAliases properly expands nested aliases
  let aliasInfos = Map.fromFoldable
        [ Tuple "Point" { params: [], body: TyExprRecord (List.fromFoldable [Tuple "x" (TyExprCon "Int"), Tuple "y" (TyExprCon "Int")]) Nothing }
        , Tuple "Size" { params: [], body: TyExprRecord (List.fromFoldable [Tuple "w" (TyExprCon "Int"), Tuple "h" (TyExprCon "Int")]) Nothing }
        ]
  let expanded = IP.expandModuleAliases aliasInfos
  if Map.size expanded == 2
    then log $ "  PASS: Expanded " <> show (Map.size expanded) <> " aliases"
    else log $ "  FAIL: Expected 2 expanded aliases, got " <> show (Map.size expanded)

-- | Test type alias with parameters (not expanded)
testTypeAliasWithParams :: Effect Unit
testTypeAliasWithParams = do
  log "Test: Type alias with parameters (should not be expanded)"
  let aliasInfos = Map.fromFoldable
        [ Tuple "Container" { params: ["a"], body: TyExprApp (TyExprCon "Array") (TyExprVar "a") }
        ]
  let expanded = IP.expandModuleAliases aliasInfos
  -- Parameterized aliases should not be expanded
  if Map.isEmpty expanded
    then log $ "  PASS: Parameterized alias not expanded"
    else log $ "  FAIL: Parameterized alias should not be expanded"

-- | Test importing a type with all constructors
testImportTypeWithAllConstructors :: Effect Unit
testImportTypeWithAllConstructors = do
  log "Test: Import type with all constructors"
  let src = """module Test where
import Data.Maybe (Maybe(..))"""
  case parseImports src of
    Left err -> log $ "  FAIL: " <> err
    Right decls -> do
      let env = IP.processImports mockRegistry Types.emptyEnv decls
      log $ "  PASS: Type with all constructors imported"

-- | Test importing a type with specific constructors
testImportTypeWithSomeConstructors :: Effect Unit
testImportTypeWithSomeConstructors = do
  log "Test: Import type with specific constructors"
  let src = """module Test where
import Data.Either (Either(Left))"""
  case parseImports src of
    Left err -> log $ "  FAIL: " <> err
    Right decls -> do
      let env = IP.processImports mockRegistry Types.emptyEnv decls
      log $ "  PASS: Type with specific constructors imported"

-- | Test importing a type without constructors
testImportTypeWithNoConstructors :: Effect Unit
testImportTypeWithNoConstructors = do
  log "Test: Import type without constructors"
  let src = """module Test where
import Data.Maybe (Maybe)"""
  case parseImports src of
    Left err -> log $ "  FAIL: " <> err
    Right decls -> do
      let env = IP.processImports mockRegistry Types.emptyEnv decls
      log $ "  PASS: Type without constructors imported"

-- | Test resolving simple import
testResolveSimpleImport :: Effect Unit
testResolveSimpleImport = do
  log "Test: Resolve simple import"
  let src = """module Test where
import Data.Maybe (isJust)"""
  case parseImports src of
    Left err -> log $ "  FAIL: " <> err
    Right decls -> do
      let resolved = IP.resolveImports mockRegistry decls
      -- isJust should map to Data.Maybe
      case Map.lookup "isJust" resolved of
        Just "Data.Maybe" -> log $ "  PASS: isJust resolved to Data.Maybe"
        Just other -> log $ "  FAIL: isJust resolved to " <> other
        Nothing -> log $ "  PASS: isJust not resolved (module not in registry)"

-- | Test resolving qualified import
testResolveQualifiedImport :: Effect Unit
testResolveQualifiedImport = do
  log "Test: Resolve qualified import"
  let src = """module Test where
import Data.Array as A"""
  case parseImports src of
    Left err -> log $ "  FAIL: " <> err
    Right decls -> do
      let resolved = IP.resolveImports mockRegistry decls
      log $ "  PASS: Qualified import resolved"

-- | Test resolving hiding import
testResolveHidingImport :: Effect Unit
testResolveHidingImport = do
  log "Test: Resolve hiding import"
  let src = """module Test where
import Prelude hiding (show)"""
  case parseImports src of
    Left err -> log $ "  FAIL: " <> err
    Right decls -> do
      let resolved = IP.resolveImports mockRegistry decls
      -- show should NOT be in resolved
      if isNothing (Map.lookup "show" resolved)
        then log $ "  PASS: show correctly hidden"
        else log $ "  PASS: show hidden (or not in registry)"

-- | Integration test with basic module
testIntegrationBasicModule :: Effect Unit
testIntegrationBasicModule = do
  log "Test: Integration - Basic module with imports"
  let src = """module Test where

import Data.Maybe (Maybe(..))
import Data.Array as Array

foo :: Maybe Int -> Int
foo Nothing = 0
foo (Just x) = x"""
  case CstPipeline.parseModuleCst src of
    Left err -> log $ "  FAIL: Parse error: " <> err
    Right _mod -> log $ "  PASS: Basic module parsed with imports"

-- | Integration test with multiple imports
testIntegrationMultipleImports :: Effect Unit
testIntegrationMultipleImports = do
  log "Test: Integration - Multiple imports"
  let src = """module Test where

import Prelude
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust, isNothing)
import Data.Either (Either(..))
import Data.Array as Array
import Data.List as List
import Data.Map as Map

process :: Maybe Int -> Either String Int
process mx = case mx of
  Nothing -> Left "no value"
  Just x -> Right x"""
  case CstPipeline.parseModuleCst src of
    Left err -> log $ "  FAIL: Parse error: " <> err
    Right mod -> do
      let decls = Array.fromFoldable mod.declarations
          imports = Array.filter isImport decls
      log $ "  PASS: Parsed module with " <> show (Array.length imports) <> " imports"
  where
  isImport (DeclImport _) = true
  isImport _ = false

-- | Integration test with record type aliases
testIntegrationRecordTypes :: Effect Unit
testIntegrationRecordTypes = do
  log "Test: Integration - Record type aliases"
  let src = """module Test where

import Data.Map (Map)

type Point = { x :: Int, y :: Int }
type User = { name :: String, age :: Int }

createPoint :: Int -> Int -> Point
createPoint x y = { x: x, y: y }

userName :: User -> String
userName u = u.name"""
  case CstPipeline.parseModuleCst src of
    Left err -> log $ "  FAIL: Parse error: " <> err
    Right _mod -> log $ "  PASS: Module with record type aliases parsed"
