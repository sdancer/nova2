-- | Test: Cross-module type alias field access
-- |
-- | This test reproduces the Dependencies.purs failure:
-- | When Module type alias is imported from another module,
-- | accessing m.declarations should work if type alias is properly expanded.
module Test.TypeCheck.TypeAliasFieldTest where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Array as Array
import Data.Map as Map
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.TypeChecker (checkModule, extractExports, addValuesToExports)
import Nova.Compiler.Types (emptyEnv, emptyRegistry, registerModule, ModuleRegistry)

main :: Effect Unit
main = do
  log "=== Type Alias Field Access Test ==="
  log "Testing: Can we access record fields via imported type alias?\n"

  -- Step 0: Load Data.List first
  log "0. Loading Data.List..."
  contentList <- readTextFile UTF8 "lib/Data/List.purs"
  case parseModuleCst contentList of
    Left err -> log $ "  FAIL: Parse error: " <> err
    Right cstList -> do
      let declsL = Array.fromFoldable cstList.declarations
      let exportsL = extractExports declsL
      case checkModule emptyRegistry emptyEnv declsL of
        Left err -> log $ "  FAIL: Type error: " <> show err
        Right envL -> do
          let exportsL' = addValuesToExports exportsL envL declsL
          let registryWithList = registerModule emptyRegistry "Data.List" exportsL'
          log $ "  OK: Data.List loaded"
          runTests registryWithList

runTests :: ModuleRegistry -> Effect Unit
runTests baseRegistry = do
  -- Step 1: Load ModuleE (defines Module type alias and Declaration ADT)
  log "\n1. Loading ModuleE.nova..."
  contentE <- readTextFile UTF8 "test/typecheck/crossmodule/ModuleE.nova"
  case parseModuleCst contentE of
    Left err -> log $ "  FAIL: Parse error: " <> err
    Right cstModE -> do
      let declsE = Array.fromFoldable cstModE.declarations
      let exportsE = extractExports declsE
      log $ "  Type aliases in exports: " <> show (Array.fromFoldable (Map.keys exportsE.typeAliases))
      log $ "  Expanded type aliases: " <> show (Array.fromFoldable (Map.keys exportsE.expandedTypeAliases))

      case checkModule baseRegistry emptyEnv declsE of
        Left err -> log $ "  FAIL: Type error: " <> show err
        Right envE -> do
          let exportsE' = addValuesToExports exportsE envE declsE
          log $ "  OK: ModuleE typechecked"
          log $ "  Type aliases after addValues: " <> show (Array.fromFoldable (Map.keys exportsE'.typeAliases))
          log $ "  Expanded aliases after addValues: " <> show (Array.fromFoldable (Map.keys exportsE'.expandedTypeAliases))

          -- Step 2: Register ModuleE
          log "\n2. Registering ModuleE..."
          let registry = registerModule baseRegistry "Test.CrossModule.ModuleE" exportsE'

          -- Step 3: Load and typecheck ModuleF with registry
          log "\n3. Loading ModuleF.nova (imports Module type alias, accesses m.declarations)..."
          contentF <- readTextFile UTF8 "test/typecheck/crossmodule/ModuleF.nova"
          case parseModuleCst contentF of
            Left err -> log $ "  FAIL: Parse error: " <> err
            Right cstModF -> do
              let declsF = Array.fromFoldable cstModF.declarations
              log $ "  Parsed " <> show (Array.length declsF) <> " declarations"

              log "\n4. Typechecking ModuleF..."
              case checkModule registry emptyEnv declsF of
                Left err -> do
                  log $ "  FAIL: " <> show err
                  log "\n=== RESULT: Type alias field access BROKEN ==="
                  log "This reproduces the Dependencies.purs failure!"
                Right _ -> do
                  log "  OK: ModuleF typechecked successfully!"
                  log "\n=== RESULT: Type alias field access WORKS ==="
