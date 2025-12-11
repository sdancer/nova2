module Test.TypeCheck.CombinedSetSchemeTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.TypeChecker (checkModule, extractExports, addValuesToExports)
import Nova.Compiler.Types (emptyEnv, emptyRegistry, defaultRegistry, registerModule, ModuleRegistry)

main :: Effect Unit
main = do
  log "=== Combined Set + Scheme Test ==="
  log "Testing: Can we use Set.empty AND destructure Scheme in same module?\n"

  -- Build registry step by step
  registry <- buildRegistry
  case registry of
    Nothing -> log "Failed to build registry"
    Just r -> testModuleI r

buildRegistry :: Effect (Maybe ModuleRegistry)
buildRegistry = do
  -- Load Data.Foldable
  log "0. Loading Data.Foldable..."
  contentF <- readTextFile UTF8 "lib/Data/Foldable.purs"
  case parseModuleCst contentF of
    Left _ -> pure Nothing
    Right cstF -> do
      let declsF = Array.fromFoldable cstF.declarations
      let exportsF = extractExports declsF
      case checkModule defaultRegistry emptyEnv declsF of
        Left err -> do
          log $ "  FAIL: " <> show err
          pure Nothing
        Right envF -> do
          let exportsF' = addValuesToExports exportsF envF declsF
          log "  OK"
          let reg1 = registerModule defaultRegistry "Data.Foldable" exportsF'

          -- Load Data.Set
          log "\n1. Loading Data.Set..."
          contentS <- readTextFile UTF8 "lib/Data/Set.purs"
          case parseModuleCst contentS of
            Left _ -> pure Nothing
            Right cstS -> do
              let declsS = Array.fromFoldable cstS.declarations
              let exportsS = extractExports declsS
              case checkModule reg1 emptyEnv declsS of
                Left err -> do
                  log $ "  FAIL: " <> show err
                  pure Nothing
                Right envS -> do
                  let exportsS' = addValuesToExports exportsS envS declsS
                  log "  OK"
                  let reg2 = registerModule reg1 "Data.Set" exportsS'

                  pure (Just reg2)

testModuleI :: ModuleRegistry -> Effect Unit
testModuleI registry = do
  log "\n3. Loading ModuleI..."
  contentI <- readTextFile UTF8 "test/typecheck/crossmodule/ModuleI.nova"
  case parseModuleCst contentI of
    Left err -> log $ "  Parse error: " <> err
    Right cstI -> do
      let declsI = Array.fromFoldable cstI.declarations
      log "4. Typechecking ModuleI..."
      case checkModule registry emptyEnv declsI of
        Left err -> do
          log $ "  FAIL: " <> show err
          log "\n=== RESULT: Combined Set+Scheme BROKEN ==="
        Right _ -> do
          log "  OK"
          log "\n=== RESULT: Combined Set+Scheme WORKS ==="
