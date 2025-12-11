module Test.TypeCheck.CrossModuleSchemeTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Array as Array
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.TypeChecker (checkModule, extractExports, addValuesToExports)
import Nova.Compiler.Types (emptyEnv, emptyRegistry, registerModule)

main :: Effect Unit
main = do
  log "=== Cross-Module Scheme Type Alias Test ==="
  log "Testing: Can we destructure an imported type alias?\n"

  -- Step 1: Load ModuleG (defines Scheme)
  log "1. Loading ModuleG.nova (defines Scheme type alias)..."
  contentG <- readTextFile UTF8 "test/typecheck/crossmodule/ModuleG.nova"
  case parseModuleCst contentG of
    Left err -> log $ "  Parse error: " <> err
    Right cstG -> do
      let declsG = Array.fromFoldable cstG.declarations
      let exportsG = extractExports declsG

      case checkModule emptyRegistry emptyEnv declsG of
        Left err -> log $ "  FAIL: " <> show err
        Right envG -> do
          let exportsG' = addValuesToExports exportsG envG declsG
          log $ "  OK: ModuleG typechecked"

          -- Step 2: Register ModuleG
          let registry = registerModule emptyRegistry "Test.CrossModule.ModuleG" exportsG'

          -- Step 3: Load and typecheck ModuleH
          log "\n2. Loading ModuleH.nova (imports Scheme, does record destructuring)..."
          contentH <- readTextFile UTF8 "test/typecheck/crossmodule/ModuleH.nova"
          case parseModuleCst contentH of
            Left err -> log $ "  Parse error: " <> err
            Right cstH -> do
              let declsH = Array.fromFoldable cstH.declarations

              log "3. Typechecking ModuleH..."
              case checkModule registry emptyEnv declsH of
                Left err -> do
                  log $ "  FAIL: " <> show err
                  log "\n=== RESULT: Cross-module Scheme destructuring BROKEN ==="
                Right _ -> do
                  log "  OK: ModuleH typechecked"
                  log "\n=== RESULT: Cross-module Scheme destructuring WORKS ==="
