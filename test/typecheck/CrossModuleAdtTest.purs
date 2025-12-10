-- | Minimal test for cross-module ADT type resolution
-- |
-- | This is the simplest possible test:
-- | - ModuleA.nova: defines `data Item = ItemPerson Person` where `Person = {name, age}`
-- | - ModuleB.nova: imports Item(..), pattern matches, accesses `p.name`
-- |
-- | If this test fails, the type checker doesn't properly resolve ADT constructor
-- | payload types from imported modules.
module Test.TypeCheck.CrossModuleAdtTest where

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
import Nova.Compiler.Types (emptyEnv, emptyRegistry, registerModule)

main :: Effect Unit
main = do
  log "=== Cross-Module ADT Test ==="
  log "Testing: Can we access record fields from imported ADT constructors?\n"

  -- Step 1: Load and typecheck Module A
  log "1. Loading ModuleA.nova (defines Item ADT)..."
  contentA <- readTextFile UTF8 "test/typecheck/crossmodule/ModuleA.nova"
  case parseModuleCst contentA of
    Left err -> log $ "  FAIL: Parse error: " <> err
    Right cstModA -> do
      let declsA = Array.fromFoldable cstModA.declarations
      let exportsA = extractExports declsA
      case checkModule emptyRegistry emptyEnv declsA of
        Left err -> log $ "  FAIL: Type error in ModuleA: " <> show err
        Right envA -> do
          let exportsA' = addValuesToExports exportsA envA declsA
          log $ "  OK: ModuleA typechecked"
          log $ "  Values: " <> show (Array.fromFoldable (Map.keys exportsA'.values))
          log $ "  Types: " <> show (Array.fromFoldable (Map.keys exportsA'.types))
          log $ "  Constructors: " <> show (Array.fromFoldable (Map.keys exportsA'.constructors))

          -- Step 2: Register Module A
          log "\n2. Registering ModuleA as Test.CrossModule.ModuleA..."
          let registry = registerModule emptyRegistry "Test.CrossModule.ModuleA" exportsA'

          -- Step 3: Load and typecheck Module B with registry
          log "\n3. Loading ModuleB.nova (imports and uses Item)..."
          contentB <- readTextFile UTF8 "test/typecheck/crossmodule/ModuleB.nova"
          case parseModuleCst contentB of
            Left err -> log $ "  FAIL: Parse error: " <> err
            Right cstModB -> do
              let declsB = Array.fromFoldable cstModB.declarations
              log $ "  Parsed " <> show (Array.length declsB) <> " declarations"

              -- This is the critical test - can we typecheck B with A's exports?
              log "\n4. Typechecking ModuleB with registry..."
              case checkModule registry emptyEnv declsB of
                Left err -> do
                  log $ "  FAIL: " <> show err
                  log "\n=== RESULT: Cross-module ADT resolution BROKEN ==="
                Right _ -> do
                  log "  OK: ModuleB typechecked successfully!"

                  -- Step 5: Test qualified imports (Module C)
                  log "\n5. Loading ModuleC.nova (uses qualified import A.Item)..."
                  contentC <- readTextFile UTF8 "test/typecheck/crossmodule/ModuleC.nova"
                  case parseModuleCst contentC of
                    Left err -> log $ "  FAIL: Parse error: " <> err
                    Right cstModC -> do
                      let declsC = Array.fromFoldable cstModC.declarations
                      log $ "  Parsed " <> show (Array.length declsC) <> " declarations"

                      log "\n6. Typechecking ModuleC with registry..."
                      case checkModule registry emptyEnv declsC of
                        Left err -> do
                          log $ "  FAIL: " <> show err
                          log "\n=== RESULT: Qualified import resolution BROKEN ==="
                        Right _ -> do
                          log "  OK: ModuleC typechecked successfully!"

                          -- Step 7: Add Data.List and Data.Foldable to registry
                          log "\n7. Loading Data.List and Data.Foldable..."
                          contentList <- readTextFile UTF8 "lib/Data/List.purs"
                          contentFold <- readTextFile UTF8 "lib/Data/Foldable.purs"
                          case parseModuleCst contentList of
                            Left err -> log $ "  FAIL: Parse List: " <> err
                            Right cstList -> case parseModuleCst contentFold of
                              Left err -> log $ "  FAIL: Parse Foldable: " <> err
                              Right cstFold -> do
                                let declsL = Array.fromFoldable cstList.declarations
                                let declsF = Array.fromFoldable cstFold.declarations
                                let exportsL = extractExports declsL
                                let exportsF = extractExports declsF
                                case checkModule registry emptyEnv declsL of
                                  Left _ -> log "  WARN: List typecheck failed"
                                  Right envL -> do
                                    let exportsL' = addValuesToExports exportsL envL declsL
                                    let registry2 = registerModule registry "Data.List" exportsL'
                                    case checkModule registry2 emptyEnv declsF of
                                      Left _ -> log "  WARN: Foldable typecheck failed"
                                      Right envF -> do
                                        let exportsF' = addValuesToExports exportsF envF declsF
                                        let registry3 = registerModule registry2 "Data.Foldable" exportsF'
                                        log $ "  OK: Added Data.List and Data.Foldable"

                                        -- Step 8: Test foldl with imported ADT
                                        log "\n8. Loading ModuleD.nova (foldl over List Item)..."
                                        contentD <- readTextFile UTF8 "test/typecheck/crossmodule/ModuleD.nova"
                                        case parseModuleCst contentD of
                                          Left err -> log $ "  FAIL: Parse error: " <> err
                                          Right cstModD -> do
                                            let declsD = Array.fromFoldable cstModD.declarations
                                            log $ "  Parsed " <> show (Array.length declsD) <> " declarations"

                                            log "\n9. Typechecking ModuleD (foldl with imported ADT)..."
                                            case checkModule registry3 emptyEnv declsD of
                                              Left err -> do
                                                log $ "  FAIL: " <> show err
                                                log "\n=== RESULT: foldl with imported ADT BROKEN ==="
                                              Right _ -> do
                                                log "  OK: ModuleD typechecked successfully!"
                                                log "\n=== RESULT: All tests PASS ==="
