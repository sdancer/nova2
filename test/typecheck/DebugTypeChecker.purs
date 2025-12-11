module Test.TypeCheck.DebugTypeChecker where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Array as Array
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Set as Set
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir)
import Data.String as String
import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.TypeChecker (checkModule, extractExports, addValuesToExports)
import Nova.Compiler.Types (emptyEnv, defaultRegistry, registerModule, ModuleRegistry, ModuleExports)

main :: Effect Unit
main = do
  log "=== Debug TypeChecker.purs Type Check ==="
  log ""

  -- Build registry step by step
  log "1. Building base registry from lib modules..."
  registry <- buildLibRegistry

  -- Show what's in the registry
  log ""
  log "2. Checking what's in the registry..."

  -- Load and check TypeChecker.purs
  log ""
  log "3. Loading TypeChecker.purs..."
  content <- readTextFile UTF8 "src/Nova/Compiler/TypeChecker.purs"
  case parseModuleCst content of
    Left err -> log $ "  Parse error: " <> err
    Right cst -> do
      let decls = Array.fromFoldable cst.declarations
      log $ "  Parsed " <> show (Array.length decls) <> " declarations"

      log ""
      log "4. Type checking..."
      case checkModule registry emptyEnv decls of
        Left err -> log $ "  FAIL: " <> show err
        Right _ -> log "  OK!"

buildLibRegistry :: Effect ModuleRegistry
buildLibRegistry = do
  -- Build registry with lib modules in dependency order
  r0 <- pure defaultRegistry

  -- Data modules
  r1 <- addModule r0 "lib/Data/Tuple.purs" "Data.Tuple"
  r2 <- addModule r1 "lib/Data/Maybe.purs" "Data.Maybe"
  r3 <- addModule r2 "lib/Data/Either.purs" "Data.Either"
  r4 <- addModule r3 "lib/Data/Array.purs" "Data.Array"
  r5 <- addModule r4 "lib/Data/List.purs" "Data.List"
  r6 <- addModule r5 "lib/Data/Foldable.purs" "Data.Foldable"
  r7 <- addModule r6 "lib/Data/Map.purs" "Data.Map"
  r8 <- addModule r7 "lib/Data/Set.purs" "Data.Set"
  r9 <- addModule r8 "lib/Data/String.purs" "Data.String"

  -- Compiler modules (TypeChecker's dependencies)
  r10 <- addModule r9 "src/Nova/Compiler/Ast.purs" "Nova.Compiler.Ast"
  r11 <- addModule r10 "src/Nova/Compiler/Types.purs" "Nova.Compiler.Types"
  r12 <- addModule r11 "src/Nova/Compiler/Unify.purs" "Nova.Compiler.Unify"
  r13 <- addModule r12 "src/Nova/Compiler/ImportProcessor.purs" "Nova.Compiler.ImportProcessor"

  pure r13

addModule :: ModuleRegistry -> String -> String -> Effect ModuleRegistry
addModule registry path modName = do
  content <- readTextFile UTF8 path
  case parseModuleCst content of
    Left err -> do
      log $ "  Skip " <> modName <> " - Parse error"
      pure registry
    Right cst -> do
      let decls = Array.fromFoldable cst.declarations
      let initialExports = extractExports decls
      case checkModule registry emptyEnv decls of
        Left err -> do
          log $ "  Partial " <> modName <> " - Type error, using initial exports"
          pure $ registerModule registry modName initialExports
        Right env -> do
          log $ "  OK " <> modName
          let fullExports = addValuesToExports initialExports env decls
          pure $ registerModule registry modName fullExports
