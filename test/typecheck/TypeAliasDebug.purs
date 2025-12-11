module Test.TypeAliasDebug where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Foldable (foldM)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.TypeChecker (checkModule, extractExportsWithRegistry, addValuesToExports)
import Nova.Compiler.Types (emptyEnv, ModuleRegistry, defaultRegistry, registerModule, lookupModule)
import Nova.Compiler.Types as Types
import Data.Map as Map

type ModuleInfo = { path :: String, name :: String }
type CompileState = { registry :: ModuleRegistry, ok :: Boolean }

compileModule :: CompileState -> ModuleInfo -> Effect CompileState
compileModule state modInfo = do
  content <- readTextFile UTF8 modInfo.path
  case parseModuleCst content of
    Left err -> do
      log $ "  FAIL parse " <> modInfo.name <> ": " <> err
      pure state { ok = false }
    Right m -> do
      let decls = Array.fromFoldable m.declarations
      let initialExports = extractExportsWithRegistry state.registry decls
      case checkModule state.registry emptyEnv decls of
        Left err -> do
          log $ "  FAIL tc " <> modInfo.name <> ": " <> show err
          -- Still register with initial exports
          let newReg = registerModule state.registry modInfo.name initialExports
          pure state { registry = newReg, ok = false }
        Right env -> do
          log $ "  OK " <> modInfo.name
          let fullExports = addValuesToExports initialExports env decls
          let newReg = registerModule state.registry modInfo.name fullExports
          pure state { registry = newReg }

main :: Effect Unit
main = do
  log "=== Type Alias Debug Test ==="
  log ""
  log "1. Building registry with lib + Types..."

  -- Build dependency chain up to Types
  let baseModules =
        [ { path: "lib/Data/Tuple.purs", name: "Data.Tuple" }
        , { path: "lib/Data/Maybe.purs", name: "Data.Maybe" }
        , { path: "lib/Data/Either.purs", name: "Data.Either" }
        , { path: "lib/Data/Char.purs", name: "Data.Char" }
        , { path: "lib/Data/Int.purs", name: "Data.Int" }
        , { path: "lib/Data/Number.purs", name: "Data.Number" }
        , { path: "lib/Data/Array.purs", name: "Data.Array" }
        , { path: "lib/Data/List.purs", name: "Data.List" }
        , { path: "lib/Data/String.purs", name: "Data.String" }
        , { path: "lib/Data/String/CodeUnits.purs", name: "Data.String.CodeUnits" }
        , { path: "lib/Data/Foldable.purs", name: "Data.Foldable" }
        , { path: "lib/Data/Map.purs", name: "Data.Map" }
        , { path: "lib/Data/Set.purs", name: "Data.Set" }
        , { path: "lib/Control/Lazy.purs", name: "Control.Lazy" }
        , { path: "src/Nova/Compiler/Ast.purs", name: "Nova.Compiler.Ast" }
        , { path: "src/Nova/Compiler/Types.purs", name: "Nova.Compiler.Types" }
        , { path: "src/Nova/Compiler/Unify.purs", name: "Nova.Compiler.Unify" }
        , { path: "src/Nova/Compiler/ImportProcessor.purs", name: "Nova.Compiler.ImportProcessor" }
        ]

  { registry: reg, ok: _ } <- foldM compileModule { registry: defaultRegistry, ok: true } baseModules

  log ""
  log "2. Testing if getAliasInfoBody is in registry..."

  -- Check what Types exports for getAliasInfoBody
  case Types.lookupModule reg "Nova.Compiler.Types" of
    Nothing -> log "  ERROR: Nova.Compiler.Types not in registry"
    Just exports -> do
      log $ "  Types module found in registry"
      log $ "  Checking for getAliasInfoBody in values..."
      case Map.lookup "getAliasInfoBody" exports.values of
        Nothing -> log "  WARNING: getAliasInfoBody NOT in exports.values"
        Just _ -> log $ "  getAliasInfoBody found in exports"
      log $ "  Checking for TypeAliasInfo in typeAliases..."
      case Map.lookup "TypeAliasInfo" exports.typeAliases of
        Nothing -> log "  WARNING: TypeAliasInfo NOT in exports.typeAliases"
        Just _ -> log $ "  TypeAliasInfo found in typeAliases"
      log $ "  Checking for TypeAliasInfo in expandedTypeAliases..."
      case Map.lookup "TypeAliasInfo" exports.expandedTypeAliases of
        Nothing -> log "  WARNING: TypeAliasInfo NOT in exports.expandedTypeAliases"
        Just _ -> log $ "  TypeAliasInfo found in expandedTypeAliases"

  log ""
  log "3. Testing minimal source with nested function..."

  -- This mimics the structure of typeExprToTypeWithAllAliases
  let testSrc = """module Test.Mini where

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Nova.Compiler.Types (Type(..), TypeAliasInfo, getAliasInfoBody, getAliasInfoParams)
import Nova.Compiler.Ast (TypeExpr(..))

-- Mimics typeExprToTypeWithAllAliases with nested tryLookup
-- This time actually USING bodyExpr
testFn :: Map.Map String Type -> Map.Map String TypeAliasInfo -> String -> Maybe Type
testFn aliasMap paramAliasMap name =
  let tryLookup :: String -> Maybe Type
      tryLookup nm = case Map.lookup nm aliasMap of
        Just ty -> Just ty
        Nothing -> case Map.lookup nm paramAliasMap of
          Just info ->
            let ps = getAliasInfoParams info
                bodyExpr = getAliasInfoBody info
            in if Array.null ps
               -- Here we need to do something with bodyExpr
               -- In real code it would be: typeExprToTypeWithAllAliases ... bodyExpr
               -- For test, just return a dummy type based on bodyExpr
               then Just (TyCon { name: "Dummy", args: [] })
               else Nothing
          _ -> Nothing
  in tryLookup name
"""
  case parseModuleCst testSrc of
    Left err -> log $ "  Parse error: " <> err
    Right testCst -> do
      let testDecls = Array.fromFoldable testCst.declarations
      case checkModule reg emptyEnv testDecls of
        Left err -> log $ "  FAIL getAliasInfoBody test: " <> show err
        Right _ -> log "  OK: getAliasInfoBody works!"

  log ""
  log "4. Now testing TypeChecker.purs..."

  tcContent <- readTextFile UTF8 "src/Nova/Compiler/TypeChecker.purs"
  case parseModuleCst tcContent of
    Left err -> log $ "Parse error TypeChecker: " <> err
    Right tcCst -> do
      let tcDecls = Array.fromFoldable tcCst.declarations
      case checkModule reg emptyEnv tcDecls of
        Left err -> log $ "FAIL TypeChecker: " <> show err
        Right _ -> log "OK: TypeChecker.purs passes!"
