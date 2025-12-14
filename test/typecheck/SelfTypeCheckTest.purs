module Test.TypeCheck.SelfTypeCheckTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Data.Either (Either(..))
import Data.Array as Array
import Data.Foldable (foldM)
import Data.Traversable (traverse)
import Data.String as String
import Control.Monad (when)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir)
import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.Types (emptyEnv, ModuleRegistry, ModuleExports, defaultRegistry, registerModule, preludeExports)
import Data.Map as Map
import Nova.Compiler.TypeChecker (checkModule, TCError, extractExports, extractExportsWithRegistry, addValuesToExports)
import Nova.Compiler.Ast (Declaration(..), FunctionDeclaration)

-- | Test result tracking
type TestResult = { passed :: Int, failed :: Int, errors :: Array String }

initResult :: TestResult
initResult = { passed: 0, failed: 0, errors: [] }

main :: Effect Unit
main = do
  log "=== Self Type-Check Test ==="
  log "Compiling all modules in topological order"
  log ""

  -- All modules in dependency order - lib first, then compiler
  -- Nova.Prelude MUST be first and registers as "Prelude" for standard imports
  let allModules =
        [ { path: "lib/Nova/Prelude.purs", name: "Prelude" }  -- Standard Prelude
        , { path: "lib/Data/Tuple.purs", name: "Data.Tuple" }
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
        , { path: "src/Nova/Compiler/Cst.purs", name: "Nova.Compiler.Cst" }
        , { path: "src/Nova/Compiler/Types.purs", name: "Nova.Compiler.Types" }
        , { path: "src/Nova/Compiler/Unify.purs", name: "Nova.Compiler.Unify" }
        , { path: "src/Nova/Compiler/CstLayout.purs", name: "Nova.Compiler.CstLayout" }
        , { path: "src/Nova/Compiler/CstLexer.purs", name: "Nova.Compiler.CstLexer" }
        , { path: "src/Nova/Compiler/CstParser.purs", name: "Nova.Compiler.CstParser" }
        , { path: "src/Nova/Compiler/CstToAst.purs", name: "Nova.Compiler.CstToAst" }
        , { path: "src/Nova/Compiler/CstPipeline.purs", name: "Nova.Compiler.CstPipeline" }
        , { path: "src/Nova/Compiler/ImportProcessor.purs", name: "Nova.Compiler.ImportProcessor" }
        , { path: "src/Nova/Compiler/TypeChecker.purs", name: "Nova.Compiler.TypeChecker" }
        , { path: "src/Nova/Compiler/RefEq.purs", name: "Nova.Compiler.RefEq" }
        , { path: "src/Nova/Compiler/Dependencies.purs", name: "Nova.Compiler.Dependencies" }
        , { path: "src/Nova/Compiler/CodeGenCoreErlang.purs", name: "Nova.Compiler.CodeGenCoreErlang" }
        ]

  -- Compile all modules in order, accumulating registry and results
  { registry: _, result } <- foldM compileModule { registry: defaultRegistry, result: initResult } allModules

  log ""
  log "=== Summary ==="
  log $ "Passed: " <> show result.passed
  log $ "Failed: " <> show result.failed

  when (result.failed > 0) do
    log ""
    log "Errors (first 10):"
    void $ traverse (\e -> log $ "  " <> e) (Array.take 10 result.errors)
    throw $ "Self type-check failed: " <> show result.failed <> " files failed"

-- | Compile a single module, adding it to registry if successful
type CompileState = { registry :: ModuleRegistry, result :: TestResult }

compileModule :: CompileState -> ModuleInfo -> Effect CompileState
compileModule state modInfo = do
  content <- readTextFile UTF8 modInfo.path
  case parseModuleCst content of
    Left parseErr -> do
      log $ "FAIL: " <> modInfo.name <> " - Parse error"
      pure $ state { result = state.result { failed = state.result.failed + 1
                                           , errors = Array.snoc state.result.errors (modInfo.name <> ": " <> parseErr) } }
    Right m -> do
      let decls = Array.fromFoldable m.declarations
      let initialExports = extractExportsWithRegistry state.registry decls
      case checkModule state.registry emptyEnv decls of
        Left tcErr -> do
          let errMsg = showTCError tcErr
          log $ "FAIL: " <> modInfo.name <> " - " <> errMsg
          -- Still register with initial exports so dependent modules can try
          let newRegistry = registerModule state.registry modInfo.name initialExports
          pure $ { registry: newRegistry
                 , result: state.result { failed = state.result.failed + 1
                                        , errors = Array.snoc state.result.errors (modInfo.name <> ": " <> errMsg) } }
        Right env -> do
          log $ "OK: " <> modInfo.name
          let fullExports = addValuesToExports initialExports env decls
          -- When registering as "Prelude", merge with primitive operators from preludeExports
          let finalExports = if modInfo.name == "Prelude"
                               then mergeWithPrimitiveOps fullExports
                               else fullExports
          let newRegistry = registerModule state.registry modInfo.name finalExports
          pure $ { registry: newRegistry
                 , result: state.result { passed = state.result.passed + 1 } }

-- | Merge dynamically extracted exports with primitive operators from preludeExports
-- | This ensures primitive operators like *>, <$>, etc. remain available
mergeWithPrimitiveOps :: ModuleExports -> ModuleExports
mergeWithPrimitiveOps exports =
  exports { values = Map.union exports.values preludeExports.values }

-- | List .purs files in a directory
listPursFiles :: String -> Effect (Array String)
listPursFiles dir = do
  entries <- readdir dir
  pure $ Array.filter isPursFile entries
  where
    isPursFile name = String.contains (String.Pattern ".purs") name

-- | Build a module registry by typechecking modules
buildRegistry :: String -> ModuleRegistry -> Array String -> Effect ModuleRegistry
buildRegistry basePath initialRegistry files =
  foldM (addModuleToRegistry basePath) initialRegistry files

-- | Add a single module to the registry
addModuleToRegistry :: String -> ModuleRegistry -> String -> Effect ModuleRegistry
addModuleToRegistry basePath registry filename = do
  let path = basePath <> filename
  content <- readTextFile UTF8 path

  case parseModuleCst content of
    Left _ -> pure registry  -- Skip on parse error
    Right m -> do
      let decls = Array.fromFoldable m.declarations
      let moduleName = getModuleName basePath filename

      -- Extract initial exports (types, constructors, aliases)
      let initialExports = extractExportsWithRegistry registry decls

      -- Type check with current registry
      case checkModule registry emptyEnv decls of
        Left _ ->
          -- Even if typecheck fails, register what we can
          pure $ registerModule registry moduleName initialExports
        Right env -> do
          -- Add function types to exports
          let fullExports = addValuesToExports initialExports env decls
          pure $ registerModule registry moduleName fullExports

-- | Get module name from path
getModuleName :: String -> String -> String
getModuleName basePath filename =
  let name = String.replace (String.Pattern ".purs") (String.Replacement "") filename
  in case basePath of
    "lib/Data/" -> "Data." <> name
    "lib/Data/String/" -> "Data.String." <> name
    "lib/Control/" -> "Control." <> name
    "src/Nova/Compiler/" -> "Nova.Compiler." <> name
    _ -> name

-- | Module info with explicit path and name
type ModuleInfo = { path :: String, name :: String }

-- | Add a module to the registry using explicit path and name
addModuleToRegistryWithPath :: ModuleRegistry -> ModuleInfo -> Effect ModuleRegistry
addModuleToRegistryWithPath registry modInfo = do
  content <- readTextFile UTF8 modInfo.path
  case parseModuleCst content of
    Left _ -> do
      log $ "  Skip " <> modInfo.name <> " - Parse error"
      pure registry
    Right m -> do
      let decls = Array.fromFoldable m.declarations
      let initialExports = extractExportsWithRegistry registry decls
      case checkModule registry emptyEnv decls of
        Left _ -> do
          log $ "  Partial " <> modInfo.name <> " - Using initial exports only"
          pure $ registerModule registry modInfo.name initialExports
        Right env -> do
          log $ "  OK " <> modInfo.name
          let fullExports = addValuesToExports initialExports env decls
          pure $ registerModule registry modInfo.name fullExports

-- | Test a module with the registry
testModuleWithRegistry :: ModuleRegistry -> TestResult -> ModuleInfo -> Effect TestResult
testModuleWithRegistry registry result modInfo = do
  content <- readTextFile UTF8 modInfo.path
  case parseModuleCst content of
    Left parseErr -> do
      log $ "FAIL: " <> modInfo.name <> " - Parse error: " <> parseErr
      pure $ result { failed = result.failed + 1
                    , errors = Array.snoc result.errors (modInfo.name <> ": " <> parseErr) }
    Right m -> do
      let decls = Array.fromFoldable m.declarations
      let declCount = Array.length decls
      case checkModule registry emptyEnv decls of
        Right _env -> do
          log $ "PASS: " <> modInfo.name <> " (" <> show declCount <> " declarations)"
          pure $ result { passed = result.passed + 1 }
        Left tcErr -> do
          let errMsg = showTCError tcErr
          log $ "FAIL: " <> modInfo.name <> " - " <> errMsg
          pure $ result { failed = result.failed + 1
                        , errors = Array.snoc result.errors (modInfo.name <> ": " <> errMsg) }

-- | Test a single file with the registry for import resolution
testFileWithRegistry :: String -> ModuleRegistry -> TestResult -> String -> Effect TestResult
testFileWithRegistry basePath registry result filename = do
  let path = basePath <> filename
  content <- readTextFile UTF8 path

  case parseModuleCst content of
    Left parseErr -> do
      log $ "FAIL: " <> filename <> " - Parse error: " <> parseErr
      pure $ result { failed = result.failed + 1
                    , errors = Array.snoc result.errors (filename <> ": " <> parseErr) }

    Right m -> do
      let decls = Array.fromFoldable m.declarations
      let declCount = Array.length decls

      -- Type check the module with registry for import resolution
      case checkModule registry emptyEnv decls of
        Right _env -> do
          log $ "PASS: " <> filename <> " (" <> show declCount <> " declarations)"
          pure $ result { passed = result.passed + 1 }

        Left tcErr -> do
          let errMsg = showTCError tcErr
          log $ "FAIL: " <> filename <> " - " <> errMsg
          -- DEBUG: For specific failing files, print more info
          when (filename == "TypeChecker.purs" || filename == "Dependencies.purs") do
            log $ "  DEBUG: Checking function declarations one by one..."
            debugCheckFunctions registry decls
          pure $ result { failed = result.failed + 1
                        , errors = Array.snoc result.errors (filename <> ": " <> errMsg) }

-- | Debug helper: check each function one by one and report which fails
debugCheckFunctions :: ModuleRegistry -> Array Declaration -> Effect Unit
debugCheckFunctions registry decls = do
  let funcDecls = Array.filter isFunction decls
  void $ traverse (debugCheckOneFunction registry) funcDecls
  where
    isFunction (DeclFunction _) = true
    isFunction _ = false

debugCheckOneFunction :: ModuleRegistry -> Declaration -> Effect Unit
debugCheckOneFunction registry (DeclFunction func) = do
  -- Process imports first
  let allDecls = [DeclFunction func]
  case checkModule registry emptyEnv allDecls of
    Right _ -> pure unit -- log $ "    OK: " <> func.name
    Left err -> log $ "    FAIL in " <> func.name <> ": " <> show err
debugCheckOneFunction _ _ = pure unit

-- | Show a type checker error in a readable format
showTCError :: TCError -> String
showTCError err = show err
