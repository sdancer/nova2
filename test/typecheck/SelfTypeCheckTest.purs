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
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir)
import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.Types (emptyEnv, ModuleRegistry, defaultRegistry, registerModule)
import Nova.Compiler.TypeChecker (checkModule, TCError, extractExports, addValuesToExports)

-- | Test result tracking
type TestResult = { passed :: Int, failed :: Int, errors :: Array String }

initResult :: TestResult
initResult = { passed: 0, failed: 0, errors: [] }

main :: Effect Unit
main = do
  log "=== Self Type-Check Test ==="
  log "Testing that all source files typecheck and unify properly"
  log ""

  -- Step 1: Build registry from lib modules
  log "--- Building Module Registry ---"
  log "Typechecking lib/Data/* modules..."
  libDataFiles <- listPursFiles "lib/Data/"
  registry1 <- buildRegistry "lib/Data/" defaultRegistry libDataFiles

  log "Typechecking lib/Data/String/* modules..."
  libDataStringFiles <- listPursFiles "lib/Data/String/"
  registry2 <- buildRegistry "lib/Data/String/" registry1 libDataStringFiles

  log "Typechecking lib/Control/* modules..."
  libControlFiles <- listPursFiles "lib/Control/"
  registry <- buildRegistry "lib/Control/" registry2 libControlFiles
  let libCount = Array.length libDataFiles + Array.length libDataStringFiles + Array.length libControlFiles
  log $ "Registry built with " <> show libCount <> " library modules"
  log ""

  -- Step 2: Test src/Nova/Compiler/ files with the registry
  log "--- Testing src/Nova/Compiler/ ---"
  srcFiles <- listPursFiles "src/Nova/Compiler/"
  log $ "Found " <> show (Array.length srcFiles) <> " files"

  -- Build registry with compiler modules too (for inter-module deps)
  fullRegistry <- buildRegistry "src/Nova/Compiler/" registry srcFiles

  -- Now test each file with the full registry
  result <- foldM (testFileWithRegistry "src/Nova/Compiler/" fullRegistry) initResult srcFiles

  log ""
  log "=== Summary ==="
  log $ "Passed: " <> show result.passed
  log $ "Failed: " <> show result.failed

  -- Show errors and fail if any
  when (result.failed > 0) do
    log ""
    log "Errors (first 10):"
    void $ traverse (\e -> log $ "  " <> e) (Array.take 10 result.errors)
    throw $ "Self type-check failed: " <> show result.failed <> " files failed"

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
      let initialExports = extractExports decls

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
          pure $ result { failed = result.failed + 1
                        , errors = Array.snoc result.errors (filename <> ": " <> errMsg) }

-- | Show a type checker error in a readable format
showTCError :: TCError -> String
showTCError err = show err
