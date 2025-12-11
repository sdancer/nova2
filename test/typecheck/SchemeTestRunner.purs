module Test.TypeCheck.SchemeTestRunner where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Array as Array
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.TypeChecker (checkModule, extractExports, addValuesToExports)
import Nova.Compiler.Types (emptyEnv, defaultRegistry, registerModule)

main :: Effect Unit
main = do
  log "=== Scheme Type Alias Test ==="

  -- Load the test file
  content <- readTextFile UTF8 "test/typecheck/SchemeTest.nova"
  log "Loaded SchemeTest.nova"

  case parseModuleCst content of
    Left err -> log $ "Parse error: " <> err
    Right m -> do
      let decls = Array.fromFoldable m.declarations
      log $ "Parsed " <> show (Array.length decls) <> " declarations"

      case checkModule defaultRegistry emptyEnv decls of
        Left err -> do
          log $ "FAIL: " <> show err
        Right env -> do
          log "PASS: SchemeTest.nova typechecked successfully"
