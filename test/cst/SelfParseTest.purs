module Test.Cst.SelfParseTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Array as Array
import Data.Foldable (foldM)
import Data.String as String
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir)
import Nova.Compiler.CstPipeline (parseModuleCst)

main :: Effect Unit
main = do
  log "=== CST Self-Parse Test ==="
  log ""

  -- Test src/Nova/Compiler/
  log "--- src/Nova/Compiler/ ---"
  srcFiles <- readdir "src/Nova/Compiler"
  let srcPurs = Array.filter (String.contains (String.Pattern ".purs")) srcFiles
  log $ "Found " <> show (Array.length srcPurs) <> " files\n"
  srcResults <- foldM (parseAndReport "src/Nova/Compiler/") initResult srcPurs

  -- Test lib/Data/
  log "\n--- lib/Data/ ---"
  libFiles <- readdir "lib/Data"
  let libPurs = Array.filter (String.contains (String.Pattern ".purs")) libFiles
  log $ "Found " <> show (Array.length libPurs) <> " files\n"
  results <- foldM (parseAndReport "lib/Data/") srcResults libPurs

  -- Summary
  log "\n=== Summary ==="
  log $ "Passed: " <> show results.passed
  log $ "Failed: " <> show results.failed

  -- Show failures if any
  when (results.failed > 0) do
    log "\nFailures:"
    void $ foldM (\_ f -> log $ "  - " <> f) unit results.failures

type TestResult = { passed :: Int, failed :: Int, failures :: Array String }

initResult :: TestResult
initResult = { passed: 0, failed: 0, failures: [] }

parseAndReport :: String -> TestResult -> String -> Effect TestResult
parseAndReport basePath acc filename = do
  let path = basePath <> filename
  content <- readTextFile UTF8 path
  case parseModuleCst content of
    Left err -> do
      log $ "FAIL: " <> filename
      log $ "      " <> String.take 80 err
      pure { passed: acc.passed
           , failed: acc.failed + 1
           , failures: Array.snoc acc.failures filename
           }
    Right mod -> do
      let declCount = Array.length (Array.fromFoldable mod.declarations)
      log $ "PASS: " <> filename <> " (" <> show declCount <> " declarations)"
      pure { passed: acc.passed + 1
           , failed: acc.failed
           , failures: acc.failures
           }
