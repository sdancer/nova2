-- | Failure Reproduction Tests
-- |
-- | These tests attempt to reproduce the 6 self-type-check failures in minimal form.
-- |
-- | KEY FINDING: Single-file tests pass, but cross-module type resolution fails.
-- | The actual failures occur when:
-- | 1. Types are imported from another module (e.g., Declaration from Ast.purs)
-- | 2. Qualified imports aren't resolved (e.g., Set.empty, Array.foldl)
-- |
-- | The type checker works correctly for same-file definitions.
module Test.TypeCheck.FailureReproTest where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Array as Array
import Data.Traversable (traverse)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.TypeChecker (checkModule, extractExports, addValuesToExports)
import Nova.Compiler.Types (emptyEnv, emptyRegistry)

type TestResult = { name :: String, passed :: Boolean, error :: String }
type TestCase = { name :: String, path :: String, shouldFail :: Boolean, issue :: String }
type RunResult = { passed :: Boolean, shouldFail :: Boolean }

-- | Test a single file
testFile :: String -> String -> Effect TestResult
testFile name path = do
  content <- readTextFile UTF8 path
  case parseModuleCst content of
    Left parseErr ->
      pure { name, passed: false, error: "Parse error: " <> parseErr }
    Right cstMod -> do
      let decls = Array.fromFoldable cstMod.declarations
      let initialExports = extractExports decls
      case checkModule emptyRegistry emptyEnv decls of
        Left tcErr ->
          pure { name, passed: false, error: "Type error: " <> show tcErr }
        Right env -> do
          let _ = addValuesToExports initialExports env decls
          pure { name, passed: true, error: "" }

-- | Test cases
-- |
-- | The actual failures in self-type-check are:
-- | - CodeGenCoreErlang: Array vs String in genModule
-- | - CstParser: Record vs LetBinding ADT
-- | - CstPipeline: CstParser.runParser qualified import
-- | - CstToAst: Number vs Int in convertBinder
-- | - Dependencies: String vs Declaration in getDependencies
-- | - TypeChecker: {ty,vars} vs Set in collectTypeNames
-- |
-- | These cannot be reproduced in single-file tests because they require
-- | cross-module type/import resolution which the test harness doesn't provide.
testCases :: Array TestCase
testCases =
  [ { name: "Qualified Import (fails)"
    , path: "test/typecheck/failures/QualifiedImportTest.nova"
    , shouldFail: true
    , issue: "Qualified imports not resolved without registry"
    }
  , { name: "ADT Pattern with Record Payload (passes)"
    , path: "test/typecheck/failures/DeclarationPatternTest.nova"
    , shouldFail: false
    , issue: "Same-file ADT pattern matching works"
    }
  , { name: "Type ADT Pattern (passes)"
    , path: "test/typecheck/failures/TypeAdtPatternTest.nova"
    , shouldFail: false
    , issue: "Same-file type alias in ADT works"
    }
  , { name: "Type Alias in ADT (passes)"
    , path: "test/typecheck/failures/TypeAliasInAdtTest.nova"
    , shouldFail: false
    , issue: "Same-file recursive type alias works"
    }
  ]

runOne :: TestCase -> Effect RunResult
runOne tc = do
  result <- testFile tc.name tc.path
  let status = if result.passed
               then if tc.shouldFail then "UNEXPECTED PASS" else "PASS"
               else if tc.shouldFail then "REPRODUCED" else "UNEXPECTED FAIL"
  log $ status <> ": " <> tc.name
  when (not result.passed) do
    log $ "  Issue: " <> tc.issue
    log $ "  Error: " <> result.error
  pure { passed: result.passed, shouldFail: tc.shouldFail }

main :: Effect Unit
main = runTests

runTests :: Effect Unit
runTests = do
  log "=== Failure Reproduction Tests ==="
  log "Testing patterns from self-type-check failures\n"
  log "NOTE: These tests use emptyRegistry, so qualified imports fail."
  log "The actual failures in self-type-check are due to cross-module"
  log "type resolution issues that can't be reproduced in single files.\n"

  results <- traverse runOne testCases

  log "\n=== Summary ==="
  let failedAsExpected = Array.length (Array.filter (\r -> not r.passed && r.shouldFail) results)
  let passedUnexpectedly = Array.length (Array.filter (\r -> r.passed && r.shouldFail) results)
  let passedAsExpected = Array.length (Array.filter (\r -> r.passed && not r.shouldFail) results)
  let failedUnexpectedly = Array.length (Array.filter (\r -> not r.passed && not r.shouldFail) results)

  log $ "Expected failures (qualified imports): " <> show failedAsExpected
  log $ "Expected passes (same-file patterns): " <> show passedAsExpected
  log $ "Unexpected: " <> show (passedUnexpectedly + failedUnexpectedly)
