module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.ImportProcessor.ImportProcessorTest as ImportProcessorTest
import Test.Cst.SelfParseTest as SelfParseTest
import Test.TypeCheck.SelfTypeCheckTest as SelfTypeCheckTest
import Test.TypeCheck.FailureReproTest as FailureReproTest

main :: Effect Unit
main = do
  log "=== Running All Tests ==="
  log ""
  ImportProcessorTest.main
  log ""
  SelfParseTest.main
  log ""
  SelfTypeCheckTest.main
  log ""
  FailureReproTest.runTests
  log ""
  log "=== All Tests Complete ==="
