module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.ImportProcessor.ImportProcessorTest as ImportProcessorTest

main :: Effect Unit
main = do
  log "=== Running All Tests ==="
  log ""
  ImportProcessorTest.main
  log ""
  log "=== All Tests Complete ==="
