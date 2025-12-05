module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Parser.ParserTest as ParserTest
import Test.Parser.TestParenCase as TestParenCase
import Test.Parser.TestAsPattern as TestAsPattern
import Test.Parser.ConstructorPatternTest as ConstructorPatternTest

main :: Effect Unit
main = do
  log "=== Running All Tests ==="
  log ""
  ParserTest.main
  log ""
  TestParenCase.main
  log ""
  TestAsPattern.main
  log ""
  ConstructorPatternTest.main
  log ""
  log "=== All Tests Complete ==="
