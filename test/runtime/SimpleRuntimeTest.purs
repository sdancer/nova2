module Test.Runtime.SimpleRuntimeTest where

import Prelude
import Data.Array as Array
import Data.String as String

-- | Simple test that compiles cleanly through Nova
-- | Tests basic runtime operations inline

-- Run all tests and return true if all pass
runTests :: Boolean
runTests =
  Array.take 2 [1, 2, 3, 4] == [1, 2]
  && Array.drop 2 [1, 2, 3, 4] == [3, 4]
  && Array.reverse [1, 2, 3] == [3, 2, 1]
  && Array.concat [1, 2] [3, 4] == [1, 2, 3, 4]
  && Array.length [1, 2, 3, 4, 5] == 5
  && String.length "hello" == 5
  && String.take 3 "hello" == "hel"
  && String.drop 2 "hello" == "llo"
  && (true && true)
  && (false || true)
  && not false
