module Test.Codegen.DebugFuncRef where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array as Array

-- Minimal test case to debug function reference issue

type Result a = { val :: a }

-- Function with 1 parameter
funcOne :: Int -> Result Int
funcOne x = { val: x }

-- Function with 2 parameters
funcTwo :: Int -> Int -> Result Int
funcTwo x y = { val: x + y }

-- This should generate &func_one/1 and &func_two/2 in the list
testList :: Array (Int -> Result Int)
testList = [funcOne]

-- Apply first function from list
testApply :: Int -> Maybe (Result Int)
testApply x = case Array.head testList of
  Nothing -> Nothing
  Just f -> Just (f x)
