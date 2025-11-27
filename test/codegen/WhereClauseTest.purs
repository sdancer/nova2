module Test.Codegen.WhereClauseTest where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array as Array

-- Test case: where clauses with multiple recursive functions
-- This tests patterns like the Parser.purs issues

-- Simple where clause
simpleWhere :: Int -> Int
simpleWhere x = helper x
  where
    helper n = n + 1

-- Recursive where clause
recursiveWhere :: Int -> Int
recursiveWhere x = countdown x
  where
    countdown n = if n <= 0 then 0 else countdown (n - 1)

-- Multiple bindings in where - these should generate in correct order
multipleWhere :: Int -> Int
multipleWhere x = first (second x)
  where
    first n = n * 2
    second n = n + 10

-- Where clause with dependencies between bindings
dependentWhere :: Int -> Int
dependentWhere x = outer x
  where
    inner n = n + 1
    outer n = inner n * inner n  -- outer depends on inner

-- Nested where clauses
nestedWhere :: Int -> Int
nestedWhere x = outer x
  where
    outer n = inner (n + 1)
      where
        inner m = m * 2

-- Where clause returning a function (closure)
closureWhere :: Int -> (Int -> Int)
closureWhere x = adder
  where
    adder y = x + y

-- Pattern matching in where clause
patternWhere :: Array Int -> Int
patternWhere arr = sumList arr
  where
    sumList xs = case Array.uncons xs of
      Nothing -> 0
      Just { head: h, tail: t } -> h + sumList t

-- Multiple mutually-dependent recursive functions in where
mutualWhere :: Int -> Int
mutualWhere n = isEvenResult n
  where
    isEvenResult x = if isEven x then 1 else 0
    isEven x = if x == 0 then true else isOdd (x - 1)
    isOdd x = if x == 0 then false else isEven (x - 1)
