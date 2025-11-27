module Test.Codegen.FuncRefTest where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array

-- Test case: Function references in lists
-- This should generate function references like &parse_a/1, not parse_a()

type Result a = Either String a

parseA :: String -> Result Int
parseA s = Right 1

parseB :: String -> Result Int
parseB s = Right 2

parseC :: String -> Result Int
parseC s = Right 3

-- parseAny takes a list of parsers (functions) and tries each one
parseAny :: forall a. Array (String -> Result a) -> String -> Result a
parseAny parsers input = go parsers
  where
    go ps = case Array.head ps of
      Nothing -> Left "No parser succeeded"
      Just p -> case p input of
        Right result -> Right result
        Left _ -> go (Array.drop 1 ps)

-- This is the problematic pattern: passing functions as list elements
parseAll :: String -> Result Int
parseAll input = parseAny [parseA, parseB, parseC] input

-- Another test: higher-order functions
applyAll :: forall a b. Array (a -> b) -> a -> Array b
applyAll funcs arg = map (\f -> f arg) funcs

testApply :: Array Int
testApply = applyAll [increment, double, negate] 5
  where
    increment x = x + 1
    double x = x * 2
    negate x = 0 - x
