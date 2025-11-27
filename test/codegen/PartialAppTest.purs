module Test.Codegen.PartialAppTest where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Foldable (foldM)

-- Test case: Partial application of functions passed to foldM
-- This tests the unifyField pattern from Unify.purs

-- Simulate the unify_field pattern:
-- unify_field takes 4 args but is partially applied with 2 in foldM

type Map = Array { key :: String, val :: Int }

lookupMap :: String -> Map -> Maybe Int
lookupMap k m = case go m of
  Just r -> Just r.val
  Nothing -> Nothing
  where
    go arr = case Array.uncons arr of
      Nothing -> Nothing
      Just { head: x, tail: xs } -> if x.key == k then Just x else go xs

-- This function has 4 parameters
unifyField :: Map -> Map -> Int -> String -> Either String Int
unifyField fields1 fields2 acc k =
  case { l: lookupMap k fields1, r: lookupMap k fields2 } of
    { l: Just v1, r: Just v2 } -> Right (acc + v1 + v2)
    _ -> Right acc

-- When passed to foldM, unify_field is partially applied with 2 args
-- foldM expects: (acc -> elem -> Either err acc)
-- So unify_field fields1 fields2 should produce a function: acc -> k -> Either String Int
unifyMaps :: Map -> Map -> Array String -> Either String Int
unifyMaps m1 m2 keys = foldM (unifyField m1 m2) 0 keys

-- Another partial application test
addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c

-- Partial apply addThree with 1 arg
addToFive :: Int -> Int -> Int
addToFive = addThree 5

-- Partial apply with 2 args
addTenAndFive :: Int -> Int
addTenAndFive = addThree 10 5

-- Test case for let-bound recursive functions with where clause
collectArgs :: forall a. Array a -> { first :: Maybe a, rest :: Array a }
collectArgs arr = go arr { first: Nothing, rest: [] }
  where
    go :: Array a -> { first :: Maybe a, rest :: Array a } -> { first :: Maybe a, rest :: Array a }
    go xs acc = case Array.uncons xs of
      Nothing -> acc
      Just { head: x, tail: rest } ->
        case acc.first of
          Nothing -> go rest { first: Just x, rest: acc.rest }
          Just _ -> go rest { first: acc.first, rest: acc.rest <> [x] }
