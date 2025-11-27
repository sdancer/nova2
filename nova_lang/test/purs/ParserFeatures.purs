module Test.ParserFeatures where

-- Test various parser features

-- Type class
class Show a where
  show :: a -> String

-- Instance
instance showInt :: Show Int where
  show n = "Int"

-- Newtype
newtype UserId = UserId Int

-- Type alias with record
type Person = { name :: String, age :: Int }

-- Multi-parameter function with guards
clamp :: Int -> Int -> Int -> Int
clamp low high x
  | x < low = low
  | x > high = high
  | otherwise = x

-- Complex pattern matching
data Tree a = Leaf a | Branch (Tree a) (Tree a)

depth :: forall a. Tree a -> Int
depth tree = case tree of
  Leaf _ -> 1
  Branch l r ->
    let dl = depth l
        dr = depth r
    in 1 + (if dl > dr then dl else dr)

-- Let bindings
compute :: Int -> Int
compute x =
  let a = x + 1
      b = a * 2
      c = b - 3
  in c

-- Add function
add :: Int -> Int -> Int
add a b = a + b

-- Test entry point
runTests :: Int
runTests = clamp 0 10 5
