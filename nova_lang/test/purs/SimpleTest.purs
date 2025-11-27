module Test.SimpleTest where

-- A simple test that doesn't use Effect monad
-- Just defines some functions and values we can check

data TestResult = Pass | Fail String

-- Test arithmetic
testAdd :: Int -> Int -> Int
testAdd x y = x + y

-- Test pattern matching
testMaybe :: forall a. Maybe a -> a -> a
testMaybe Nothing def = def
testMaybe (Just x) _ = x

data Maybe a = Nothing | Just a

-- Test list operations
testList :: Array Int
testList = [1, 2, 3, 4, 5]

testHead :: Array Int -> Int
testHead xs = case xs of
  [] -> 0
  (x : _) -> x

-- Test record
testRecord :: { name :: String, value :: Int }
testRecord = { name: "test", value: 42 }

-- Test let binding
testLet :: Int
testLet =
  let x = 10
      y = 20
  in x + y

-- Test lambda
testLambda :: Int -> Int
testLambda = \x -> x * 2

-- Test if expression
testIf :: Boolean -> Int
testIf cond = if cond then 1 else 0

-- Test guards
testGuard :: Int -> String
testGuard n
  | n < 0 = "negative"
  | n == 0 = "zero"
  | otherwise = "positive"

-- Test function composition
compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- Entry point for test runner to call
runTests :: Int
runTests = testAdd 1 2
