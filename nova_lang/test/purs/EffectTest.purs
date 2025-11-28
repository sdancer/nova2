module Test.EffectTest where

-- Test Effect monad do-notation
-- Effect is just a thunk: fn -> result end

-- Make an Effect (wraps a thunk)
mkEffect :: forall a. (Int -> a) -> Effect a
mkEffect f = Effect f

-- Effect represented as tagged tuple
data Effect a = Effect (Int -> a)

-- Pure wraps a value in Effect (ignore the unused argument)
pureEff :: forall a. a -> Effect a
pureEff x = Effect (\_ -> x)

-- Bind sequences Effect computations (named 'bind' for do-notation)
bind :: forall a b. Effect a -> (a -> Effect b) -> Effect b
bind (Effect f) g = Effect (\u ->
  let a = f u
      Effect h = g a
  in h u)

-- Run an Effect (for testing)
runEff :: forall a. Effect a -> a
runEff (Effect f) = f 0

-- Test do-notation with explicit bind
testDoManual :: Effect Int
testDoManual =
  bind (pureEff 10) (\x ->
    bind (pureEff 20) (\y ->
      pureEff (x + y)))

-- Test actual do-notation (should desugar to bind)
testDoSyntax :: Effect Int
testDoSyntax = do
  x <- pureEff 10
  y <- pureEff 20
  pureEff (x + y)

-- Entry point - test both manual and do-syntax versions
runTests :: Int
runTests = runEff testDoSyntax

-- Main function for test runner - returns Effect for compatibility
main :: Effect Int
main = testDoSyntax
