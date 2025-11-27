module Test.Runtime.RuntimeTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl, foldr)
import Data.String as String

-- | Test structure: name, predicate
type Test = { name :: String, test :: Boolean }

-- | Run all tests and report results
main :: Effect Unit
main = do
  log "=== Nova Runtime Test Suite ==="
  log ""

  -- Run all test groups
  runTestGroup "Array.elem" arrayElemTests
  runTestGroup "Array.head/tail" arrayHeadTailTests
  runTestGroup "Array.uncons" arrayUnconsTests
  runTestGroup "Array.take/drop" arrayTakeDropTests
  runTestGroup "Array.filter" arrayFilterTests
  runTestGroup "Array.map" arrayMapTests
  runTestGroup "Array.foldl/foldr" arrayFoldTests
  runTestGroup "Array.find" arrayFindTests
  runTestGroup "Array.concat" arrayConcatTests
  runTestGroup "Array.reverse" arrayReverseTests
  runTestGroup "Array.span" arraySpanTests
  runTestGroup "Array.zipWith" arrayZipWithTests
  runTestGroup "Array.replicate" arrayReplicateTests
  runTestGroup "String.length" stringLengthTests
  runTestGroup "String.take/drop" stringTakeDropTests
  runTestGroup "String.contains" stringContainsTests
  runTestGroup "String.indexOf" stringIndexOfTests
  runTestGroup "String.split" stringSplitTests
  runTestGroup "String.stripPrefix" stringStripPrefixTests
  runTestGroup "String.replaceAll" stringReplaceAllTests
  runTestGroup "String.joinWith" stringJoinWithTests
  runTestGroup "Maybe operations" maybeTests
  runTestGroup "Either operations" eitherTests
  runTestGroup "Tuple operations" tupleTests
  runTestGroup "Basic prelude" preludeTests

  log ""
  log "=== All tests complete ==="

runTestGroup :: String -> Array Test -> Effect Unit
runTestGroup groupName tests = do
  log $ "Testing " <> groupName <> "..."
  let results = map runTest tests
  let passed = Array.length (Array.filter (\r -> r.passed) results)
  let total = Array.length results
  if passed == total
    then log $ "  ✓ All " <> show total <> " tests passed"
    else do
      log $ "  ✗ " <> show passed <> "/" <> show total <> " tests passed"
      -- Show failed tests
      let failed = Array.filter (\r -> not r.passed) results
      traverse_ (\r -> log $ "    FAILED: " <> r.name) failed
  where
    traverse_ f xs = foldl (\_ x -> f x) (pure unit) xs

runTest :: Test -> { name :: String, passed :: Boolean }
runTest t = { name: t.name, passed: t.test }

-- ============================================================
-- Array tests
-- ============================================================

arrayElemTests :: Array Test
arrayElemTests =
  [ { name: "elem finds existing element", test: Array.elem 2 [1, 2, 3] }
  , { name: "elem returns false for missing", test: not (Array.elem 4 [1, 2, 3]) }
  , { name: "elem works on empty array", test: not (Array.elem 1 []) }
  ]

arrayHeadTailTests :: Array Test
arrayHeadTailTests =
  [ { name: "head of non-empty", test: Array.head [1, 2, 3] == Just 1 }
  , { name: "head of empty", test: Array.head ([] :: Array Int) == Nothing }
  , { name: "tail of non-empty", test: Array.tail [1, 2, 3] == Just [2, 3] }
  , { name: "tail of empty", test: Array.tail ([] :: Array Int) == Nothing }
  , { name: "last of non-empty", test: Array.last [1, 2, 3] == Just 3 }
  , { name: "last of empty", test: Array.last ([] :: Array Int) == Nothing }
  ]

arrayUnconsTests :: Array Test
arrayUnconsTests =
  [ { name: "uncons non-empty", test: Array.uncons [1, 2, 3] == Just { head: 1, tail: [2, 3] } }
  , { name: "uncons empty", test: Array.uncons ([] :: Array Int) == Nothing }
  , { name: "uncons singleton", test: Array.uncons [42] == Just { head: 42, tail: [] } }
  ]

arrayTakeDropTests :: Array Test
arrayTakeDropTests =
  [ { name: "take 2", test: Array.take 2 [1, 2, 3, 4] == [1, 2] }
  , { name: "take 0", test: Array.take 0 [1, 2, 3] == [] }
  , { name: "take more than length", test: Array.take 10 [1, 2] == [1, 2] }
  , { name: "drop 2", test: Array.drop 2 [1, 2, 3, 4] == [3, 4] }
  , { name: "drop 0", test: Array.drop 0 [1, 2, 3] == [1, 2, 3] }
  , { name: "drop more than length", test: Array.drop 10 [1, 2] == [] }
  , { name: "takeWhile", test: Array.takeWhile (\x -> x < 3) [1, 2, 3, 4] == [1, 2] }
  , { name: "dropWhile", test: Array.dropWhile (\x -> x < 3) [1, 2, 3, 4] == [3, 4] }
  ]

arrayFilterTests :: Array Test
arrayFilterTests =
  [ { name: "filter even", test: Array.filter (\x -> x `mod` 2 == 0) [1, 2, 3, 4, 5, 6] == [2, 4, 6] }
  , { name: "filter all", test: Array.filter (\_ -> true) [1, 2, 3] == [1, 2, 3] }
  , { name: "filter none", test: Array.filter (\_ -> false) [1, 2, 3] == [] }
  ]

arrayMapTests :: Array Test
arrayMapTests =
  [ { name: "map double", test: map (\x -> x * 2) [1, 2, 3] == [2, 4, 6] }
  , { name: "map empty", test: map (\x -> x * 2) ([] :: Array Int) == [] }
  , { name: "mapWithIndex", test: Array.mapWithIndex (\i x -> i + x) [10, 20, 30] == [10, 21, 32] }
  ]

arrayFoldTests :: Array Test
arrayFoldTests =
  [ { name: "foldl sum", test: foldl (+) 0 [1, 2, 3, 4] == 10 }
  , { name: "foldl string", test: foldl (\acc x -> acc <> show x) "" [1, 2, 3] == "123" }
  , { name: "foldr sum", test: foldr (+) 0 [1, 2, 3, 4] == 10 }
  , { name: "foldr string", test: foldr (\x acc -> show x <> acc) "" [1, 2, 3] == "123" }
  ]

arrayFindTests :: Array Test
arrayFindTests =
  [ { name: "find existing", test: Array.find (\x -> x > 2) [1, 2, 3, 4] == Just 3 }
  , { name: "find missing", test: Array.find (\x -> x > 10) [1, 2, 3] == Nothing }
  , { name: "findIndex existing", test: Array.findIndex (\x -> x > 2) [1, 2, 3, 4] == Just 2 }
  , { name: "findIndex missing", test: Array.findIndex (\x -> x > 10) [1, 2, 3] == Nothing }
  ]

arrayConcatTests :: Array Test
arrayConcatTests =
  [ { name: "concat two arrays", test: [1, 2] <> [3, 4] == [1, 2, 3, 4] }
  , { name: "concat with empty left", test: [] <> [1, 2] == [1, 2] }
  , { name: "concat with empty right", test: [1, 2] <> [] == [1, 2] }
  , { name: "snoc", test: Array.snoc [1, 2, 3] 4 == [1, 2, 3, 4] }
  , { name: "cons", test: Array.cons 0 [1, 2, 3] == [0, 1, 2, 3] }
  ]

arrayReverseTests :: Array Test
arrayReverseTests =
  [ { name: "reverse", test: Array.reverse [1, 2, 3] == [3, 2, 1] }
  , { name: "reverse empty", test: Array.reverse ([] :: Array Int) == [] }
  , { name: "reverse singleton", test: Array.reverse [1] == [1] }
  ]

arraySpanTests :: Array Test
arraySpanTests =
  [ { name: "span", test:
      let r = Array.span (\x -> x < 3) [1, 2, 3, 4, 5]
      in r.init == [1, 2] && r.rest == [3, 4, 5]
    }
  , { name: "span all pass", test:
      let r = Array.span (\x -> x < 10) [1, 2, 3]
      in r.init == [1, 2, 3] && r.rest == []
    }
  , { name: "span none pass", test:
      let r = Array.span (\x -> x < 0) [1, 2, 3]
      in r.init == [] && r.rest == [1, 2, 3]
    }
  ]

arrayZipWithTests :: Array Test
arrayZipWithTests =
  [ { name: "zipWith add", test: Array.zipWith (+) [1, 2, 3] [10, 20, 30] == [11, 22, 33] }
  , { name: "zipWith different lengths", test: Array.zipWith (+) [1, 2, 3] [10, 20] == [11, 22] }
  , { name: "zip", test: Array.zip [1, 2] ["a", "b"] == [Tuple 1 "a", Tuple 2 "b"] }
  ]

arrayReplicateTests :: Array Test
arrayReplicateTests =
  [ { name: "replicate", test: Array.replicate 3 "x" == ["x", "x", "x"] }
  , { name: "replicate 0", test: Array.replicate 0 "x" == [] }
  , { name: "range", test: Array.range 1 5 == [1, 2, 3, 4, 5] }
  ]

-- ============================================================
-- String tests
-- ============================================================

stringLengthTests :: Array Test
stringLengthTests =
  [ { name: "length non-empty", test: String.length "hello" == 5 }
  , { name: "length empty", test: String.length "" == 0 }
  ]

stringTakeDropTests :: Array Test
stringTakeDropTests =
  [ { name: "take", test: String.take 3 "hello" == "hel" }
  , { name: "drop", test: String.drop 2 "hello" == "llo" }
  , { name: "take more than length", test: String.take 10 "hi" == "hi" }
  , { name: "drop more than length", test: String.drop 10 "hi" == "" }
  ]

stringContainsTests :: Array Test
stringContainsTests =
  [ { name: "contains true", test: String.contains (String.Pattern "ell") "hello" }
  , { name: "contains false", test: not (String.contains (String.Pattern "xyz") "hello") }
  , { name: "contains empty pattern", test: String.contains (String.Pattern "") "hello" }
  ]

stringIndexOfTests :: Array Test
stringIndexOfTests =
  [ { name: "indexOf found", test: String.indexOf (String.Pattern "ll") "hello" == Just 2 }
  , { name: "indexOf not found", test: String.indexOf (String.Pattern "xyz") "hello" == Nothing }
  , { name: "lastIndexOf", test: String.lastIndexOf (String.Pattern "l") "hello" == Just 3 }
  ]

stringSplitTests :: Array Test
stringSplitTests =
  [ { name: "split", test: String.split (String.Pattern ",") "a,b,c" == ["a", "b", "c"] }
  , { name: "split no match", test: String.split (String.Pattern ",") "abc" == ["abc"] }
  ]

stringStripPrefixTests :: Array Test
stringStripPrefixTests =
  [ { name: "stripPrefix match", test: String.stripPrefix (String.Pattern "hel") "hello" == Just "lo" }
  , { name: "stripPrefix no match", test: String.stripPrefix (String.Pattern "xyz") "hello" == Nothing }
  ]

stringReplaceAllTests :: Array Test
stringReplaceAllTests =
  [ { name: "replaceAll", test: String.replaceAll (String.Pattern "l") (String.Replacement "L") "hello" == "heLLo" }
  , { name: "replaceAll no match", test: String.replaceAll (String.Pattern "x") (String.Replacement "y") "hello" == "hello" }
  ]

stringJoinWithTests :: Array Test
stringJoinWithTests =
  [ { name: "joinWith", test: String.joinWith ", " ["a", "b", "c"] == "a, b, c" }
  , { name: "joinWith empty array", test: String.joinWith ", " [] == "" }
  , { name: "joinWith singleton", test: String.joinWith ", " ["a"] == "a" }
  ]

-- ============================================================
-- Maybe tests
-- ============================================================

maybeTests :: Array Test
maybeTests =
  [ { name: "Just value", test: Just 42 == Just 42 }
  , { name: "Nothing", test: Nothing == (Nothing :: Maybe Int) }
  , { name: "map Just", test: map (\x -> x * 2) (Just 21) == Just 42 }
  , { name: "map Nothing", test: map (\x -> x * 2) Nothing == (Nothing :: Maybe Int) }
  , { name: "fromMaybe Just", test: fromMaybe 0 (Just 42) == 42 }
  , { name: "fromMaybe Nothing", test: fromMaybe 0 Nothing == 0 }
  , { name: "isJust true", test: isJust (Just 42) }
  , { name: "isJust false", test: not (isJust (Nothing :: Maybe Int)) }
  , { name: "isNothing true", test: isNothing (Nothing :: Maybe Int) }
  , { name: "isNothing false", test: not (isNothing (Just 42)) }
  ]
  where
    fromMaybe def m = case m of
      Just x -> x
      Nothing -> def
    isJust m = case m of
      Just _ -> true
      Nothing -> false
    isNothing m = not (isJust m)

-- ============================================================
-- Either tests
-- ============================================================

eitherTests :: Array Test
eitherTests =
  [ { name: "Right value", test: Right 42 == (Right 42 :: Either String Int) }
  , { name: "Left value", test: Left "error" == (Left "error" :: Either String Int) }
  , { name: "map Right", test: map (\x -> x * 2) (Right 21 :: Either String Int) == Right 42 }
  , { name: "map Left", test: map (\x -> x * 2) (Left "err" :: Either String Int) == Left "err" }
  , { name: "either Right", test: either (\_ -> 0) (\x -> x) (Right 42 :: Either String Int) == 42 }
  , { name: "either Left", test: either (\_ -> 0) (\x -> x) (Left "err" :: Either String Int) == 0 }
  ]
  where
    either f g e = case e of
      Left x -> f x
      Right x -> g x

-- ============================================================
-- Tuple tests
-- ============================================================

tupleTests :: Array Test
tupleTests =
  [ { name: "Tuple creation", test: Tuple 1 "a" == Tuple 1 "a" }
  , { name: "fst", test: fst (Tuple 1 "a") == 1 }
  , { name: "snd", test: snd (Tuple 1 "a") == "a" }
  ]
  where
    fst (Tuple a _) = a
    snd (Tuple _ b) = b

-- ============================================================
-- Basic prelude tests
-- ============================================================

preludeTests :: Array Test
preludeTests =
  [ { name: "identity", test: identity 42 == 42 }
  , { name: "const", test: const 42 "ignored" == 42 }
  , { name: "compose", test: ((\x -> x * 2) <<< (\x -> x + 1)) 5 == 12 }
  , { name: "flip compose", test: ((\x -> x + 1) >>> (\x -> x * 2)) 5 == 12 }
  , { name: "show int", test: show 42 == "42" }
  , { name: "show string", test: show "hello" == "\"hello\"" }
  , { name: "show bool", test: show true == "true" }
  ]
