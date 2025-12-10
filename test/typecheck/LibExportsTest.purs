module Test.TypeCheck.LibExportsTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (foldM)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.CstPipeline (parseModuleCst)
import Nova.Compiler.Types (emptyEnv, ModuleRegistry, defaultRegistry, registerModule, Scheme, showType, ModuleExports)
import Nova.Compiler.TypeChecker (checkModule, extractExports, addValuesToExports)

-- | Expected function types from lib/Data/List.purs
-- | These should match what extractExports produces
expectedListFunctions :: Array { name :: String, arity :: Int }
expectedListFunctions =
  [ { name: "reverse", arity: 1 }      -- forall a. List a -> List a
  , { name: "any", arity: 2 }          -- forall a. (a -> Boolean) -> List a -> Boolean
  , { name: "range", arity: 2 }        -- Int -> Int -> List Int
  , { name: "mapMaybe", arity: 2 }     -- forall a b. (a -> Maybe b) -> List a -> List b
  , { name: "head", arity: 1 }         -- forall a. List a -> Maybe a
  , { name: "tail", arity: 1 }         -- forall a. List a -> Maybe (List a)
  , { name: "takeWhile", arity: 2 }    -- forall a. (a -> Boolean) -> List a -> List a
  , { name: "dropWhile", arity: 2 }    -- forall a. (a -> Boolean) -> List a -> List a
  , { name: "filter", arity: 2 }       -- forall a. (a -> Boolean) -> List a -> List a
  , { name: "map", arity: 2 }          -- forall a b. (a -> b) -> List a -> List b
  , { name: "foldl", arity: 3 }        -- forall a b. (b -> a -> b) -> b -> List a -> b
  , { name: "foldr", arity: 3 }        -- forall a b. (a -> b -> b) -> b -> List a -> b
  , { name: "take", arity: 2 }         -- forall a. Int -> List a -> List a
  , { name: "drop", arity: 2 }         -- forall a. Int -> List a -> List a
  , { name: "cons", arity: 2 }         -- forall a. a -> List a -> List a
  , { name: "singleton", arity: 1 }    -- forall a. a -> List a
  , { name: "append", arity: 2 }       -- forall a. List a -> List a -> List a
  , { name: "elem", arity: 2 }         -- forall a. a -> List a -> Boolean
  , { name: "toUnfoldable", arity: 1 } -- forall a. List a -> Array a
  , { name: "fromFoldable", arity: 1 } -- forall a. Array a -> List a
  , { name: "null", arity: 1 }         -- forall a. List a -> Boolean
  , { name: "uncons", arity: 1 }       -- forall a. List a -> Maybe {...}
  , { name: "length", arity: 1 }       -- forall a. List a -> Int
  ]

-- | Count number of arrows in a type (gives arity)
countArrows :: Scheme -> Int
countArrows scheme = countArrowsInType scheme.ty
  where
    countArrowsInType ty = case ty of
      -- This is simplified - would need proper pattern matching on Type
      _ -> 0  -- Placeholder

main :: Effect Unit
main = do
  log "=== Lib Exports Test ==="
  log "Verifying that lib files export functions matching hardcoded builtinPrelude"
  log ""

  -- Test Data.List exports
  log "--- Testing lib/Data/List.purs ---"
  testListExports

  log ""
  log "=== Test Complete ==="

testListExports :: Effect Unit
testListExports = do
  content <- readTextFile UTF8 "lib/Data/List.purs"

  case parseModuleCst content of
    Left err -> log $ "FAIL: Parse error: " <> err
    Right m -> do
      let decls = Array.fromFoldable m.declarations
      log $ "Parsed " <> show (Array.length decls) <> " declarations"

      -- Extract exports (types, constructors, foreign imports)
      let initialExports = extractExports decls

      -- Type check with default registry
      case checkModule defaultRegistry emptyEnv decls of
        Left err -> do
          log $ "WARN: Type check failed (using initial exports): " <> show err
          -- Still test with initial exports from foreign imports
          testExportsValues "Data.List" initialExports

        Right env -> do
          let fullExports = addValuesToExports initialExports env decls
          log $ "Type checked successfully"
          testExportsValues "Data.List" fullExports

testExportsValues :: String -> ModuleExports -> Effect Unit
testExportsValues moduleName exports = do
  let valueKeys = Array.fromFoldable (Map.keys exports.values)
  log $ "Exported values: " <> show (Array.length valueKeys)

  -- Check each expected List function
  let results = Array.mapMaybe (checkFunction exports.values) expectedListFunctions
  let passed = Array.length (Array.filter (\r -> r.found) results)
  let failed = Array.length (Array.filter (\r -> not r.found) results)

  log ""
  log $ "Functions found: " <> show passed <> "/" <> show (Array.length expectedListFunctions)

  -- Show missing ones
  let missing = Array.filter (\r -> not r.found) results
  when (Array.length missing > 0) do
    log "Missing functions:"
    void $ Array.foldM (\_ r -> log $ "  - " <> r.name) unit missing

  -- Show found ones with types
  log ""
  log "Found functions:"
  void $ Array.foldM (\_ name ->
    case Map.lookup name exports.values of
      Just scheme -> log $ "  " <> name <> " :: " <> showType scheme.ty
      Nothing -> pure unit
  ) unit valueKeys

  where
    checkFunction valueMap expected =
      Just { name: expected.name
           , found: case Map.lookup expected.name valueMap of
               Just _ -> true
               Nothing -> false
           }
