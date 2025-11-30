module Nova.Compiler.CodeGenWasm.Types where

import Prelude
import Data.Array (length, filter)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Partial.Unsafe (unsafePartial)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Nova.Compiler.Ast (Expr, FunctionDeclaration)

-- Lifted lambda info
type LiftedLambda =
  { id :: Int
  , expr :: Expr
  , params :: Array String
  , freeVars :: Array String
  , body :: Expr
  }

-- Code generation context
type WasmCtx =
  { moduleName :: String
  , moduleFuncs :: Set String
  , funcArities :: Map String Int
  , locals :: Map String Int
  , localCount :: Int
  , stringLiterals :: Array String
  , stringTable :: Map String { offset :: Int, len :: Int }
  , dataConstructors :: Map String { tag :: Int, arity :: Int }
  , funcTable :: Array String
  , lambdas :: Array LiftedLambda
  , lambdaCounter :: Int
  , funcWrapperIdx :: Map String Int
  , curryWrapperIdx :: Map String Int  -- Maps "__curry{stage}_{name}" to table index
  }

-- A group of function clauses with same name/arity
type FunctionGroup =
  { name :: String
  , arity :: Int
  , clauses :: Array FunctionDeclaration
  }

-- Lambda collector state
type LambdaCollector = { counter :: Int, lambdas :: Array LiftedLambda }

-- Well-known Prelude constructors (with their tags and arities)
preludeConstructors :: Map String { tag :: Int, arity :: Int }
preludeConstructors = Map.fromFoldable
  [ Tuple "Nothing" { tag: 0, arity: 0 }
  , Tuple "Just" { tag: 1, arity: 1 }
  , Tuple "Left" { tag: 0, arity: 1 }
  , Tuple "Right" { tag: 1, arity: 1 }
  , Tuple "Tuple" { tag: 0, arity: 2 }
  , Tuple "Nil" { tag: 0, arity: 0 }
  , Tuple "Cons" { tag: 1, arity: 2 }
  , Tuple "TokKeyword" { tag: 0, arity: 0 }
  , Tuple "TokIdentifier" { tag: 1, arity: 0 }
  , Tuple "TokNumber" { tag: 2, arity: 0 }
  , Tuple "TokString" { tag: 3, arity: 0 }
  , Tuple "TokChar" { tag: 4, arity: 0 }
  , Tuple "TokOperator" { tag: 5, arity: 0 }
  , Tuple "TokDelimiter" { tag: 6, arity: 0 }
  , Tuple "TokNewline" { tag: 7, arity: 0 }
  , Tuple "TokUnrecognized" { tag: 8, arity: 0 }
  ]

emptyCtx :: String -> WasmCtx
emptyCtx modName =
  { moduleName: modName
  , moduleFuncs: Set.empty
  , funcArities: Map.empty
  , locals: Map.empty
  , localCount: 0
  , stringLiterals: []
  , stringTable: Map.empty
  , dataConstructors: preludeConstructors
  , funcTable: []
  , lambdas: []
  , lambdaCounter: 0
  , funcWrapperIdx: Map.empty
  , curryWrapperIdx: Map.empty
  }

-- Unsafe array head/tail (will throw on empty array)
unsafeHead :: forall a. Array a -> a
unsafeHead arr = unsafePartial (Array.unsafeIndex arr 0)

unsafeTail :: forall a. Array a -> Array a
unsafeTail arr = fromMaybe [] (Array.tail arr)

-- Group function declarations by name and arity
groupFunctions :: Array FunctionDeclaration -> Array FunctionGroup
groupFunctions funcs =
  let keys = nubByNameArity (map (\f -> { name: f.name, arity: length f.parameters }) funcs)
      mkGroup k = { name: k.name, arity: k.arity, clauses: filter (\f -> f.name == k.name && length f.parameters == k.arity) funcs }
  in map mkGroup keys

nubByNameArity :: Array { name :: String, arity :: Int } -> Array { name :: String, arity :: Int }
nubByNameArity arr = nubByNameArityHelper [] arr

nubByNameArityHelper :: Array { name :: String, arity :: Int } -> Array { name :: String, arity :: Int } -> Array { name :: String, arity :: Int }
nubByNameArityHelper acc arr =
  case Array.uncons arr of
    Nothing -> acc
    Just { head: h, tail: t } ->
      let exists = anyNameArity h.name h.arity acc
      in if exists then nubByNameArityHelper acc t else nubByNameArityHelper (acc <> [h]) t

anyNameArity :: String -> Int -> Array { name :: String, arity :: Int } -> Boolean
anyNameArity name arity arr =
  case Array.uncons arr of
    Nothing -> false
    Just { head: h, tail: t } ->
      if h.name == name && h.arity == arity then true else anyNameArity name arity t
