module Nova.Compiler.CodeGenWasmSimple where

import Prelude
import Data.Array (intercalate, length, (:))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.String as String

import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (foldr, foldl)
import Data.List (List(..))
import Data.List as List
import Data.Int as Int
import Data.Enum (fromEnum)
import Nova.Compiler.Ast (Module, Declaration(..), FunctionDeclaration, Expr(..), Pattern(..), Literal(..), LetBind, CaseClause, DoStatement(..))

-- WebAssembly Text Format (WAT) Code Generation
-- Generates runnable WASM with JS runtime imports

-- Value representation (tagged pointers):
-- Bits [1:0] = tag:
--   00 = Integer (value in bits [31:2])
--   01 = Boolean (value in bit [2])
--   10 = Heap pointer (address in bits [31:2])
--   11 = Constructor tag (ctor index in [15:2], arity in [31:16])

-- Lifted lambda info
type LiftedLambda =
  { id :: Int
  , expr :: Expr                 -- the full ExprLambda (for reference equality matching)
  , params :: Array String       -- lambda parameters
  , freeVars :: Array String     -- captured variables from enclosing scope
  , body :: Expr
  }

-- Code generation context
type WasmCtx =
  { moduleName :: String
  , moduleFuncs :: Set String
  , funcArities :: Map String Int  -- function name -> arity
  , locals :: Map String Int  -- var name -> local index
  , localCount :: Int
  , stringLiterals :: Array String
  , stringTable :: Map String { offset :: Int, len :: Int }  -- string -> data segment location
  , dataConstructors :: Map String { tag :: Int, arity :: Int }
  , funcTable :: Array String  -- functions for indirect calls
  , lambdas :: Array LiftedLambda  -- lifted lambdas
  , lambdaCounter :: Int  -- for generating unique IDs
  , funcWrapperIdx :: Map String Int  -- function name -> wrapper table index (for function-as-value)
  }

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
  }

-- | Well-known Prelude constructors (with their tags and arities)
preludeConstructors :: Map String { tag :: Int, arity :: Int }
preludeConstructors = Map.fromFoldable
  [ Tuple "Nothing" { tag: 0, arity: 0 }
  , Tuple "Just" { tag: 1, arity: 1 }
  , Tuple "Left" { tag: 0, arity: 1 }
  , Tuple "Right" { tag: 1, arity: 1 }
  , Tuple "Tuple" { tag: 0, arity: 2 }
  , Tuple "Nil" { tag: 0, arity: 0 }
  , Tuple "Cons" { tag: 1, arity: 2 }
  -- TokenType constructors (from Tokenizer, imported by Parser)
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

-- | Mangle a name for WASM identifier
mangleName :: String -> String
mangleName name =
  let escaped = String.replaceAll (String.Pattern "'") (String.Replacement "_prime_") name
      escaped2 = String.replaceAll (String.Pattern ".") (String.Replacement "_") escaped
  in "$" <> escaped2

-- | Helper for collectExternalRefs - handles do statements
collectDoStmtRefs :: DoStatement -> Array (Tuple String String)
collectDoStmtRefs (DoExpr e) = collectExternalRefs e
collectDoStmtRefs (DoBind _ e) = collectExternalRefs e
collectDoStmtRefs (DoLet binds) = listConcatMap (\b -> collectExternalRefs b.value) binds

-- | Collect all external module references from expression
collectExternalRefs :: Expr -> Array (Tuple String String)
collectExternalRefs (ExprQualified modName name) = [Tuple modName name]
collectExternalRefs (ExprApp f a) = collectExternalRefs f <> collectExternalRefs a
collectExternalRefs (ExprLambda _ body) = collectExternalRefs body
collectExternalRefs (ExprLet binds body) =
  listConcatMap (\b -> collectExternalRefs b.value) binds <> collectExternalRefs body
collectExternalRefs (ExprIf c t e) = collectExternalRefs c <> collectExternalRefs t <> collectExternalRefs e
collectExternalRefs (ExprCase s clauses) =
  collectExternalRefs s <> listConcatMap (\c -> collectExternalRefs c.body) clauses
collectExternalRefs (ExprBinOp _ l r) = collectExternalRefs l <> collectExternalRefs r
collectExternalRefs (ExprUnaryOp _ e) = collectExternalRefs e
collectExternalRefs (ExprList es) = listConcatMap collectExternalRefs es
collectExternalRefs (ExprTuple es) = listConcatMap collectExternalRefs es
collectExternalRefs (ExprRecord fs) = listConcatMap (\(Tuple _ e) -> collectExternalRefs e) fs
collectExternalRefs (ExprRecordAccess e _) = collectExternalRefs e
collectExternalRefs (ExprRecordUpdate e fs) = collectExternalRefs e <> listConcatMap (\(Tuple _ ex) -> collectExternalRefs ex) fs
collectExternalRefs (ExprParens e) = collectExternalRefs e
collectExternalRefs (ExprDo stmts) = listConcatMapDo collectDoStmtRefs stmts
collectExternalRefs (ExprTyped e _) = collectExternalRefs e
collectExternalRefs (ExprSectionLeft e _) = collectExternalRefs e
collectExternalRefs (ExprSectionRight _ e) = collectExternalRefs e
collectExternalRefs (ExprVar name) =
  -- Handle qualified names like "Array.elem" in ExprVar (from backtick syntax)
  case String.indexOf (String.Pattern ".") name of
    Just idx -> [Tuple (String.take idx name) (String.drop (idx + 1) name)]
    Nothing -> []
collectExternalRefs _ = []

-- | Collect external refs from declaration
collectDeclRefs :: Declaration -> Array (Tuple String String)
collectDeclRefs (DeclFunction f) = collectExternalRefs f.body
collectDeclRefs _ = []

-- | Collect all string literals from expression
collectStrings :: Expr -> Array String
collectStrings (ExprLit (LitString s)) = [s]
collectStrings (ExprApp f a) = collectStrings f <> collectStrings a
collectStrings (ExprLambda _ body) = collectStrings body
collectStrings (ExprLet binds body) =
  listConcatMap (\b -> collectStrings b.value) binds <> collectStrings body
collectStrings (ExprIf c t e) = collectStrings c <> collectStrings t <> collectStrings e
collectStrings (ExprCase s clauses) =
  collectStrings s <> listConcatMap (\c -> collectStrings c.body) clauses
collectStrings (ExprBinOp _ l r) = collectStrings l <> collectStrings r
collectStrings (ExprUnaryOp _ e) = collectStrings e
collectStrings (ExprList es) = listConcatMap collectStrings es
collectStrings (ExprTuple es) = listConcatMap collectStrings es
collectStrings (ExprRecord fs) = listConcatMap (\(Tuple _ e) -> collectStrings e) fs
collectStrings (ExprRecordAccess e _) = collectStrings e
collectStrings (ExprRecordUpdate e fs) = collectStrings e <> listConcatMap (\(Tuple _ ex) -> collectStrings ex) fs
collectStrings (ExprParens e) = collectStrings e
collectStrings (ExprDo stmts) = listConcatMapDo collectDoStmt stmts
  where
    collectDoStmt (DoExpr e) = collectStrings e
    collectDoStmt (DoBind _ e) = collectStrings e
    collectDoStmt (DoLet binds) = listConcatMap (\b -> collectStrings b.value) binds
collectStrings (ExprTyped e _) = collectStrings e
collectStrings (ExprSectionLeft e _) = collectStrings e
collectStrings (ExprSectionRight _ e) = collectStrings e
collectStrings _ = []

-- | Helper: concatMap for List of DoStatement returning Array
listConcatMapDo :: forall b. (DoStatement -> Array b) -> List DoStatement -> Array b
listConcatMapDo f lst = foldl (\acc x -> acc <> f x) [] lst

-- | Collect free variables from expression (variables not in bound set)
collectFreeVars :: Set String -> Expr -> Set String
collectFreeVars bound (ExprVar name) =
  if Set.member name bound then Set.empty else Set.singleton name
collectFreeVars bound (ExprApp f a) =
  Set.union (collectFreeVars bound f) (collectFreeVars bound a)
collectFreeVars bound (ExprLambda pats body) =
  let paramNames = Set.fromFoldable (listConcatMap patternVars pats)
      bound' = Set.union bound paramNames
  in collectFreeVars bound' body
collectFreeVars bound (ExprLet binds body) =
  let bindNames = Set.fromFoldable (listConcatMap (\b -> patternVars b.pattern) binds)
      bound' = Set.union bound bindNames
      bindFree = foldl (\s b -> Set.union s (collectFreeVars bound b.value)) Set.empty binds
  in Set.union bindFree (collectFreeVars bound' body)
collectFreeVars bound (ExprIf c t e) =
  Set.union (collectFreeVars bound c) (Set.union (collectFreeVars bound t) (collectFreeVars bound e))
collectFreeVars bound (ExprCase s clauses) =
  let scruFree = collectFreeVars bound s
      clauseFree = foldl (\acc c ->
        let patVars = Set.fromFoldable (patternVars c.pattern)
        in Set.union acc (collectFreeVars (Set.union bound patVars) c.body)) Set.empty clauses
  in Set.union scruFree clauseFree
collectFreeVars bound (ExprBinOp _ l r) =
  Set.union (collectFreeVars bound l) (collectFreeVars bound r)
collectFreeVars bound (ExprUnaryOp _ e) = collectFreeVars bound e
collectFreeVars bound (ExprList es) = foldl (\s e -> Set.union s (collectFreeVars bound e)) Set.empty es
collectFreeVars bound (ExprTuple es) = foldl (\s e -> Set.union s (collectFreeVars bound e)) Set.empty es
collectFreeVars bound (ExprRecord fs) = foldl (\s (Tuple _ e) -> Set.union s (collectFreeVars bound e)) Set.empty fs
collectFreeVars bound (ExprRecordAccess e _) = collectFreeVars bound e
collectFreeVars bound (ExprRecordUpdate e fs) =
  Set.union (collectFreeVars bound e) (foldl (\s (Tuple _ ex) -> Set.union s (collectFreeVars bound ex)) Set.empty fs)
collectFreeVars bound (ExprParens e) = collectFreeVars bound e
collectFreeVars bound (ExprDo stmts) = collectDoFreeVars bound stmts
collectFreeVars bound (ExprTyped e _) = collectFreeVars bound e
collectFreeVars bound (ExprSectionLeft e _) = collectFreeVars bound e
collectFreeVars bound (ExprSectionRight _ e) = collectFreeVars bound e
collectFreeVars _ _ = Set.empty

collectDoFreeVars :: Set.Set String -> List DoStatement -> Set.Set String
collectDoFreeVars _ Nil = Set.empty
collectDoFreeVars b (Cons (DoExpr e) rest) = Set.union (collectFreeVars b e) (collectDoFreeVars b rest)
collectDoFreeVars b (Cons (DoBind pat e) rest) = let patVars = Set.fromFoldable (patternVars pat) in Set.union (collectFreeVars b e) (collectDoFreeVars (Set.union b patVars) rest)
collectDoFreeVars b (Cons (DoLet binds) rest) = let b' = Set.union b (Set.fromFoldable (listConcatMap (\bn -> patternVars bn.pattern) binds)) in Set.union (foldl (\s bn -> Set.union s (collectFreeVars b bn.value)) Set.empty binds) (collectDoFreeVars b' rest)

-- | Collect lambdas from expression and assign unique IDs
-- Returns updated counter and list of lifted lambdas
type LambdaCollector = { counter :: Int, lambdas :: Array LiftedLambda }

-- | Get parameter names from patterns, giving unique names to wildcards
getLambdaParamName :: Int -> Pattern -> String
getLambdaParamName i (PatVar "_") = "__w" <> show i  -- Unique names for underscore variables
getLambdaParamName i (PatVar n) = n
getLambdaParamName i PatWildcard = "__w" <> show i  -- Unique names for wildcards
getLambdaParamName i _ = "__p" <> show i  -- Unique names for complex patterns

collectLambdas :: Set String -> Expr -> LambdaCollector -> LambdaCollector
collectLambdas bound (ExprLambda pats body) state =
  let paramNames = Array.mapWithIndex getLambdaParamName (Array.fromFoldable pats)
      bound' = Set.union bound (Set.fromFoldable paramNames)
      state' = collectLambdas bound' body state
      lambdaParams = Set.fromFoldable paramNames
      freeVars = Set.toUnfoldable (collectFreeVars lambdaParams body)
      lambda = { id: state'.counter, expr: ExprLambda pats body, params: paramNames, freeVars: freeVars, body: body }
  in { counter: state'.counter + 1, lambdas: Array.snoc state'.lambdas lambda }
collectLambdas bound (ExprApp f a) state =
  collectLambdas bound a (collectLambdas bound f state)
collectLambdas bound (ExprLet binds body) state =
  let bindsArr = Array.fromFoldable binds
      bindNames = Set.fromFoldable (Array.concatMap (\b -> patternVars b.pattern) bindsArr)
      bound' = Set.union bound bindNames
      state' = Array.foldl (\s b -> collectLambdas bound b.value s) state bindsArr
  in collectLambdas bound' body state'
collectLambdas bound (ExprIf c t e) state =
  collectLambdas bound e (collectLambdas bound t (collectLambdas bound c state))
collectLambdas bound (ExprCase s clauses) state =
  let state' = collectLambdas bound s state
  in foldl (\st c ->
       let patVars = Set.fromFoldable (patternVars c.pattern)
       in collectLambdas (Set.union bound patVars) c.body st) state' clauses
collectLambdas bound (ExprBinOp _ l r) state =
  collectLambdas bound r (collectLambdas bound l state)
collectLambdas bound (ExprUnaryOp _ e) state = collectLambdas bound e state
collectLambdas bound (ExprList es) state =
  foldl (\s e -> collectLambdas bound e s) state es
collectLambdas bound (ExprTuple es) state =
  foldl (\s e -> collectLambdas bound e s) state es
collectLambdas bound (ExprRecord fs) state =
  foldl (\s (Tuple _ e) -> collectLambdas bound e s) state fs
collectLambdas bound (ExprRecordAccess e _) state = collectLambdas bound e state
collectLambdas bound (ExprRecordUpdate e fs) state =
  let state' = collectLambdas bound e state
  in foldl (\s (Tuple _ ex) -> collectLambdas bound ex s) state' fs
collectLambdas bound (ExprParens e) state = collectLambdas bound e state
collectLambdas bound (ExprDo stmts) state = collectDoLambdas bound stmts state
  where
    collectDoLambdas :: Set String -> List DoStatement -> LambdaCollector -> LambdaCollector
    collectDoLambdas _ Nil st = st
    collectDoLambdas b (Cons (DoExpr e) rest) st =
      collectDoLambdas b rest (collectLambdas b e st)
    collectDoLambdas b (Cons (DoBind pat e) rest) st =
      let patVars = Set.fromFoldable (patternVars pat)
      in collectDoLambdas (Set.union b patVars) rest (collectLambdas b e st)
    collectDoLambdas b (Cons (DoLet binds) rest) st =
      let bindsArr = Array.fromFoldable binds
          bindNames = Set.fromFoldable (Array.concatMap (\bn -> patternVars bn.pattern) bindsArr)
          b' = Set.union b bindNames
          st' = Array.foldl (\s bn -> collectLambdas b bn.value s) st bindsArr
      in collectDoLambdas b' rest st'
collectLambdas bound (ExprTyped e _) state = collectLambdas bound e state
collectLambdas bound (ExprSectionLeft e _) state = collectLambdas bound e state
collectLambdas bound (ExprSectionRight _ e) state = collectLambdas bound e state
collectLambdas _ _ state = state

-- | Collect all lambdas from a declaration
collectDeclLambdas :: Declaration -> LambdaCollector -> LambdaCollector
collectDeclLambdas (DeclFunction f) state =
  let paramNames = Set.fromFoldable (listConcatMap patternVars f.parameters)
  in collectLambdas paramNames f.body state
collectDeclLambdas _ state = state

-- | Add locals from pattern
addLocalsFromPattern :: Pattern -> WasmCtx -> WasmCtx
addLocalsFromPattern (PatVar name) ctx =
  if Map.member name ctx.locals
  then ctx
  else ctx { locals = Map.insert name ctx.localCount ctx.locals
           , localCount = ctx.localCount + 1 }
addLocalsFromPattern PatWildcard ctx = ctx
addLocalsFromPattern (PatLit _) ctx = ctx
addLocalsFromPattern (PatCon _ pats) ctx = foldr addLocalsFromPattern ctx pats
addLocalsFromPattern (PatRecord fields) ctx = foldr (\(Tuple _ p) c -> addLocalsFromPattern p c) ctx fields
addLocalsFromPattern (PatList pats) ctx = foldr addLocalsFromPattern ctx pats
addLocalsFromPattern (PatCons hd tl) ctx = addLocalsFromPattern tl (addLocalsFromPattern hd ctx)
addLocalsFromPattern (PatAs name pat) ctx =
  let ctx' = addLocalsFromPattern pat ctx
  in if Map.member name ctx'.locals
     then ctx'
     else ctx' { locals = Map.insert name ctx'.localCount ctx'.locals
               , localCount = ctx'.localCount + 1 }
addLocalsFromPattern (PatParens p) ctx = addLocalsFromPattern p ctx

-- | Collect pattern variable names
patternVars :: Pattern -> Array String
patternVars (PatVar name) = [name]
patternVars PatWildcard = []
patternVars (PatLit _) = []
patternVars (PatCon _ pats) = listConcatMap patternVars pats
patternVars (PatRecord fields) = listConcatMap (\(Tuple _ p) -> patternVars p) fields
patternVars (PatList pats) = listConcatMap patternVars pats
patternVars (PatCons hd tl) = patternVars hd <> patternVars tl
patternVars (PatAs name pat) = [name] <> patternVars pat
patternVars (PatParens p) = patternVars p

-- | Helper: concatMap for List returning Array
listConcatMap :: forall a b. (a -> Array b) -> List a -> Array b
listConcatMap f lst = foldl (\acc x -> acc <> f x) [] lst

-- | A group of function clauses with same name/arity
type FunctionGroup =
  { name :: String
  , arity :: Int
  , clauses :: Array FunctionDeclaration
  }

-- | Group function declarations by name and arity
groupFunctions :: Array FunctionDeclaration -> Array FunctionGroup
groupFunctions funcs =
  let keys = Array.nubByEq (\a b -> a.name == b.name && a.arity == b.arity)
               (map (\f -> { name: f.name, arity: List.length f.parameters }) funcs)
      mkGroup k = { name: k.name, arity: k.arity, clauses: Array.filter (\f -> f.name == k.name && List.length f.parameters == k.arity) funcs }
  in map mkGroup keys

-- | Collect data constructors from module
collectDataCtors :: Array Declaration -> Map String { tag :: Int, arity :: Int }
collectDataCtors decls =
  let datas = Array.mapMaybe getData decls
      addCtors m d = foldr (\(Tuple i ctor) acc ->
        Map.insert ctor.name { tag: i, arity: List.length ctor.fields } acc) m
        (Array.mapWithIndex (\i c -> Tuple i c) (Array.fromFoldable d.constructors))
  in Array.foldl addCtors Map.empty datas
  where
    getData (DeclDataType d) = Just d
    getData _ = Nothing

-- | Collect strings from declaration

-- Main entry point (simplified)
genModuleSimple :: Module -> String
genModuleSimple m = "(module ;; " <> m.name <> ")"
