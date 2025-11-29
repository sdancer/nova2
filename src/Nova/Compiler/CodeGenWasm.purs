module Nova.Compiler.CodeGenWasm where

import Prelude
import Data.Array (intercalate, length, (:))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.String as String
import Data.String.Pattern as StringPattern
import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (foldr)
import Data.Int as Int
import Data.Enum (fromEnum)
import Nova.Compiler.Ast (Module, Declaration(..), FunctionDeclaration, Expr(..), Pattern(..), Literal(..), LetBind, CaseClause, DoStatement(..))
import Nova.Compiler.RefEq (refEqExpr)

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

-- | Collect all external module references from expression
collectExternalRefs :: Expr -> Array (Tuple String String)
collectExternalRefs (ExprQualified modName name) = [Tuple modName name]
collectExternalRefs (ExprApp f a) = collectExternalRefs f <> collectExternalRefs a
collectExternalRefs (ExprLambda _ body) = collectExternalRefs body
collectExternalRefs (ExprLet binds body) =
  Array.concatMap (\b -> collectExternalRefs b.value) binds <> collectExternalRefs body
collectExternalRefs (ExprIf c t e) = collectExternalRefs c <> collectExternalRefs t <> collectExternalRefs e
collectExternalRefs (ExprCase s clauses) =
  collectExternalRefs s <> Array.concatMap (\c -> collectExternalRefs c.body) clauses
collectExternalRefs (ExprBinOp _ l r) = collectExternalRefs l <> collectExternalRefs r
collectExternalRefs (ExprUnaryOp _ e) = collectExternalRefs e
collectExternalRefs (ExprList es) = Array.concatMap collectExternalRefs es
collectExternalRefs (ExprTuple es) = Array.concatMap collectExternalRefs es
collectExternalRefs (ExprRecord fs) = Array.concatMap (\(Tuple _ e) -> collectExternalRefs e) fs
collectExternalRefs (ExprRecordAccess e _) = collectExternalRefs e
collectExternalRefs (ExprRecordUpdate e fs) = collectExternalRefs e <> Array.concatMap (\(Tuple _ ex) -> collectExternalRefs ex) fs
collectExternalRefs (ExprParens e) = collectExternalRefs e
collectExternalRefs (ExprDo stmts) = Array.concatMap collectDoStmtRefs stmts
  where
    collectDoStmtRefs (DoExpr e) = collectExternalRefs e
    collectDoStmtRefs (DoBind _ e) = collectExternalRefs e
    collectDoStmtRefs (DoLet binds) = Array.concatMap (\b -> collectExternalRefs b.value) binds
collectExternalRefs (ExprTyped e _) = collectExternalRefs e
collectExternalRefs (ExprSectionLeft e _) = collectExternalRefs e
collectExternalRefs (ExprSectionRight _ e) = collectExternalRefs e
collectExternalRefs (ExprVar name) =
  -- Handle qualified names like "Array.elem" in ExprVar (from backtick syntax)
  case String.indexOf (StringPattern.Pattern ".") name of
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
  Array.concatMap (\b -> collectStrings b.value) binds <> collectStrings body
collectStrings (ExprIf c t e) = collectStrings c <> collectStrings t <> collectStrings e
collectStrings (ExprCase s clauses) =
  collectStrings s <> Array.concatMap (\c -> collectStrings c.body) clauses
collectStrings (ExprBinOp _ l r) = collectStrings l <> collectStrings r
collectStrings (ExprUnaryOp _ e) = collectStrings e
collectStrings (ExprList es) = Array.concatMap collectStrings es
collectStrings (ExprTuple es) = Array.concatMap collectStrings es
collectStrings (ExprRecord fs) = Array.concatMap (\(Tuple _ e) -> collectStrings e) fs
collectStrings (ExprRecordAccess e _) = collectStrings e
collectStrings (ExprRecordUpdate e fs) = collectStrings e <> Array.concatMap (\(Tuple _ ex) -> collectStrings ex) fs
collectStrings (ExprParens e) = collectStrings e
collectStrings (ExprDo stmts) = Array.concatMap collectDoStmt stmts
  where
    collectDoStmt (DoExpr e) = collectStrings e
    collectDoStmt (DoBind _ e) = collectStrings e
    collectDoStmt (DoLet binds) = Array.concatMap (\b -> collectStrings b.value) binds
collectStrings (ExprTyped e _) = collectStrings e
collectStrings (ExprSectionLeft e _) = collectStrings e
collectStrings (ExprSectionRight _ e) = collectStrings e
collectStrings _ = []

-- | Collect free variables from expression (variables not in bound set)
collectFreeVars :: Set String -> Expr -> Set String
collectFreeVars bound (ExprVar name) =
  if Set.member name bound then Set.empty else Set.singleton name
collectFreeVars bound (ExprApp f a) =
  Set.union (collectFreeVars bound f) (collectFreeVars bound a)
collectFreeVars bound (ExprLambda pats body) =
  let paramNames = Set.fromFoldable (Array.concatMap patternVars pats)
      bound' = Set.union bound paramNames
  in collectFreeVars bound' body
collectFreeVars bound (ExprLet binds body) =
  let bindNames = Set.fromFoldable (Array.concatMap (\b -> patternVars b.pattern) binds)
      bound' = Set.union bound bindNames
      bindFree = Array.foldl (\s b -> Set.union s (collectFreeVars bound b.value)) Set.empty binds
  in Set.union bindFree (collectFreeVars bound' body)
collectFreeVars bound (ExprIf c t e) =
  Set.union (collectFreeVars bound c) (Set.union (collectFreeVars bound t) (collectFreeVars bound e))
collectFreeVars bound (ExprCase s clauses) =
  let scruFree = collectFreeVars bound s
      clauseFree = Array.foldl (\acc c ->
        let patVars = Set.fromFoldable (patternVars c.pattern)
        in Set.union acc (collectFreeVars (Set.union bound patVars) c.body)) Set.empty clauses
  in Set.union scruFree clauseFree
collectFreeVars bound (ExprBinOp _ l r) =
  Set.union (collectFreeVars bound l) (collectFreeVars bound r)
collectFreeVars bound (ExprUnaryOp _ e) = collectFreeVars bound e
collectFreeVars bound (ExprList es) = Array.foldl (\s e -> Set.union s (collectFreeVars bound e)) Set.empty es
collectFreeVars bound (ExprTuple es) = Array.foldl (\s e -> Set.union s (collectFreeVars bound e)) Set.empty es
collectFreeVars bound (ExprRecord fs) = Array.foldl (\s (Tuple _ e) -> Set.union s (collectFreeVars bound e)) Set.empty fs
collectFreeVars bound (ExprRecordAccess e _) = collectFreeVars bound e
collectFreeVars bound (ExprRecordUpdate e fs) =
  Set.union (collectFreeVars bound e) (Array.foldl (\s (Tuple _ ex) -> Set.union s (collectFreeVars bound ex)) Set.empty fs)
collectFreeVars bound (ExprParens e) = collectFreeVars bound e
collectFreeVars bound (ExprDo stmts) = collectDoFreeVars bound stmts
collectFreeVars bound (ExprTyped e _) = collectFreeVars bound e
collectFreeVars bound (ExprSectionLeft e _) = collectFreeVars bound e
collectFreeVars bound (ExprSectionRight _ e) = collectFreeVars bound e
collectFreeVars _ _ = Set.empty

collectDoFreeVars :: Set.Set String -> Array DoStatement -> Set.Set String
collectDoFreeVars _ arr | Array.null arr = Set.empty
collectDoFreeVars b arr = collectDoFreeVarsHelper b (Array.uncons arr)

collectDoFreeVarsHelper :: Set.Set String -> Maybe { head :: DoStatement, tail :: Array DoStatement } -> Set.Set String
collectDoFreeVarsHelper _ Nothing = Set.empty
collectDoFreeVarsHelper b (Just { head: DoExpr e, tail: rest }) = Set.union (collectFreeVars b e) (collectDoFreeVars b rest)
collectDoFreeVarsHelper b (Just { head: DoBind pat e, tail: rest }) = let patVars = Set.fromFoldable (patternVars pat) in Set.union (collectFreeVars b e) (collectDoFreeVars (Set.union b patVars) rest)
collectDoFreeVarsHelper b (Just { head: DoLet binds, tail: rest }) = let b' = Set.union b (Set.fromFoldable (Array.concatMap (\bn -> patternVars bn.pattern) binds)) in Set.union (Array.foldl (\s bn -> Set.union s (collectFreeVars b bn.value)) Set.empty binds) (collectDoFreeVars b' rest)

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
  let paramNames = Array.mapWithIndex getLambdaParamName pats
      bound' = Set.union bound (Set.fromFoldable paramNames)
      state' = collectLambdas bound' body state
      lambdaParams = Set.fromFoldable paramNames
      freeVars = Set.toUnfoldable (collectFreeVars lambdaParams body)
      lambda = { id: state'.counter, expr: ExprLambda pats body, params: paramNames, freeVars: freeVars, body: body }
  in { counter: state'.counter + 1, lambdas: Array.snoc state'.lambdas lambda }
collectLambdas bound (ExprApp f a) state =
  collectLambdas bound a (collectLambdas bound f state)
collectLambdas bound (ExprLet binds body) state =
  let bindNames = Set.fromFoldable (Array.concatMap (\b -> patternVars b.pattern) binds)
      bound' = Set.union bound bindNames
      state' = Array.foldl (\s b -> collectLambdas bound b.value s) state binds
  in collectLambdas bound' body state'
collectLambdas bound (ExprIf c t e) state =
  collectLambdas bound e (collectLambdas bound t (collectLambdas bound c state))
collectLambdas bound (ExprCase s clauses) state =
  let state' = collectLambdas bound s state
  in Array.foldl (\st c ->
       let patVars = Set.fromFoldable (patternVars c.pattern)
       in collectLambdas (Set.union bound patVars) c.body st) state' clauses
collectLambdas bound (ExprBinOp _ l r) state =
  collectLambdas bound r (collectLambdas bound l state)
collectLambdas bound (ExprUnaryOp _ e) state = collectLambdas bound e state
collectLambdas bound (ExprList es) state =
  Array.foldl (\s e -> collectLambdas bound e s) state es
collectLambdas bound (ExprTuple es) state =
  Array.foldl (\s e -> collectLambdas bound e s) state es
collectLambdas bound (ExprRecord fs) state =
  Array.foldl (\s (Tuple _ e) -> collectLambdas bound e s) state fs
collectLambdas bound (ExprRecordAccess e _) state = collectLambdas bound e state
collectLambdas bound (ExprRecordUpdate e fs) state =
  let state' = collectLambdas bound e state
  in Array.foldl (\s (Tuple _ ex) -> collectLambdas bound ex s) state' fs
collectLambdas bound (ExprParens e) state = collectLambdas bound e state
collectLambdas bound (ExprDo stmts) state = collectDoLambdas bound stmts state
collectLambdas bound (ExprTyped e _) state = collectLambdas bound e state
collectLambdas bound (ExprSectionLeft e _) state = collectLambdas bound e state
collectLambdas bound (ExprSectionRight _ e) state = collectLambdas bound e state
collectLambdas _ _ state = state

-- | Helper for collecting lambdas from do statements
collectDoLambdas :: Set String -> Array DoStatement -> LambdaCollector -> LambdaCollector
collectDoLambdas _ arr st | Array.null arr = st
collectDoLambdas b arr st =
  case Array.uncons arr of
    Nothing -> st
    Just { head: DoExpr e, tail: rest } ->
      collectDoLambdas b rest (collectLambdas b e st)
    Just { head: DoBind pat e, tail: rest } ->
      let patVars = Set.fromFoldable (patternVars pat)
      in collectDoLambdas (Set.union b patVars) rest (collectLambdas b e st)
    Just { head: DoLet binds, tail: rest } ->
      let bindNames = Set.fromFoldable (Array.concatMap (\bn -> patternVars bn.pattern) binds)
          b' = Set.union b bindNames
          st' = Array.foldl (\s bn -> collectLambdas b bn.value s) st binds
      in collectDoLambdas b' rest st'

-- | Collect all lambdas from a declaration
collectDeclLambdas :: Declaration -> LambdaCollector -> LambdaCollector
collectDeclLambdas (DeclFunction f) state =
  let paramNames = Set.fromFoldable (Array.concatMap patternVars f.parameters)
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
patternVars (PatCon _ pats) = Array.concatMap patternVars pats
patternVars (PatRecord fields) = Array.concatMap (\(Tuple _ p) -> patternVars p) fields
patternVars (PatList pats) = Array.concatMap patternVars pats
patternVars (PatCons hd tl) = patternVars hd <> patternVars tl
patternVars (PatAs name pat) = [name] <> patternVars pat
patternVars (PatParens p) = patternVars p

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
               (map (\f -> { name: f.name, arity: length f.parameters }) funcs)
      mkGroup k = { name: k.name, arity: k.arity, clauses: Array.filter (\f -> f.name == k.name && length f.parameters == k.arity) funcs }
  in map mkGroup keys

-- | Collect data constructors from module
collectDataCtors :: Array Declaration -> Map String { tag :: Int, arity :: Int }
collectDataCtors decls =
  let datas = Array.mapMaybe getData decls
      addCtors m d = foldr (\(Tuple i ctor) acc ->
        Map.insert ctor.name { tag: i, arity: length ctor.fields } acc) m
        (Array.mapWithIndex Tuple d.constructors)
  in Array.foldl addCtors Map.empty datas
  where
    getData (DeclDataType d) = Just d
    getData _ = Nothing

-- | Collect strings from declaration
collectDeclStrings :: Declaration -> Array String
collectDeclStrings (DeclFunction f) = collectStrings f.body
collectDeclStrings _ = []

-- | Build string table from list of unique strings
-- Returns map from string -> { offset, len } and total size
buildStringTable :: Array String -> { table :: Map String { offset :: Int, len :: Int }, totalSize :: Int }
buildStringTable strs =
  let addString { table, offset } s =
        let slen = String.length s
        in { table: Map.insert s { offset, len: slen } table
           , offset: offset + slen }
      result = Array.foldl addString { table: Map.empty, offset: 1024 } strs
  in { table: result.table, totalSize: result.offset - 1024 }

-- | Generate data segment for strings
genDataSegment :: Array String -> Int -> String
genDataSegment strs baseOffset =
  if Array.null strs
  then ""
  else "  ;; String data\n  (data (i32.const " <> show baseOffset <> ") \"" <>
       intercalate "" (map escapeWasmString strs) <> "\")\n\n"

-- | Escape a string for WASM data segment
escapeWasmString :: String -> String
escapeWasmString s =
  let s1 = String.replaceAll (String.Pattern "\\") (String.Replacement "\\\\") s
      s2 = String.replaceAll (String.Pattern "\"") (String.Replacement "\\\"") s1
      s3 = String.replaceAll (String.Pattern "\n") (String.Replacement "\\n") s2
      s4 = String.replaceAll (String.Pattern "\t") (String.Replacement "\\t") s3
      s5 = String.replaceAll (String.Pattern "\r") (String.Replacement "\\r") s4
  in s5

-- | Generate module
genModule :: Module -> String
genModule m =
  let modName = m.name
      allFuncs = Array.mapMaybe getFunc m.declarations
      grouped = groupFunctions allFuncs
      uniqueFuncs = Array.nubByEq (\a b -> a.name == b.name && a.arity == b.arity)
                      (map (\g -> { name: g.name, arity: g.arity }) grouped)
      exports = intercalate "\n  " (map genExport uniqueFuncs)
      ctors = collectDataCtors m.declarations
      -- Collect all external module references
      allExternalRefs = Array.nub (Array.concatMap collectDeclRefs m.declarations)
      externalImports = genExternalImports allExternalRefs
      -- Collect all string literals and build table
      allStrings = Array.nub (Array.concatMap collectDeclStrings m.declarations)
      stringTableData = buildStringTable allStrings
      dataSegment = genDataSegment allStrings 1024
      -- Collect all lambdas for lifting
      lambdaCollector = Array.foldl (\s d -> collectDeclLambdas d s) { counter: 0, lambdas: [] } m.declarations
      allLambdas = lambdaCollector.lambdas
      -- Build function arity map
      funcArityMap = Map.fromFoldable (map (\f -> Tuple f.name f.arity) uniqueFuncs)
      -- Functions that need wrappers (arity >= 1)
      -- Only create wrappers for arity-1 functions for now (curried single-arg closures)
      funcsNeedingWrappers = Array.filter (\f -> f.arity == 1) uniqueFuncs
      -- Build wrapper index map: lambdas come first, then function wrappers
      numLambdas = length allLambdas
      funcWrapperIdxMap = Map.fromFoldable $ Array.mapWithIndex
        (\i f -> Tuple f.name (numLambdas + i)) funcsNeedingWrappers
      ctx = (emptyCtx modName)
        { moduleFuncs = Set.fromFoldable (map (\f -> f.name) uniqueFuncs)
        , funcArities = funcArityMap
        , dataConstructors = Map.union ctors preludeConstructors
        , stringTable = stringTableData.table
        , lambdas = allLambdas
        , funcWrapperIdx = funcWrapperIdxMap
        }
      funcDefs = intercalate "\n\n" (map (genFunctionGroup ctx) grouped)
      -- Generate lifted lambda functions
      lambdaDefs = if Array.null allLambdas
                   then ""
                   else "\n\n  ;; Lifted lambdas\n" <>
                        intercalate "\n\n" (map (genLiftedLambda ctx) allLambdas)
      -- Generate function wrapper functions (for function-as-value)
      wrapperDefs = if Array.null funcsNeedingWrappers
                    then ""
                    else "\n\n  ;; Function wrappers (for function-as-value)\n" <>
                         intercalate "\n\n" (map genFunctionWrapper funcsNeedingWrappers)
      -- Generate function table for indirect calls (lambdas + wrappers)
      tableDecl = genFunctionTableWithWrappers allLambdas funcsNeedingWrappers
  in "(module\n" <>
     "  ;; Module: " <> m.name <> "\n\n" <>
     genImports <> "\n\n" <>
     externalImports <>
     "  (memory (export \"memory\") 1)\n" <>
     "  (global $heap_ptr (mut i32) (i32.const 1024))\n\n" <>
     tableDecl <>
     dataSegment <>
     "  ;; Runtime helpers\n" <>
     genRuntimeHelpers <> "\n\n" <>
     "  ;; Functions\n" <>
     funcDefs <>
     lambdaDefs <>
     wrapperDefs <> "\n\n" <>
     "  ;; Exports\n  " <>
     exports <> "\n" <>
     ")\n"
  where
    getFunc (DeclFunction f) = Just f
    getFunc _ = Nothing

-- | Generate function table for indirect calls
genFunctionTable :: Array LiftedLambda -> String
genFunctionTable lambdas =
  let funcNames = map (\l -> mangleName ("__lambda_" <> show l.id)) lambdas
      funcList = intercalate " " funcNames
      n = max 1 (length lambdas)  -- Ensure at least 1 element
  in "  ;; Function table for indirect calls\n" <>
     "  (type $closure_type (func (param i32 i32) (result i32)))\n" <>
     "  (table (export \"__indirect_function_table\") " <> show n <> " funcref)\n" <>
     (if length lambdas > 0
      then "  (elem (i32.const 0) func " <> funcList <> ")\n\n"
      else "\n")

-- | Generate function table including function wrappers
genFunctionTableWithWrappers :: Array LiftedLambda -> Array { name :: String, arity :: Int } -> String
genFunctionTableWithWrappers lambdas wrappers =
  let lambdaNames = map (\l -> mangleName ("__lambda_" <> show l.id)) lambdas
      wrapperNames = map (\f -> mangleName ("__fn_wrap_" <> f.name)) wrappers
      allFuncNames = lambdaNames <> wrapperNames
      funcList = intercalate " " allFuncNames
      n = max 1 (length allFuncNames)  -- Ensure at least 1 element
  in "  ;; Function table for indirect calls\n" <>
     "  (type $closure_type (func (param i32 i32) (result i32)))\n" <>
     "  (table (export \"__indirect_function_table\") " <> show n <> " funcref)\n" <>
     (if length allFuncNames > 0
      then "  (elem (i32.const 0) func " <> funcList <> ")\n\n"
      else "\n")

-- | Generate a wrapper function for function-as-value (arity 1 only for now)
-- Wrapper takes (env, arg) and calls the actual function with arg
genFunctionWrapper :: { name :: String, arity :: Int } -> String
genFunctionWrapper f =
  let wrapperName = "__fn_wrap_" <> f.name
      actualFuncName = mangleName f.name
  in "  (func " <> mangleName wrapperName <> " (param $__env i32) (param $__arg i32) (result i32)\n" <>
     "    (call " <> actualFuncName <> " (local.get $__arg))\n" <>
     "  )"

-- | Generate a lifted lambda function
genLiftedLambda :: WasmCtx -> LiftedLambda -> String
genLiftedLambda ctx lambda =
  let funcName = "__lambda_" <> show lambda.id
      -- Parameters: first the closure env (tuple of captured vars), then lambda params
      allParams = ["__env"] <> lambda.params
      params = map (\n -> "(param " <> mangleName n <> " i32)") allParams
      paramsStr = intercalate " " params
      -- Build local context: captured vars are extracted from env
      capturedVarLocals = Map.fromFoldable (Array.mapWithIndex (\i n -> Tuple n (i + 1 + length lambda.params)) lambda.freeVars)
      paramLocals = Map.fromFoldable (Array.mapWithIndex (\i n -> Tuple n i) allParams)
      allLocals = Map.union paramLocals capturedVarLocals
      bodyLocals = collectLocals lambda.body
      extraLocals = Array.nub (Array.filter (\n -> not (Map.member n allLocals)) bodyLocals)
      localCount = length allParams + length lambda.freeVars + length extraLocals
      localCtx = ctx
        { locals = foldr (\n m -> if Map.member n m then m else Map.insert n (Map.size m) m) allLocals extraLocals
        , localCount = localCount
        }
      -- Generate code to extract captured vars from env tuple
      extractCaptures = if Array.null lambda.freeVars
                        then ""
                        else intercalate "\n    " (Array.mapWithIndex (\i n ->
                          "(local.set " <> mangleName n <> " (call $tuple_get (local.get $__env) (i32.const " <> show i <> ")))") lambda.freeVars) <> "\n    "
      -- Generate local declarations
      localDecls = map (\n -> "(local " <> mangleName n <> " i32)") (lambda.freeVars <> extraLocals <> ["__tmp", "__rec_base", "__arg0", "__arg1", "__arg2", "__arg3", "__arg4"])
      localsStr = intercalate " " localDecls
      bodyCode = genExpr localCtx lambda.body
  in "  (func " <> mangleName funcName <> " " <> paramsStr <> " (result i32)\n" <>
     "    " <> localsStr <> "\n" <>
     "    " <> extractCaptures <> bodyCode <> "\n" <>
     "  )"

-- | Generate imports for external module functions
genExternalImports :: Array (Tuple String String) -> String
genExternalImports refs =
  if Array.null refs
  then ""
  else "  ;; External module imports\n" <>
       intercalate "\n" (map genExtImport refs) <> "\n\n"
  where
    genExtImport (Tuple modName name) =
      let fullName = modName <> "_" <> name
      in "  (import \"modules\" \"" <> fullName <> "\" (func " <> mangleName fullName <> " (result i32)))"

-- | Generate JS imports
genImports :: String
genImports =
  "  ;; JS Runtime imports\n" <>
  "  (import \"runtime\" \"alloc\" (func $rt_alloc (param i32) (result i32)))\n" <>
  "  (import \"runtime\" \"make_string\" (func $rt_make_string (param i32 i32) (result i32)))\n" <>
  "  (import \"runtime\" \"string_append\" (func $rt_string_append (param i32 i32) (result i32)))\n" <>
  "  (import \"runtime\" \"string_eq\" (func $rt_string_eq (param i32 i32) (result i32)))\n" <>
  "  (import \"runtime\" \"generic_eq\" (func $rt_generic_eq (param i32 i32) (result i32)))\n" <>
  "  (import \"runtime\" \"generic_ne\" (func $rt_generic_ne (param i32 i32) (result i32)))\n" <>
  "  (import \"runtime\" \"make_array\" (func $rt_make_array (param i32) (result i32)))\n" <>
  "  (import \"runtime\" \"array_push\" (func $rt_array_push (param i32 i32) (result i32)))\n" <>
  "  (import \"runtime\" \"array_get\" (func $rt_array_get (param i32 i32) (result i32)))\n" <>
  "  (import \"runtime\" \"array_length\" (func $rt_array_length (param i32) (result i32)))\n" <>
  "  (import \"runtime\" \"print\" (func $rt_print (param i32)))\n" <>
  "  (import \"runtime\" \"make_closure\" (func $rt_make_closure (param i32 i32) (result i32)))\n" <>
  "  (import \"runtime\" \"apply_closure\" (func $rt_apply_closure (param i32 i32) (result i32)))\n" <>
  "  (import \"runtime\" \"closure_get_env\" (func $closure_get_env (param i32) (result i32)))\n" <>
  "  (import \"runtime\" \"alloc_tuple\" (func $alloc_tuple (param i32) (result i32)))\n" <>
  "  (import \"runtime\" \"tuple_get\" (func $tuple_get (param i32 i32) (result i32)))\n" <>
  "  (import \"runtime\" \"tuple_set\" (func $tuple_set (param i32 i32 i32)))"

-- | Generate runtime helper functions
genRuntimeHelpers :: String
genRuntimeHelpers =
  "  ;; Tag constants\n" <>
  "  ;; 00 = int, 01 = bool, 10 = heap ptr, 11 = ctor\n\n" <>
  "  (func $make_int (param $val i32) (result i32)\n" <>
  "    (i32.shl (local.get $val) (i32.const 2))\n" <>
  "  )\n\n" <>
  "  (func $unbox_int (param $val i32) (result i32)\n" <>
  "    (i32.shr_s (local.get $val) (i32.const 2))\n" <>
  "  )\n\n" <>
  "  (func $make_bool (param $val i32) (result i32)\n" <>
  "    (i32.or (i32.shl (local.get $val) (i32.const 2)) (i32.const 1))\n" <>
  "  )\n\n" <>
  "  (func $unbox_bool (param $val i32) (result i32)\n" <>
  "    (i32.shr_u (local.get $val) (i32.const 2))\n" <>
  "  )\n\n" <>
  "  (func $make_unit (result i32)\n" <>
  "    (i32.const 1)  ;; unit is bool-tagged 0\n" <>
  "  )\n\n" <>
  "  (func $make_heap_ptr (param $addr i32) (result i32)\n" <>
  "    (i32.or (i32.shl (local.get $addr) (i32.const 2)) (i32.const 2))\n" <>
  "  )\n\n" <>
  "  (func $unbox_heap_ptr (param $val i32) (result i32)\n" <>
  "    (i32.shr_u (local.get $val) (i32.const 2))\n" <>
  "  )\n\n" <>
  "  (func $make_ctor (param $tag i32) (param $arity i32) (result i32)\n" <>
  "    (i32.or\n" <>
  "      (i32.or\n" <>
  "        (i32.shl (local.get $arity) (i32.const 16))\n" <>
  "        (i32.shl (local.get $tag) (i32.const 2)))\n" <>
  "      (i32.const 3))\n" <>
  "  )\n\n" <>
  "  (func $get_ctor_tag (param $val i32) (result i32)\n" <>
  "    (i32.and (i32.shr_u (local.get $val) (i32.const 2)) (i32.const 0x3fff))\n" <>
  "  )\n\n" <>
  "  (func $get_tag (param $val i32) (result i32)\n" <>
  "    (i32.and (local.get $val) (i32.const 3))\n" <>
  "  )\n\n" <>
  "  (func $is_ctor (param $val i32) (result i32)\n" <>
  "    (i32.eq (call $get_tag (local.get $val)) (i32.const 3))\n" <>
  "  )\n\n" <>
  "  ;; Heap allocation (for internal use only)\n" <>
  "  (func $alloc (param $size i32) (result i32)\n" <>
  "    (local $ptr i32)\n" <>
  "    (local.set $ptr (global.get $heap_ptr))\n" <>
  "    (global.set $heap_ptr (i32.add (global.get $heap_ptr) (local.get $size)))\n" <>
  "    (local.get $ptr)\n" <>
  "  )\n\n" <>
  "  ;; Tuple operations are imported from runtime (alloc_tuple, tuple_get, tuple_set)"

-- | Generate export declaration
genExport :: { name :: String, arity :: Int } -> String
genExport f =
  "(export \"" <> f.name <> "\" (func " <> mangleName f.name <> "))"

-- | Check if parameters contain complex patterns
hasComplexPattern :: Array Pattern -> Boolean
hasComplexPattern pats = Array.any isComplex pats
  where
    isComplex (PatVar _) = false
    isComplex PatWildcard = false
    isComplex _ = true

-- | Generate a function group
genFunctionGroup :: WasmCtx -> FunctionGroup -> String
genFunctionGroup ctx group =
  case Array.uncons group.clauses of
    Nothing -> ""
    Just { head: first, tail: [] } ->
      if hasComplexPattern first.parameters
      then genMergedFunction ctx group
      else genFunction ctx first
    Just _ ->
      genMergedFunction ctx group

-- | Get parameter name from pattern
getParamName :: Int -> Pattern -> String
getParamName i (PatVar n) = n
getParamName i _ = "p" <> show i

-- | Collect all locals needed in a function body
-- NOTE: We do NOT recurse into lambdas - they are lifted to separate functions
collectLocals :: Expr -> Array String
collectLocals (ExprLet binds body) =
  Array.concatMap (\b -> patternVars b.pattern <> collectLocals b.value) binds <> collectLocals body
collectLocals (ExprLambda _ _) = []  -- Lambda params and body are handled in lifted function
collectLocals (ExprCase _ clauses) =
  Array.concatMap (\c -> patternVars c.pattern <> collectLocals c.body) clauses
collectLocals (ExprIf c t e) = collectLocals c <> collectLocals t <> collectLocals e
collectLocals (ExprApp f a) = collectLocals f <> collectLocals a
collectLocals (ExprBinOp _ l r) = collectLocals l <> collectLocals r
collectLocals (ExprUnaryOp _ e) = collectLocals e
collectLocals (ExprList es) = Array.concatMap collectLocals es
collectLocals (ExprTuple es) = Array.concatMap collectLocals es
collectLocals (ExprRecord fs) = Array.concatMap (\(Tuple _ e) -> collectLocals e) fs
collectLocals (ExprRecordAccess e _) = collectLocals e
collectLocals (ExprRecordUpdate e fs) = collectLocals e <> Array.concatMap (\(Tuple _ ex) -> collectLocals ex) fs
collectLocals (ExprParens e) = collectLocals e
collectLocals (ExprDo stmts) = Array.concatMap collectDoStmt stmts
  where
    collectDoStmt (DoExpr e) = collectLocals e
    collectDoStmt (DoBind p e) = patternVars p <> collectLocals e
    collectDoStmt (DoLet binds) = Array.concatMap (\b -> patternVars b.pattern <> collectLocals b.value) binds
collectLocals (ExprTyped e _) = collectLocals e
collectLocals _ = []

-- | Generate simple function
genFunction :: WasmCtx -> FunctionDeclaration -> String
genFunction ctx func =
  let paramNames = Array.mapWithIndex getParamName func.parameters
      params = map (\n -> "(param " <> mangleName n <> " i32)") paramNames
      paramsStr = intercalate " " params
      paramLocals = Map.fromFoldable (Array.mapWithIndex (\i n -> Tuple n i) paramNames)
      bodyLocals = Array.nub (collectLocals func.body)
      -- Always include __tmp for tuple allocation, __rec_base for record updates, and __argN for constructor args
      argSlots = ["__arg0", "__arg1", "__arg2", "__arg3", "__arg4"]
      extraLocals = Array.nub (Array.filter (\n -> not (Array.elem n paramNames)) ("__tmp" : "__rec_base" : argSlots <> bodyLocals))
      allLocals = Map.union paramLocals
                    (Map.fromFoldable (Array.mapWithIndex (\i n -> Tuple n (i + length paramNames)) extraLocals))
      localDecls = "\n    " <> intercalate " " (map (\n -> "(local " <> mangleName n <> " i32)") extraLocals)
      ctxWithParams = ctx { locals = allLocals, localCount = Map.size allLocals }
      bodyCode = genExpr ctxWithParams func.body
  in "  (func " <> mangleName func.name <> " " <> paramsStr <> " (result i32)" <> localDecls <> "\n" <>
     "    " <> bodyCode <> "\n" <>
     "  )"

-- | Generate merged function with pattern matching
genMergedFunction :: WasmCtx -> FunctionGroup -> String
genMergedFunction ctx group =
  let paramNames = Array.range 0 (group.arity - 1) # map (\i -> "p" <> show i)
      params = map (\n -> "(param " <> mangleName n <> " i32)") paramNames
      paramsStr = intercalate " " params
      paramLocals = Map.fromFoldable (Array.mapWithIndex (\i n -> Tuple n i) paramNames)
      -- Collect all locals from all clauses
      clauseLocals = Array.concatMap (\c ->
        Array.concatMap patternVars c.parameters <> collectLocals c.body) group.clauses
      -- Always include __tmp for tuple allocation, __rec_base for record updates, and __argN for constructor args
      argSlots = ["__arg0", "__arg1", "__arg2", "__arg3", "__arg4"]
      extraLocals = Array.nub (Array.filter (\n -> not (Array.elem n paramNames)) ("__tmp" : "__rec_base" : argSlots <> clauseLocals))
      allLocals = Map.union paramLocals
                    (Map.fromFoldable (Array.mapWithIndex (\i n -> Tuple n (i + length paramNames)) extraLocals))
      localDecls = "\n    " <> intercalate " " (map (\n -> "(local " <> mangleName n <> " i32)") extraLocals)
      ctxWithParams = ctx { locals = allLocals, localCount = Map.size allLocals }
      matchCode = genPatternMatch ctxWithParams paramNames group.clauses
  in "  (func " <> mangleName group.name <> " " <> paramsStr <> " (result i32)" <> localDecls <> "\n" <>
     "    " <> matchCode <> "\n" <>
     "  )"

-- | Generate pattern matching code
genPatternMatch :: WasmCtx -> Array String -> Array FunctionDeclaration -> String
genPatternMatch ctx paramNames clauses =
  case Array.uncons clauses of
    Nothing -> "(unreachable)"
    Just { head: clause, tail: rest } ->
      let bindCode = genPatternBindings ctx paramNames clause.parameters
          bodyCode = genExpr ctx clause.body
          testCode = genPatternTest ctx paramNames clause.parameters
          fallback = genPatternMatch ctx paramNames rest
      in if Array.null rest
         then bindCode <> bodyCode
         else "(if (result i32) " <> testCode <> "\n" <>
              "      (then " <> bindCode <> bodyCode <> ")\n" <>
              "      (else " <> fallback <> "))"

-- | Generate pattern test
genPatternTest :: WasmCtx -> Array String -> Array Pattern -> String
genPatternTest ctx paramNames patterns =
  let tests = Array.mapWithIndex (\i pat -> genSinglePatternTest ctx ("$p" <> show i) pat)
              (Array.zip paramNames patterns # map (\(Tuple _ p) -> p))
      validTests = Array.filter (_ /= "(i32.const 1)") tests
  in if Array.null validTests
     then "(i32.const 1)"
     else intercalate "\n        (i32.and " validTests <>
          String.joinWith "" (Array.replicate (length validTests - 1) ")")

-- | Generate test for single pattern
genSinglePatternTest :: WasmCtx -> String -> Pattern -> String
genSinglePatternTest _ _ (PatVar _) = "(i32.const 1)"
genSinglePatternTest _ _ PatWildcard = "(i32.const 1)"
genSinglePatternTest _ param (PatLit (LitInt n)) =
  "(i32.eq (local.get " <> param <> ") (call $make_int (i32.const " <> show n <> ")))"
genSinglePatternTest _ param (PatLit (LitBool b)) =
  "(i32.eq (local.get " <> param <> ") (call $make_bool (i32.const " <> (if b then "1" else "0") <> ")))"
genSinglePatternTest ctx param (PatCon name _) =
  case Map.lookup name ctx.dataConstructors of
    Just { tag } ->
      "(i32.and\n" <>
      "          (call $is_ctor (local.get " <> param <> "))\n" <>
      "          (i32.eq (call $get_ctor_tag (local.get " <> param <> ")) (i32.const " <> show tag <> ")))"
    Nothing -> "(i32.const 1)"
genSinglePatternTest ctx param (PatParens p) = genSinglePatternTest ctx param p
genSinglePatternTest ctx param (PatAs _ p) = genSinglePatternTest ctx param p
genSinglePatternTest _ _ _ = "(i32.const 1)"

-- | Generate pattern bindings
genPatternBindings :: WasmCtx -> Array String -> Array Pattern -> String
genPatternBindings ctx paramNames patterns =
  let bindings = Array.concatMap (\(Tuple pname pat) -> genSinglePatternBinding ctx pname pat)
                   (Array.zip paramNames patterns)
  in intercalate "\n        " bindings

-- | Bind a pattern to a field extraction expression
genPatternFieldBinding :: WasmCtx -> String -> Pattern -> Array String
genPatternFieldBinding ctx expr (PatVar name) =
  case Map.lookup name ctx.locals of
    Just _ -> ["(local.set " <> mangleName name <> " " <> expr <> ")"]
    Nothing -> []
genPatternFieldBinding _ _ PatWildcard = []
genPatternFieldBinding _ _ (PatLit _) = []
genPatternFieldBinding _ _ (PatCon _ _) = [] -- Nested constructor matching not yet supported
genPatternFieldBinding ctx expr (PatAs name p) =
  (case Map.lookup name ctx.locals of
    Just _ -> ["(local.set " <> mangleName name <> " " <> expr <> ")"]
    Nothing -> []) <> genPatternFieldBinding ctx expr p
genPatternFieldBinding ctx expr (PatParens p) = genPatternFieldBinding ctx expr p
genPatternFieldBinding _ _ _ = []

-- | Generate binding for single pattern
genSinglePatternBinding :: WasmCtx -> String -> Pattern -> Array String
genSinglePatternBinding ctx param (PatVar name) =
  case Map.lookup name ctx.locals of
    Just _ | name /= param -> ["(local.set " <> mangleName name <> " (local.get " <> mangleName param <> "))"]
    _ -> []
genSinglePatternBinding _ _ PatWildcard = []
genSinglePatternBinding _ _ (PatLit _) = []
genSinglePatternBinding ctx param (PatCon _ subPats) =
  -- Constructor fields are stored in tuple starting at index 1 (index 0 is tag)
  Array.concatMap (\(Tuple i p) ->
    let fieldExpr = "(call $tuple_get (local.get " <> mangleName param <> ") (i32.const " <> show (i + 1) <> "))"
    in genPatternFieldBinding ctx fieldExpr p)
    (Array.mapWithIndex Tuple subPats)
genSinglePatternBinding ctx param (PatAs name p) =
  (case Map.lookup name ctx.locals of
    Just _ -> ["(local.set " <> mangleName name <> " (local.get " <> mangleName param <> "))"]
    Nothing -> []) <> genSinglePatternBinding ctx param p
genSinglePatternBinding ctx param (PatParens p) = genSinglePatternBinding ctx param p
genSinglePatternBinding _ _ _ = []

-- | Flatten a chain of function applications into (func, [args])
flattenApp :: Expr -> Tuple Expr (Array Expr)
flattenApp (ExprApp f a) =
  let Tuple baseFunc args = flattenApp f
  in Tuple baseFunc (args <> [a])
flattenApp e = Tuple e []

-- | Generate a chain of closure applications
genClosureApps :: WasmCtx -> Expr -> Array Expr -> String
genClosureApps ctx baseExpr args =
  let genE = genExpr ctx
  in case Array.uncons args of
    Nothing -> genE baseExpr
    Just { head: firstArg, tail: restArgs } ->
      let baseCode = genE baseExpr
          firstArgCode = genE firstArg
          firstCall = "(call $rt_apply_closure " <> baseCode <> " " <> firstArgCode <> ")"
      in Array.foldl (\acc arg ->
           "(call $rt_apply_closure " <> acc <> " " <> genE arg <> ")") firstCall restArgs

-- | Generate expression
genExpr :: WasmCtx -> Expr -> String
genExpr ctx (ExprVar name) =
  case Map.lookup name ctx.locals of
    Just _ -> "(local.get " <> mangleName name <> ")"
    Nothing ->
      if Set.member name ctx.moduleFuncs
      then
        -- Check arity: 0-arity functions are called immediately, others need closures
        case Map.lookup name ctx.funcArities of
          Just 0 -> "(call " <> mangleName name <> ")"  -- 0-arity: call and get result
          Just 1 ->
            -- Arity 1: use the wrapper function index from funcWrapperIdx
            case Map.lookup name ctx.funcWrapperIdx of
              Just wrapperIdx ->
                "(call $rt_make_closure (i32.const " <> show wrapperIdx <> ") (call $alloc_tuple (i32.const 0)))"
              Nothing ->
                -- Fallback: no wrapper registered (shouldn't happen for arity-1 funcs)
                "(call $rt_make_closure (i32.const 0) (call $alloc_tuple (i32.const 0)))"
          Just _arity ->
            -- Higher arity (>1): still needs currying support, use placeholder for now
            "(call $rt_make_closure (i32.const 0) (call $alloc_tuple (i32.const 0)))"
          Nothing -> "(call " <> mangleName name <> ")"  -- Default to call
      else case Map.lookup name ctx.dataConstructors of
        Just { tag, arity: 0 } -> "(call $make_ctor (i32.const " <> show tag <> ") (i32.const 0))"
        Just _ -> "(i32.const 0)" -- Constructor with args, needs partial application
        Nothing ->
          -- Check if this is a qualified name like "Array.elem"
          case String.indexOf (StringPattern.Pattern ".") name of
            Just idx ->
              let modName = String.take idx name
                  funcName = String.drop (idx + 1) name
                  callName = "$" <> modName <> "_" <> funcName
              in "(call " <> callName <> ")"  -- External module function
            Nothing -> "(i32.const 0)" -- Truly unknown

genExpr _ (ExprQualified modName name) =
  "(call " <> mangleName (modName <> "_" <> name) <> ")"

-- String literals need special handling to use the stringTable
genExpr ctx (ExprLit (LitString s)) =
  case Map.lookup s ctx.stringTable of
    Just { offset, len } -> "(call $rt_make_string (i32.const " <> show offset <> ") (i32.const " <> show len <> "))"
    Nothing -> "(call $make_unit)"  -- Fallback if string not in table

genExpr _ (ExprLit lit) = genLiteral lit

genExpr ctx (ExprApp func arg) =
  -- Flatten nested applications to handle multi-arg functions
  let Tuple baseFunc allArgs = flattenApp (ExprApp func arg)
  in case baseFunc of
    ExprVar name | Set.member name ctx.moduleFuncs && not (Map.member name ctx.locals) ->
      -- Known module function NOT captured as local - check arity before calling directly
      case Map.lookup name ctx.funcArities of
        Just arity | length allArgs == arity ->
          -- Exact arity match - call directly
          let argCodes = map (genExpr ctx) allArgs
          in "(call " <> mangleName name <> " " <> intercalate " " argCodes <> ")"
        _ ->
          -- Partial application or arity mismatch - use closures
          genClosureApps ctx baseFunc allArgs
    ExprVar name ->
      case Map.lookup name ctx.dataConstructors of
        Just { tag, arity } | arity > 0 && length allArgs == arity ->
          -- Constructor with all args provided - create tuple
          -- IMPORTANT: Evaluate args first into __argN temps, THEN allocate tuple
          -- This prevents nested constructor allocations from clobbering __tmp
          let n = arity + 1
              argCodes = map (genExpr ctx) allArgs
              argSlots = Array.mapWithIndex (\i _ -> "$__arg" <> show i) allArgs
              -- First evaluate all args into arg slots
              evalCode = Array.zipWith (\slot code ->
                "(local.set " <> slot <> " " <> code <> ")") argSlots argCodes
              -- Then set tuple fields from arg slots
              setCode = Array.mapWithIndex (\i slot ->
                "(call $tuple_set (local.get $__tmp) (i32.const " <> show (i + 1) <> ") (local.get " <> slot <> "))") argSlots
          in "(block (result i32)\n        " <>
             intercalate "\n        " evalCode <> "\n" <>
             "        (local.set $__tmp (call $alloc_tuple (i32.const " <> show n <> ")))\n" <>
             "        (call $tuple_set (local.get $__tmp) (i32.const 0) (call $make_ctor (i32.const " <> show tag <> ") (i32.const " <> show arity <> ")))\n        " <>
             intercalate "\n        " setCode <>
             "\n        (local.get $__tmp))"
        _ ->
          -- Not a known function/constructor - use closures
          genClosureApps ctx baseFunc allArgs
    _ ->
      -- Not a simple variable - use closures
      genClosureApps ctx baseFunc allArgs

genExpr ctx (ExprLambda pats body) =
  let thisLambda = ExprLambda pats body
      matchingLambda = Array.find (\l -> refEqExpr l.expr thisLambda) ctx.lambdas
  in case matchingLambda of
    Just lambda ->
      let freeVars = lambda.freeVars
          n = length freeVars
          tableIdx = lambda.id
      in if n == 0
         -- No captures: just create closure with empty env
         then "(call $rt_make_closure (i32.const " <> show tableIdx <> ") (call $alloc_tuple (i32.const 0)))"
         else
           -- Build env tuple with captured variables
           let setCaptures = Array.mapWithIndex (\i v ->
                 "(call $tuple_set (local.get $__tmp) (i32.const " <> show i <> ") " <> genExpr ctx (ExprVar v) <> ")") freeVars
           in "(block (result i32)\n" <>
              "        (local.set $__tmp (call $alloc_tuple (i32.const " <> show n <> ")))\n        " <>
              intercalate "\n        " setCaptures <>
              "\n        (call $rt_make_closure (i32.const " <> show tableIdx <> ") (local.get $__tmp)))"
    Nothing ->
      -- Fallback: inline the body (for nested lambdas not yet tracked)
      let vars = Array.concatMap patternVars pats
          ctx' = foldr (\n c ->
            if Map.member n c.locals then c
            else c { locals = Map.insert n c.localCount c.locals, localCount = c.localCount + 1 }) ctx vars
      in genExpr ctx' body

genExpr ctx (ExprLet binds body) =
  genLetBinds ctx binds body

genExpr ctx (ExprIf cond thenE elseE) =
  "(if (result i32) (call $unbox_bool " <> genExpr ctx cond <> ")\n" <>
  "      (then " <> genExpr ctx thenE <> ")\n" <>
  "      (else " <> genExpr ctx elseE <> "))"

genExpr ctx (ExprCase scrutinee clauses) =
  genCaseExpr ctx scrutinee clauses

genExpr ctx (ExprBinOp op l r) =
  genBinOp ctx op l r

genExpr ctx (ExprUnaryOp "-" e) =
  "(call $make_int (i32.sub (i32.const 0) (call $unbox_int " <> genExpr ctx e <> ")))"

genExpr ctx (ExprUnaryOp "not" e) =
  "(call $make_bool (i32.xor (call $unbox_bool " <> genExpr ctx e <> ") (i32.const 1)))"

genExpr ctx (ExprUnaryOp _ e) = genExpr ctx e

genExpr ctx (ExprList elems) =
  -- Create an actual Array (not a linked list) for Array.elem to work
  let n = length elems
      pushAll = intercalate "\n" $ Array.mapWithIndex (\i e ->
        "      (drop (call $rt_array_push (local.get $__tmp) " <> genExpr ctx e <> "))") elems
  in if n == 0
     then "(call $rt_make_array (i32.const 0))"
     else "(block (result i32)\n" <>
          "      (local.set $__tmp (call $rt_make_array (i32.const " <> show n <> ")))\n" <>
          pushAll <>
          "\n      (local.get $__tmp))"

genExpr ctx (ExprTuple elems) =
  let n = length elems
  in if n == 0
     then "(call $make_unit)"
     else "(block (result i32)\n" <>
          "      (local.set $__tmp (call $alloc_tuple (i32.const " <> show n <> ")))\n" <>
          intercalate "\n" (Array.mapWithIndex (\i e ->
            "      (call $tuple_set (local.get $__tmp) (i32.const " <> show i <> ") " <> genExpr ctx e <> ")") elems) <>
          "\n      (local.get $__tmp))"

genExpr ctx (ExprRecord fields) =
  -- Sort fields alphabetically for consistent access
  let sortedFields = Array.sortWith (\(Tuple name _) -> name) fields
      n = length sortedFields
  in "(block (result i32)\n" <>
     "      (local.set $__tmp (call $alloc_tuple (i32.const " <> show n <> ")))\n" <>
     intercalate "\n" (Array.mapWithIndex (\i (Tuple _ e) ->
       "      (call $tuple_set (local.get $__tmp) (i32.const " <> show i <> ") " <> genExpr ctx e <> ")") sortedFields) <>
     "\n      (local.get $__tmp))"

genExpr ctx (ExprRecordAccess expr field) =
  -- Access field by alphabetical index within the record
  -- Records are stored with fields sorted alphabetically
  -- We use a known field index map for common record types
  let recCode = genExpr ctx expr
      idx = fieldIndex field
  in "(call $tuple_get " <> recCode <> " (i32.const " <> show idx <> "))"
  where
    -- Known field indices based on alphabetical ordering
    -- TokState: { column, input, line, pos } -> [0,1,2,3]
    -- Token: { column, line, pos, tokenType, value } -> [0,1,2,3,4]
    -- Note: These are actually different due to 'input' presence in TokState
    --   TokState: column=0, input=1, line=2, pos=3
    --   Token: column=0, line=1, pos=2, tokenType=3, value=4
    -- We use TokState indices for shared fields since Tokenizer uses them more
    fieldIndex :: String -> Int
    fieldIndex s = case s of
      -- TokState fields
      "column" -> 0
      "input" -> 1
      "line" -> 2  -- TokState index (Tokenizer uses this)
      "pos" -> 3   -- TokState index (Tokenizer uses this)
      -- Token-only fields - use Token's alphabetical indices
      "tokenType" -> 3  -- In Token: [column=0, line=1, pos=2, tokenType=3, value=4]
      "value" -> 4
      -- Additional common fields
      "arity" -> 0
      "body" -> 0
      "clauses" -> 0
      "declarations" -> 0
      "expr" -> 0
      "fields" -> 0
      "imports" -> 1
      "moduleName" -> 2
      "name" -> 2
      "params" -> 3
      "pattern" -> 3
      "patterns" -> 3
      "tag" -> 4
      "type" -> 4
      "typeParams" -> 4
      -- Array.span result fields: { init, rest } alphabetical order
      "init" -> 0
      "rest" -> 1
      -- Fallback - use string position as rough estimate
      _ -> 0

genExpr ctx (ExprRecordUpdate baseExpr fieldUpdates) =
  -- Record update: create new tuple, copy all fields from original, overwrite updated ones
  -- We need to know the record size - infer from fields being updated
  let updateFieldNames = map (\(Tuple name _) -> name) fieldUpdates
      -- Determine record size based on known record types
      recordSize = inferRecordSize updateFieldNames
      baseCode = genExpr ctx baseExpr
      -- Generate code to allocate new tuple and copy/update fields
  in "(block (result i32)\n" <>
     "      (local.set $__rec_base " <> baseCode <> ")\n" <>
     "      (local.set $__tmp (call $alloc_tuple (i32.const " <> show recordSize <> ")))\n" <>
     -- For each field position, either copy from base or use update value
     intercalate "\n" (map (\idx ->
       let maybeUpdate = Array.find (\(Tuple n _) -> fieldIndex n == idx) fieldUpdates
       in case maybeUpdate of
            Just (Tuple _ updateExpr) ->
              "      (call $tuple_set (local.get $__tmp) (i32.const " <> show idx <> ") " <> genExpr ctx updateExpr <> ")"
            Nothing ->
              "      (call $tuple_set (local.get $__tmp) (i32.const " <> show idx <> ") (call $tuple_get (local.get $__rec_base) (i32.const " <> show idx <> ")))"
       ) (Array.range 0 (recordSize - 1))) <>
     "\n      (local.get $__tmp))"
  where
    -- Known field indices based on alphabetical ordering
    fieldIndex :: String -> Int
    fieldIndex s = case s of
      "column" -> 0
      "input" -> 1
      "line" -> 2
      "pos" -> 3
      "tokenType" -> 4
      "value" -> 5
      _ -> 0

    -- Infer record size from field names being updated
    inferRecordSize :: Array String -> Int
    inferRecordSize fields
      | Array.any (\f -> f == "pos" || f == "column" || f == "line" || f == "input") fields = 4 -- TokState
      | Array.any (\f -> f == "tokenType" || f == "value") fields = 5 -- Token
      | otherwise =
          -- Use max field index + 1
          let maxIdx = Array.foldl (\acc f -> max acc (fieldIndex f)) 0 fields
          in maxIdx + 1

genExpr ctx (ExprParens e) = genExpr ctx e

genExpr ctx (ExprDo stmts) = genDoStmts ctx stmts

genExpr ctx (ExprTyped e _) = genExpr ctx e

genExpr _ (ExprSection _) = "(call $make_unit)"

genExpr ctx (ExprSectionLeft e _) = genExpr ctx e

genExpr ctx (ExprSectionRight _ e) = genExpr ctx e

-- | Generate literal
genLiteral :: Literal -> String
genLiteral (LitInt n) = "(call $make_int (i32.const " <> show n <> "))"
genLiteral (LitNumber n) = "(call $make_int (i32.const " <> show (Int.floor n) <> "))"
genLiteral (LitString _) =
  -- For now, represent strings as heap pointers to UTF-8 data
  -- The JS runtime will handle actual string operations
  "(call $make_unit)" -- Placeholder
genLiteral (LitChar c) = "(call $make_int (i32.const " <> show (fromEnum c) <> "))"
genLiteral (LitBool true) = "(call $make_bool (i32.const 1))"
genLiteral (LitBool false) = "(call $make_bool (i32.const 0))"

-- | Generate let bindings
genLetBinds :: WasmCtx -> Array LetBind -> Expr -> String
genLetBinds ctx binds body =
  case Array.uncons binds of
    Nothing -> genExpr ctx body
    Just { head: bind, tail: rest } ->
      let vars = patternVars bind.pattern
          ctx' = foldr (\n c ->
            if Map.member n c.locals then c
            else c { locals = Map.insert n c.localCount c.locals, localCount = c.localCount + 1 }) ctx vars
          valueCode = genExpr ctx bind.value
          restCode = genLetBinds ctx' rest body
      in case bind.pattern of
           PatVar name ->
             case Map.lookup name ctx'.locals of
               Just _ ->
                 -- Check if this is a self-referential lambda (closure that captures itself)
                 let selfRefPatch = case bind.value of
                       ExprLambda _ _ ->
                         -- Find the lambda in ctx.lambdas
                         case Array.find (\l -> refEqExpr l.expr bind.value) ctx.lambdas of
                           Just lambda ->
                             -- Check if name is in freeVars
                             case Array.findIndex (_ == name) lambda.freeVars of
                               Just idx ->
                                 -- Generate backpatching: patch the env tuple with the closure itself
                                 -- Closure layout: tuple[0]=funcIdx, tuple[1]=envPtr
                                 -- So we patch: env[idx] = closure
                                 "\n        (call $tuple_set (call $closure_get_env (local.get " <> mangleName name <> ")) (i32.const " <> show idx <> ") (local.get " <> mangleName name <> "))"
                               Nothing -> ""
                           Nothing -> ""
                       _ -> ""
                 in "(block (result i32)\n" <>
                    "        (local.set " <> mangleName name <> " " <> valueCode <> ")" <> selfRefPatch <> "\n" <>
                    "        " <> restCode <> ")"
               Nothing -> restCode
           _ ->
             -- Complex pattern - bind scrutinee to temp, then pattern match
             let patBindCode = genPatBindInner ctx' "(local.get $__rec_base)" bind.pattern
             in "(block (result i32)\n" <>
                "        (local.set $__rec_base " <> valueCode <> ")\n" <>
                (if patBindCode == "" then "" else "        " <> patBindCode <> "\n") <>
                "        " <> restCode <> ")"

-- | Generate binding for a sub-pattern at a given index in a constructor
genPatternBinding :: WasmCtx -> String -> Int -> Pattern -> String
genPatternBinding ctx scrutCode idx pat =
  let fieldExpr = "(call $tuple_get " <> scrutCode <> " (i32.const " <> show (idx + 1) <> "))"
  in genPatBindInner ctx fieldExpr pat

-- | Generate binding code for a pattern given the expression to bind
genPatBindInner :: WasmCtx -> String -> Pattern -> String
genPatBindInner ctx expr (PatVar name) =
  case Map.lookup name ctx.locals of
    Just _ -> "(local.set " <> mangleName name <> " " <> expr <> ")"
    Nothing -> ""
genPatBindInner _ _ PatWildcard = ""
genPatBindInner _ _ (PatLit _) = ""
genPatBindInner ctx expr (PatCon name subPats) =
  -- For nested constructors like (Tuple tok state'), we need to:
  -- 1. The expr is already a tuple with ctor tag at 0, args at 1+
  -- 2. Extract each sub-pattern from fields 1, 2, etc.
  case Map.lookup name ctx.dataConstructors of
    Just { arity } | arity > 0 ->
      let subBindings = Array.mapWithIndex (\i subPat ->
            let subExpr = "(call $tuple_get " <> expr <> " (i32.const " <> show (i + 1) <> "))"
            in genPatBindInner ctx subExpr subPat) subPats
      in intercalate "\n        " (Array.filter (_ /= "") subBindings)
    _ -> ""
genPatBindInner ctx expr (PatParens p) = genPatBindInner ctx expr p
genPatBindInner ctx expr (PatAs name p) =
  let nameBinding = case Map.lookup name ctx.locals of
        Just _ -> "(local.set " <> mangleName name <> " " <> expr <> ")"
        Nothing -> ""
      subBinding = genPatBindInner ctx expr p
  in if nameBinding == "" then subBinding
     else if subBinding == "" then nameBinding
     else nameBinding <> "\n        " <> subBinding
genPatBindInner _ _ _ = ""

-- | Generate case expression
genCaseExpr :: WasmCtx -> Expr -> Array CaseClause -> String
genCaseExpr ctx scrutinee clauses =
  let scrutCode = genExpr ctx scrutinee
  in genCaseClauses ctx scrutCode clauses

genCaseClauses :: WasmCtx -> String -> Array CaseClause -> String
genCaseClauses ctx scrutCode clauses =
  case Array.uncons clauses of
    Nothing -> "(unreachable)"
    Just { head: clause, tail: rest } ->
      let vars = patternVars clause.pattern
          ctx' = foldr (\n c ->
            if Map.member n c.locals then c
            else c { locals = Map.insert n c.localCount c.locals, localCount = c.localCount + 1 }) ctx vars
          bodyCode = genExpr ctx' clause.body
          fallback = genCaseClauses ctx scrutCode rest
          -- Wrap body with guard check if guard is present
          guardedBody = case clause.guard of
            Nothing -> bodyCode
            Just guardExpr ->
              let guardCode = genExpr ctx' guardExpr
              in "(if (result i32) (call $unbox_bool " <> guardCode <> ")\n" <>
                 "        (then " <> bodyCode <> ")\n" <>
                 "        (else " <> fallback <> "))"
      in case clause.pattern of
           PatVar name ->
             case Map.lookup name ctx'.locals of
               Just _ ->
                 "(block (result i32)\n" <>
                 "        (local.set " <> mangleName name <> " " <> scrutCode <> ")\n" <>
                 "        " <> guardedBody <> ")"
               Nothing -> guardedBody
           PatWildcard -> guardedBody
           PatLit lit ->
             let check = "(i32.eq " <> scrutCode <> " " <> genLiteral lit <> ")"
             in "(if (result i32) " <> check <> "\n" <>
                "        (then " <> guardedBody <> ")\n" <>
                "        (else " <> fallback <> "))"
           PatCon name subPats ->
             case Map.lookup name ctx.dataConstructors of
               Just { tag, arity } ->
                 -- Nullary constructors are ctor values directly
                 -- Non-nullary constructors are heap pointers to tuples with ctor tag in field 0
                 let check = if arity == 0
                       then "(i32.and (call $is_ctor " <> scrutCode <> ")\n" <>
                            "                  (i32.eq (call $get_ctor_tag " <> scrutCode <> ") (i32.const " <> show tag <> ")))"
                       else "(i32.and (i32.eq (i32.and " <> scrutCode <> " (i32.const 3)) (i32.const 2))\n" <>  -- is heap ptr (tag 2)
                            "                  (i32.and (call $is_ctor (call $tuple_get " <> scrutCode <> " (i32.const 0)))\n" <>
                            "                           (i32.eq (call $get_ctor_tag (call $tuple_get " <> scrutCode <> " (i32.const 0))) (i32.const " <> show tag <> "))))"
                     -- Generate bindings for sub-patterns
                     -- For non-nullary ctor, field 0 is ctor tag, fields 1+ are arguments
                     bindings = if arity == 0 then ""
                       else intercalate "\n        " (Array.mapWithIndex (genPatternBinding ctx' scrutCode) subPats)
                     bodyWithBindings = if bindings == "" then guardedBody
                       else "(block (result i32)\n        " <> bindings <> "\n        " <> guardedBody <> ")"
                 in "(if (result i32) " <> check <> "\n" <>
                    "        (then " <> bodyWithBindings <> ")\n" <>
                    "        (else " <> fallback <> "))"
               Nothing ->
                 if Array.null rest then guardedBody
                 else "(if (result i32) (i32.const 1)\n" <>
                      "        (then " <> guardedBody <> ")\n" <>
                      "        (else " <> fallback <> "))"
           _ ->
             if Array.null rest
             then guardedBody
             else "(if (result i32) (i32.const 1)\n" <>
                  "        (then " <> guardedBody <> ")\n" <>
                  "        (else " <> fallback <> "))"

-- | Generate binary operator
genBinOp :: WasmCtx -> String -> Expr -> Expr -> String
genBinOp ctx op l r =
  let lCode = genExpr ctx l
      rCode = genExpr ctx r
  in case op of
    "+" -> "(call $make_int (i32.add (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    "-" -> "(call $make_int (i32.sub (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    "*" -> "(call $make_int (i32.mul (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    "/" -> "(call $make_int (i32.div_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    "mod" -> "(call $make_int (i32.rem_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    "==" -> "(call $rt_generic_eq " <> lCode <> " " <> rCode <> ")"
    "/=" -> "(call $rt_generic_ne " <> lCode <> " " <> rCode <> ")"
    "<" -> "(call $make_bool (i32.lt_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    ">" -> "(call $make_bool (i32.gt_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    "<=" -> "(call $make_bool (i32.le_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    ">=" -> "(call $make_bool (i32.ge_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    "&&" -> "(if (result i32) (call $unbox_bool " <> lCode <> ")\n" <>
            "      (then " <> rCode <> ")\n" <>
            "      (else (call $make_bool (i32.const 0))))"
    "||" -> "(if (result i32) (call $unbox_bool " <> lCode <> ")\n" <>
            "      (then (call $make_bool (i32.const 1)))\n" <>
            "      (else " <> rCode <> "))"
    "<>" -> "(call $rt_string_append " <> lCode <> " " <> rCode <> ")"
    ":" -> -- Cons
      "(block (result i32)\n" <>
      "      (local.set $__tmp (call $alloc_tuple (i32.const 3)))\n" <>
      "      (call $tuple_set (local.get $__tmp) (i32.const 0) (call $make_ctor (i32.const 1) (i32.const 2)))\n" <>
      "      (call $tuple_set (local.get $__tmp) (i32.const 1) " <> lCode <> ")\n" <>
      "      (call $tuple_set (local.get $__tmp) (i32.const 2) " <> rCode <> ")\n" <>
      "      (local.get $__tmp))"
    "$" -> "(call $rt_apply_closure " <> lCode <> " " <> rCode <> ")"
    "<<<" -> lCode
    ">>>" -> rCode
    _ ->
      -- Handle qualified operators like "Array.elem" used with backticks
      -- These should be treated as curried function applications
      case String.indexOf (StringPattern.Pattern ".") op of
        Just idx ->
          let modName = String.take idx op
              funcName = String.drop (idx + 1) op
              callName = "$" <> modName <> "_" <> funcName
          in "(call $rt_apply_closure (call $rt_apply_closure (call " <> callName <> ") " <> lCode <> ") " <> rCode <> ")"
        Nothing ->
          -- Unknown operator, try to call as a function
          "(call $rt_apply_closure (call $rt_apply_closure (call $" <> op <> ") " <> lCode <> ") " <> rCode <> ")"

-- | Generate do statements
genDoStmts :: WasmCtx -> Array DoStatement -> String
genDoStmts ctx stmts =
  case Array.uncons stmts of
    Nothing -> "(call $make_unit)"
    Just { head: stmt, tail: [] } ->
      case stmt of
        DoExpr e -> genExpr ctx e
        DoLet binds -> genExpr ctx (ExprLet binds (ExprLit (LitInt 0)))
        DoBind _ e -> genExpr ctx e
    Just { head: stmt, tail: rest } ->
      case stmt of
        DoExpr e ->
          "(block (result i32)\n" <>
          "        (drop " <> genExpr ctx e <> ")\n" <>
          "        " <> genDoStmts ctx rest <> ")"
        DoLet binds ->
          genLetBinds ctx binds (ExprDo rest)
        DoBind pat e ->
          let vars = patternVars pat
              ctx' = foldr (\n c ->
                if Map.member n c.locals then c
                else c { locals = Map.insert n c.localCount c.locals, localCount = c.localCount + 1 }) ctx vars
              -- For Maybe monad: check if Nothing, return Nothing; else extract from Just
              exprCode = genExpr ctx e
              restCode = genDoStmts ctx' rest
          in case pat of
               PatVar name ->
                 case Map.lookup name ctx'.locals of
                   Just _ ->
                     -- Monad handling for Maybe/Either: x <- expr becomes:
                     -- Store expr result in __tmp, check for Nothing/Left (error), unwrap Just/Right
                     -- Maybe Nothing: tag 3 with ctor tag 0 (direct ctor)
                     -- Either Left: tag 2 with tuple[0] ctor tag 0 (heap tuple)
                     "(block (result i32)\n" <>
                     "        (local.set $__tmp " <> exprCode <> ")\n" <>
                     "        ;; Check if Nothing (tag 3, ctor 0) OR Left (tag 2, tuple[0] ctor 0)\n" <>
                     "        (if (result i32) (i32.or\n" <>
                     "              (i32.and (i32.eq (i32.and (local.get $__tmp) (i32.const 3)) (i32.const 3))\n" <>
                     "                       (i32.eq (call $get_ctor_tag (local.get $__tmp)) (i32.const 0)))\n" <>
                     "              (i32.and (i32.eq (i32.and (local.get $__tmp) (i32.const 3)) (i32.const 2))\n" <>
                     "                       (i32.and (call $is_ctor (call $tuple_get (local.get $__tmp) (i32.const 0)))\n" <>
                     "                                (i32.eq (call $get_ctor_tag (call $tuple_get (local.get $__tmp) (i32.const 0))) (i32.const 0)))))\n" <>
                     "          (then (local.get $__tmp)) ;; return Nothing/Left unchanged\n" <>
                     "          (else\n" <>
                     "            ;; Extract value: from Just (direct) or Right (tuple field 1)\n" <>
                     "            (local.set " <> mangleName name <> " (if (result i32) (i32.eq (i32.and (local.get $__tmp) (i32.const 3)) (i32.const 3))\n" <>
                     "              (then (call $tuple_get (local.get $__tmp) (i32.const 1))) ;; This shouldn't happen - Nothing has no value\n" <>
                     "              (else (call $tuple_get (local.get $__tmp) (i32.const 1)))))\n" <>
                     "            " <> restCode <> ")))"
                   Nothing ->
                     "(block (result i32)\n" <>
                     "        (local.set $__tmp " <> exprCode <> ")\n" <>
                     "        ;; Check if Nothing (tag 3, ctor 0) OR Left (tag 2, tuple[0] ctor 0)\n" <>
                     "        (if (result i32) (i32.or\n" <>
                     "              (i32.and (i32.eq (i32.and (local.get $__tmp) (i32.const 3)) (i32.const 3))\n" <>
                     "                       (i32.eq (call $get_ctor_tag (local.get $__tmp)) (i32.const 0)))\n" <>
                     "              (i32.and (i32.eq (i32.and (local.get $__tmp) (i32.const 3)) (i32.const 2))\n" <>
                     "                       (i32.and (call $is_ctor (call $tuple_get (local.get $__tmp) (i32.const 0)))\n" <>
                     "                                (i32.eq (call $get_ctor_tag (call $tuple_get (local.get $__tmp) (i32.const 0))) (i32.const 0)))))\n" <>
                     "          (then (local.get $__tmp))\n" <>
                     "          (else " <> restCode <> ")))"
               PatCon "Tuple" pats ->
                 -- Tuple pattern: extract fields from the tuple
                 -- The expr result is Right (Tuple ...), we extract field 1 (the tuple)
                 -- then extract each field from that tuple
                 let extractBindings = intercalate "\n" $
                       Array.mapWithIndex (\idx p -> case p of
                         PatVar pname ->
                           "            (local.set " <> mangleName pname <> " (call $tuple_get (local.get $__rec_base) (i32.const " <> show (idx + 1) <> ")))"
                         _ -> "") pats
                 in "(block (result i32)\n" <>
                    "        (local.set $__tmp " <> exprCode <> ")\n" <>
                    "        ;; Check if Nothing (tag 3, ctor 0) OR Left (tag 2, tuple[0] ctor 0)\n" <>
                    "        (if (result i32) (i32.or\n" <>
                    "              (i32.and (i32.eq (i32.and (local.get $__tmp) (i32.const 3)) (i32.const 3))\n" <>
                    "                       (i32.eq (call $get_ctor_tag (local.get $__tmp)) (i32.const 0)))\n" <>
                    "              (i32.and (i32.eq (i32.and (local.get $__tmp) (i32.const 3)) (i32.const 2))\n" <>
                    "                       (i32.and (call $is_ctor (call $tuple_get (local.get $__tmp) (i32.const 0)))\n" <>
                    "                                (i32.eq (call $get_ctor_tag (call $tuple_get (local.get $__tmp) (i32.const 0))) (i32.const 0)))))\n" <>
                    "          (then (local.get $__tmp)) ;; return Nothing/Left unchanged\n" <>
                    "          (else\n" <>
                    "            ;; Extract from Right (tuple field 1), then extract tuple fields\n" <>
                    "            (local.set $__rec_base (call $tuple_get (local.get $__tmp) (i32.const 1)))\n" <>
                    extractBindings <> "\n" <>
                    "            " <> restCode <> ")))"
               _ ->
                 "(block (result i32)\n" <>
                 "        (local.set $__tmp " <> exprCode <> ")\n" <>
                 "        ;; Check if Nothing (tag 3, ctor 0) OR Left (tag 2, tuple[0] ctor 0)\n" <>
                 "        (if (result i32) (i32.or\n" <>
                 "              (i32.and (i32.eq (i32.and (local.get $__tmp) (i32.const 3)) (i32.const 3))\n" <>
                 "                       (i32.eq (call $get_ctor_tag (local.get $__tmp)) (i32.const 0)))\n" <>
                 "              (i32.and (i32.eq (i32.and (local.get $__tmp) (i32.const 3)) (i32.const 2))\n" <>
                 "                       (i32.and (call $is_ctor (call $tuple_get (local.get $__tmp) (i32.const 0)))\n" <>
                 "                                (i32.eq (call $get_ctor_tag (call $tuple_get (local.get $__tmp) (i32.const 0))) (i32.const 0)))))\n" <>
                 "          (then (local.get $__tmp))\n" <>
                 "          (else " <> restCode <> ")))"
