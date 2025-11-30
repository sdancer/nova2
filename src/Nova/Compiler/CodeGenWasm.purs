module Nova.Compiler.CodeGenWasm where

import Prelude
import Data.Array (intercalate, length, (:))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust)
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
import Nova.Compiler.Ast (Module, Declaration(..), FunctionDeclaration, Expr(..), Pattern(..), Literal(..), LetBind, CaseClause, DoStatement(..), TypeExpr, DataType, DataConstructor)
import Nova.Compiler.RefEq (refEqExpr)

unsafeHead :: forall a. Array a -> a
unsafeHead arr = fromMaybe (unsafeHead arr) (Array.head arr)

unsafeTail :: forall a. Array a -> Array a
unsafeTail arr = fromMaybe [] (Array.tail arr)

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
      newLocals = Map.insert name ctx'.localCount ctx'.locals
      newCount = ctx'.localCount + 1
  in if Map.member name ctx'.locals then ctx' else ctx' { locals = newLocals, localCount = newCount }
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

-- | Helper for getData in collectDataCtors
getDataDecl :: Declaration -> Maybe DataType
getDataDecl (DeclDataType d) = Just d
getDataDecl _ = Nothing

-- | Helper to add constructors
addDataCtors :: Map String { tag :: Int, arity :: Int } -> DataType -> Map String { tag :: Int, arity :: Int }
addDataCtors m d = let insertCtor = \acc tuple -> let i = fstTupleIntCtor tuple in let ctor = sndTupleIntCtor tuple in Map.insert ctor.name { tag: i, arity: length ctor.fields } acc in Array.foldl insertCtor m (Array.mapWithIndex Tuple d.constructors)

fstTupleIntCtor :: Tuple Int DataConstructor -> Int
fstTupleIntCtor (Tuple i _) = i

sndTupleIntCtor :: Tuple Int DataConstructor -> DataConstructor
sndTupleIntCtor (Tuple _ r) = r

-- | Collect data constructors from module
collectDataCtors :: Array Declaration -> Map String { tag :: Int, arity :: Int }
collectDataCtors decls =
  let datas = Array.mapMaybe getDataDecl decls
  in Array.foldl addDataCtors Map.empty datas

-- | Collect strings from declaration
collectDeclStrings :: Declaration -> Array String
collectDeclStrings (DeclFunction f) = collectStrings f.body
collectDeclStrings _ = []

-- | Helper for building string table
addStringEntry :: { table :: Map String { offset :: Int, len :: Int }, offset :: Int } -> String -> { table :: Map String { offset :: Int, len :: Int }, offset :: Int }
addStringEntry acc s =
  let slen = String.length s
      newTable = Map.insert s { offset: acc.offset, len: slen } acc.table
  in { table: newTable, offset: acc.offset + slen }

-- | Build string table from list of unique strings
-- Returns map from string -> { offset, len } and total size
buildStringTable :: Array String -> { table :: Map String { offset :: Int, len :: Int }, totalSize :: Int }
buildStringTable strs =
  let initial = { table: Map.empty, offset: 1024 }
      result = Array.foldl addStringEntry initial strs
  in { table: result.table, totalSize: result.offset - 1024 }

-- | Generate data segment for strings
genDataSegment :: Array String -> Int -> String
genDataSegment strs baseOffset =
  if Array.null strs then "" else "  ;; String data\n  (data (i32.const " <> show baseOffset <> ") \"" <> intercalate "" (map escapeWasmString strs) <> "\")\n\n"

-- | Escape a string for WASM data segment
escapeWasmString :: String -> String
escapeWasmString s = String.replaceAll (String.Pattern "\r") (String.Replacement "\\r") (String.replaceAll (String.Pattern "\t") (String.Replacement "\\t") (String.replaceAll (String.Pattern "\n") (String.Replacement "\\n") (String.replaceAll (String.Pattern "\"") (String.Replacement "\\\"") (String.replaceAll (String.Pattern "\\") (String.Replacement "\\\\") s))))

-- | Generate module
genModule :: Module -> String
genModule m = let modName = m.name in let allFuncs = Array.mapMaybe getFuncDecl m.declarations in let grouped = groupFunctions allFuncs in let uniqueFuncs = Array.nubByEq (\a b -> a.name == b.name && a.arity == b.arity) (map (\g -> { name: g.name, arity: g.arity }) grouped) in let exports = intercalate "\n  " (map genExport uniqueFuncs) in let ctors = collectDataCtors m.declarations in let allExternalRefs = Array.nub (Array.concatMap collectDeclRefs m.declarations) in let externalImports = genExternalImports allExternalRefs in let allStrings = Array.nub (Array.concatMap collectDeclStrings m.declarations) in let stringTableData = buildStringTable allStrings in let dataSegment = genDataSegment allStrings 1024 in let lambdaCollector = Array.foldl (\s d -> collectDeclLambdas d s) { counter: 0, lambdas: [] } m.declarations in let allLambdas = lambdaCollector.lambdas in let funcArityMap = Map.fromFoldable (map (\f -> Tuple f.name f.arity) uniqueFuncs) in let funcsNeedingWrappers = Array.filter (\f -> f.arity == 1) uniqueFuncs in let numLambdas = length allLambdas in let funcWrapperIdxMap = Map.fromFoldable (Array.mapWithIndex (\i f -> Tuple f.name (numLambdas + i)) funcsNeedingWrappers) in let ctx = (emptyCtx modName) { moduleFuncs = Set.fromFoldable (map (\f -> f.name) uniqueFuncs), funcArities = funcArityMap, dataConstructors = Map.union ctors preludeConstructors, stringTable = stringTableData.table, lambdas = allLambdas, funcWrapperIdx = funcWrapperIdxMap } in let funcDefs = intercalate "\n\n" (map (genFunctionGroup ctx) grouped) in let lambdaDefs = if Array.null allLambdas then "" else "\n\n  ;; Lifted lambdas\n" <> intercalate "\n\n" (map (genLiftedLambda ctx) allLambdas) in let wrapperDefs = if Array.null funcsNeedingWrappers then "" else "\n\n  ;; Function wrappers (for function-as-value)\n" <> intercalate "\n\n" (map genFunctionWrapper funcsNeedingWrappers) in let tableDecl = genFunctionTableWithWrappers allLambdas funcsNeedingWrappers in "(module\n" <> "  ;; Module: " <> m.name <> "\n\n" <> genImports <> "\n\n" <> externalImports <> "  (memory (export \"memory\") 1)\n" <> "  (global $heap_ptr (mut i32) (i32.const 1024))\n\n" <> tableDecl <> dataSegment <> "  ;; Runtime helpers\n" <> genRuntimeHelpers <> "\n\n" <> "  ;; Functions\n" <> funcDefs <> lambdaDefs <> wrapperDefs <> "\n\n" <> "  ;; Exports\n  " <> exports <> "\n" <> ")\n"

-- | Extract function from declaration for genModule
getFuncDecl :: Declaration -> Maybe FunctionDeclaration
getFuncDecl (DeclFunction f) = Just f
getFuncDecl _ = Nothing

-- | Generate function table for indirect calls
genFunctionTable :: Array LiftedLambda -> String
genFunctionTable lambdas = let funcNames = map (\l -> mangleName ("__lambda_" <> show l.id)) lambdas in let funcList = intercalate " " funcNames in let n = max 1 (length lambdas) in "  ;; Function table for indirect calls\n" <> "  (type $closure_type (func (param i32 i32) (result i32)))\n" <> "  (table (export \"__indirect_function_table\") " <> show n <> " funcref)\n" <> (if length lambdas > 0 then "  (elem (i32.const 0) func " <> funcList <> ")\n\n" else "\n")

-- | Generate function table including function wrappers
genFunctionTableWithWrappers :: Array LiftedLambda -> Array { name :: String, arity :: Int } -> String
genFunctionTableWithWrappers lambdas wrappers = let lambdaNames = map (\l -> mangleName ("__lambda_" <> show l.id)) lambdas in let wrapperNames = map (\f -> mangleName ("__fn_wrap_" <> f.name)) wrappers in let allFuncNames = lambdaNames <> wrapperNames in let funcList = intercalate " " allFuncNames in let n = max 1 (length allFuncNames) in "  ;; Function table for indirect calls\n" <> "  (type $closure_type (func (param i32 i32) (result i32)))\n" <> "  (table (export \"__indirect_function_table\") " <> show n <> " funcref)\n" <> (if length allFuncNames > 0 then "  (elem (i32.const 0) func " <> funcList <> ")\n\n" else "\n")

-- | Generate a wrapper function for function-as-value (arity 1 only for now)
-- Wrapper takes (env, arg) and calls the actual function with arg
genFunctionWrapper :: { name :: String, arity :: Int } -> String
genFunctionWrapper f = let wrapperName = "__fn_wrap_" <> f.name in let actualFuncName = mangleName f.name in "  (func " <> mangleName wrapperName <> " (param $__env i32) (param $__arg i32) (result i32)\n" <> "    (call " <> actualFuncName <> " (local.get $__arg))\n" <> "  )"

-- | Generate a lifted lambda function
genLiftedLambda :: WasmCtx -> LiftedLambda -> String
genLiftedLambda ctx lambda = let funcName = "__lambda_" <> show lambda.id in let allParams = ["__env"] <> lambda.params in let params = map (\n -> "(param " <> mangleName n <> " i32)") allParams in let paramsStr = intercalate " " params in let capturedVarLocals = Map.fromFoldable (Array.mapWithIndex (\i n -> Tuple n (i + 1 + length lambda.params)) lambda.freeVars) in let paramLocals = Map.fromFoldable (Array.mapWithIndex (\i n -> Tuple n i) allParams) in let allLocals = Map.union paramLocals capturedVarLocals in let bodyLocals = collectLocals lambda.body in let extraLocals = Array.nub (Array.filter (\n -> not (Map.member n allLocals)) bodyLocals) in let localCount = length allParams + length lambda.freeVars + length extraLocals in let localCtx = ctx { locals = foldr (\n m -> if Map.member n m then m else Map.insert n (Map.size m) m) allLocals extraLocals, localCount = localCount } in let extractCaptures = if Array.null lambda.freeVars then "" else intercalate "\n    " (Array.mapWithIndex (\i n -> "(local.set " <> mangleName n <> " (call $tuple_get (local.get $__env) (i32.const " <> show i <> ")))") lambda.freeVars) <> "\n    " in let localDecls = map (\n -> "(local " <> mangleName n <> " i32)") (lambda.freeVars <> extraLocals <> ["__tmp", "__rec_base", "__arg0", "__arg1", "__arg2", "__arg3", "__arg4"]) in let localsStr = intercalate " " localDecls in let bodyCode = genExpr localCtx lambda.body in "  (func " <> mangleName funcName <> " " <> paramsStr <> " (result i32)\n" <> "    " <> localsStr <> "\n" <> "    " <> extractCaptures <> bodyCode <> "\n" <> "  )"

-- | Generate imports for external module functions
genExternalImports :: Array (Tuple String String) -> String
genExternalImports refs = if Array.null refs then "" else "  ;; External module imports\n" <> intercalate "\n" (map (\(Tuple modName name) -> let fullName = modName <> "_" <> name in "  (import \"modules\" \"" <> fullName <> "\" (func " <> mangleName fullName <> " (result i32)))") refs) <> "\n\n"

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
genFunction ctx func = let paramNames = Array.mapWithIndex getParamName func.parameters in let params = map (\n -> "(param " <> mangleName n <> " i32)") paramNames in let paramsStr = intercalate " " params in let paramLocals = Map.fromFoldable (Array.mapWithIndex (\i n -> Tuple n i) paramNames) in let bodyLocals = Array.nub (collectLocals func.body) in let argSlots = ["__arg0", "__arg1", "__arg2", "__arg3", "__arg4"] in let extraLocals = Array.nub (Array.filter (\n -> not (Array.elem n paramNames)) ("__tmp" : "__rec_base" : argSlots <> bodyLocals)) in let allLocals = Map.union paramLocals (Map.fromFoldable (Array.mapWithIndex (\i n -> Tuple n (i + length paramNames)) extraLocals)) in let localDecls = "\n    " <> intercalate " " (map (\n -> "(local " <> mangleName n <> " i32)") extraLocals) in let ctxWithParams = ctx { locals = allLocals, localCount = Map.size allLocals } in let bodyCode = genExpr ctxWithParams func.body in "  (func " <> mangleName func.name <> " " <> paramsStr <> " (result i32)" <> localDecls <> "\n" <> "    " <> bodyCode <> "\n" <> "  )"

-- | Generate merged function with pattern matching
genMergedFunction :: WasmCtx -> FunctionGroup -> String
genMergedFunction ctx group = let paramNames = Array.range 0 (group.arity - 1) # map (\i -> "p" <> show i) in let params = map (\n -> "(param " <> mangleName n <> " i32)") paramNames in let paramsStr = intercalate " " params in let paramLocals = Map.fromFoldable (Array.mapWithIndex (\i n -> Tuple n i) paramNames) in let clauseLocals = Array.concatMap (\c -> Array.concatMap patternVars c.parameters <> collectLocals c.body) group.clauses in let argSlots = ["__arg0", "__arg1", "__arg2", "__arg3", "__arg4"] in let extraLocals = Array.nub (Array.filter (\n -> not (Array.elem n paramNames)) ("__tmp" : "__rec_base" : argSlots <> clauseLocals)) in let allLocals = Map.union paramLocals (Map.fromFoldable (Array.mapWithIndex (\i n -> Tuple n (i + length paramNames)) extraLocals)) in let localDecls = "\n    " <> intercalate " " (map (\n -> "(local " <> mangleName n <> " i32)") extraLocals) in let ctxWithParams = ctx { locals = allLocals, localCount = Map.size allLocals } in let matchCode = genPatternMatch ctxWithParams paramNames group.clauses in "  (func " <> mangleName group.name <> " " <> paramsStr <> " (result i32)" <> localDecls <> "\n" <> "    " <> matchCode <> "\n" <> "  )"

-- | Generate pattern matching code
genPatternMatch :: WasmCtx -> Array String -> Array FunctionDeclaration -> String
genPatternMatch ctx paramNames clauses = case Array.uncons clauses of
  Nothing -> "(unreachable)"
  Just { head: clause, tail: rest } -> let bindCode = genPatternBindings ctx paramNames clause.parameters in let bodyCode = genExpr ctx clause.body in let testCode = genPatternTest ctx paramNames clause.parameters in let fallback = genPatternMatch ctx paramNames rest in if Array.null rest then bindCode <> bodyCode else "(if (result i32) " <> testCode <> "\n" <> "      (then " <> bindCode <> bodyCode <> ")\n" <> "      (else " <> fallback <> "))"

-- | Generate pattern test
genPatternTest :: WasmCtx -> Array String -> Array Pattern -> String
genPatternTest ctx paramNames patterns = let tests = Array.mapWithIndex (\i pat -> genSinglePatternTest ctx ("$p" <> show i) pat) (Array.zip paramNames patterns # map (\(Tuple _ p) -> p)) in let validTests = Array.filter (_ /= "(i32.const 1)") tests in if Array.null validTests then "(i32.const 1)" else intercalate "\n        (i32.and " validTests <> String.joinWith "" (Array.replicate (length validTests - 1) ")")

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
genPatternBindings ctx paramNames patterns = let bindings = Array.concatMap (\(Tuple pname pat) -> genSinglePatternBinding ctx pname pat) (Array.zip paramNames patterns) in intercalate "\n        " bindings

-- | Bind a pattern to a field extraction expression
genPatternFieldBinding :: WasmCtx -> String -> Pattern -> Array String
genPatternFieldBinding ctx expr (PatVar name) = if isJust (Map.lookup name ctx.locals) then ["(local.set " <> mangleName name <> " " <> expr <> ")"] else []
genPatternFieldBinding _ _ PatWildcard = []
genPatternFieldBinding _ _ (PatLit _) = []
genPatternFieldBinding _ _ (PatCon _ _) = [] -- Nested constructor matching not yet supported
genPatternFieldBinding ctx expr (PatAs name p) = (if isJust (Map.lookup name ctx.locals) then ["(local.set " <> mangleName name <> " " <> expr <> ")"] else []) <> genPatternFieldBinding ctx expr p
genPatternFieldBinding ctx expr (PatParens p) = genPatternFieldBinding ctx expr p
genPatternFieldBinding _ _ _ = []

-- | Generate binding for single pattern
genSinglePatternBinding :: WasmCtx -> String -> Pattern -> Array String
genSinglePatternBinding ctx param (PatVar name) = if isJust (Map.lookup name ctx.locals) && name /= param then ["(local.set " <> mangleName name <> " (local.get " <> mangleName param <> "))"] else []
genSinglePatternBinding _ _ PatWildcard = []
genSinglePatternBinding _ _ (PatLit _) = []
genSinglePatternBinding ctx param (PatCon _ subPats) = Array.concatMap (\(Tuple i p) -> let fieldExpr = "(call $tuple_get (local.get " <> mangleName param <> ") (i32.const " <> show (i + 1) <> "))" in genPatternFieldBinding ctx fieldExpr p) (Array.mapWithIndex Tuple subPats)
genSinglePatternBinding ctx param (PatAs name p) = (if isJust (Map.lookup name ctx.locals) then ["(local.set " <> mangleName name <> " (local.get " <> mangleName param <> "))"] else []) <> genSinglePatternBinding ctx param p
genSinglePatternBinding ctx param (PatParens p) = genSinglePatternBinding ctx param p
genSinglePatternBinding _ _ _ = []

-- | Flatten a chain of function applications into (func, [args])
flattenApp :: Expr -> Tuple Expr (Array Expr)
flattenApp (ExprApp f a) = let Tuple baseFunc args = flattenApp f in Tuple baseFunc (args <> [a])
flattenApp e = Tuple e []

-- | Generate a chain of closure applications
genClosureApps :: WasmCtx -> Expr -> Array Expr -> String
genClosureApps ctx baseExpr args = genClosureAppsHelper ctx (genExpr ctx baseExpr) args

genClosureAppsHelper :: WasmCtx -> String -> Array Expr -> String
genClosureAppsHelper ctx acc args = if Array.null args then acc else let firstCall = "(call $rt_apply_closure " <> acc <> " " <> genExpr ctx (unsafeHead args) <> ")" in genClosureAppsHelper ctx firstCall (unsafeTail args)

genExprVarLocal :: String -> String
genExprVarLocal name = "(local.get " <> mangleName name <> ")"

genExprVarModuleFunc :: WasmCtx -> String -> String
genExprVarModuleFunc ctx name = genExprVarModuleFuncArity ctx name (Map.lookup name ctx.funcArities)

genExprVarModuleFuncArity :: WasmCtx -> String -> Maybe Int -> String
genExprVarModuleFuncArity _ name (Just 0) = "(call " <> mangleName name <> ")"
genExprVarModuleFuncArity ctx name (Just 1) = genExprVarArity1 (Map.lookup name ctx.funcWrapperIdx)
genExprVarModuleFuncArity _ _ (Just _) = "(call $rt_make_closure (i32.const 0) (call $alloc_tuple (i32.const 0)))"
genExprVarModuleFuncArity _ name Nothing = "(call " <> mangleName name <> ")"

genExprVarArity1 :: Maybe Int -> String
genExprVarArity1 (Just wrapperIdx) = "(call $rt_make_closure (i32.const " <> show wrapperIdx <> ") (call $alloc_tuple (i32.const 0)))"
genExprVarArity1 Nothing = "(call $rt_make_closure (i32.const 0) (call $alloc_tuple (i32.const 0)))"

genExprVarDataCtor :: Maybe { tag :: Int, arity :: Int } -> String -> String
genExprVarDataCtor (Just { tag, arity: 0 }) _ = "(call $make_ctor (i32.const " <> show tag <> ") (i32.const 0))"
genExprVarDataCtor (Just _) _ = "(i32.const 0)"
genExprVarDataCtor Nothing name = genExprVarQualified (String.indexOf (StringPattern.Pattern ".") name) name

genExprVarQualified :: Maybe Int -> String -> String
genExprVarQualified (Just idx) name = let callName = "$" <> String.take idx name <> "_" <> String.drop (idx + 1) name in "(call " <> callName <> ")"
genExprVarQualified Nothing _ = "(i32.const 0)"

genExprVar :: WasmCtx -> String -> String
genExprVar ctx name = if isJust (Map.lookup name ctx.locals) then genExprVarLocal name else if Set.member name ctx.moduleFuncs then genExprVarModuleFunc ctx name else genExprVarDataCtor (Map.lookup name ctx.dataConstructors) name

-- String literals need special handling to use the stringTable
genExprLitString :: Maybe { offset :: Int, len :: Int } -> String
genExprLitString (Just { offset, len }) = "(call $rt_make_string (i32.const " <> show offset <> ") (i32.const " <> show len <> "))"
genExprLitString Nothing = "(call $make_unit)"

genExprAppModuleFunc :: WasmCtx -> String -> Array Expr -> Maybe Int -> String
genExprAppModuleFunc ctx name allArgs (Just arity) = if length allArgs == arity then "(call " <> mangleName name <> " " <> intercalate " " (map (genExpr ctx) allArgs) <> ")" else genClosureApps ctx (ExprVar name) allArgs
genExprAppModuleFunc ctx name allArgs Nothing = genClosureApps ctx (ExprVar name) allArgs

genExprAppDataCtorCode :: WasmCtx -> Int -> Int -> Array Expr -> String
genExprAppDataCtorCode ctx tag arity allArgs = let n = arity + 1 in let argCodes = map (genExpr ctx) allArgs in let argSlots = Array.mapWithIndex (\i _ -> "$__arg" <> show i) allArgs in let evalCode = Array.zipWith (\slot code -> "(local.set " <> slot <> " " <> code <> ")") argSlots argCodes in let setCode = Array.mapWithIndex (\i slot -> "(call $tuple_set (local.get $__tmp) (i32.const " <> show (i + 1) <> ") (local.get " <> slot <> "))") argSlots in "(block (result i32)\n        " <> intercalate "\n        " evalCode <> "\n        (local.set $__tmp (call $alloc_tuple (i32.const " <> show n <> ")))\n        (call $tuple_set (local.get $__tmp) (i32.const 0) (call $make_ctor (i32.const " <> show tag <> ") (i32.const " <> show arity <> ")))\n        " <> intercalate "\n        " setCode <> "\n        (local.get $__tmp))"

genExprAppDataCtor :: WasmCtx -> String -> Array Expr -> Maybe { tag :: Int, arity :: Int } -> String
genExprAppDataCtor ctx _ allArgs (Just { tag, arity }) = if arity > 0 && length allArgs == arity then genExprAppDataCtorCode ctx tag arity allArgs else genClosureApps ctx (ExprVar "") allArgs
genExprAppDataCtor ctx name allArgs Nothing = genClosureApps ctx (ExprVar name) allArgs

genExprAppVar :: WasmCtx -> String -> Array Expr -> String
genExprAppVar ctx name allArgs = if Set.member name ctx.moduleFuncs && not (Map.member name ctx.locals) then genExprAppModuleFunc ctx name allArgs (Map.lookup name ctx.funcArities) else genExprAppDataCtor ctx name allArgs (Map.lookup name ctx.dataConstructors)

genExprAppBaseFunc :: WasmCtx -> Expr -> Array Expr -> String
genExprAppBaseFunc ctx (ExprVar name) allArgs = genExprAppVar ctx name allArgs
genExprAppBaseFunc ctx baseFunc allArgs = genClosureApps ctx baseFunc allArgs

genLambdaWithCaptures :: WasmCtx -> Array String -> Int -> String
genLambdaWithCaptures ctx freeVars tableIdx = if length freeVars == 0 then "(call $rt_make_closure (i32.const " <> show tableIdx <> ") (call $alloc_tuple (i32.const 0)))" else let n = length freeVars in let setCaptures = Array.mapWithIndex (\i v -> "(call $tuple_set (local.get $__tmp) (i32.const " <> show i <> ") " <> genExpr ctx (ExprVar v) <> ")") freeVars in "(block (result i32)\n        (local.set $__tmp (call $alloc_tuple (i32.const " <> show n <> ")))\n        " <> intercalate "\n        " setCaptures <> "\n        (call $rt_make_closure (i32.const " <> show tableIdx <> ") (local.get $__tmp)))"

genLambdaFallback :: WasmCtx -> Array Pattern -> Expr -> String
genLambdaFallback ctx pats body = let vars = Array.concatMap patternVars pats in let ctx' = foldr (\n c -> if Map.member n c.locals then c else c { locals = Map.insert n c.localCount c.locals, localCount = c.localCount + 1 }) ctx vars in genExpr ctx' body

genLambdaMatch :: WasmCtx -> Array Pattern -> Expr -> Maybe LiftedLambda -> String
genLambdaMatch ctx pats body (Just lambda) = genLambdaWithCaptures ctx lambda.freeVars lambda.id
genLambdaMatch ctx pats body Nothing = genLambdaFallback ctx pats body

genExprListCode :: WasmCtx -> Array Expr -> String
genExprListCode ctx elems = if length elems == 0 then "(call $rt_make_array (i32.const 0))" else let n = length elems in let pushAll = intercalate "\n" $ Array.mapWithIndex (\i e -> "      (drop (call $rt_array_push (local.get $__tmp) " <> genExpr ctx e <> "))") elems in "(block (result i32)\n      (local.set $__tmp (call $rt_make_array (i32.const " <> show n <> ")))\n" <> pushAll <> "\n      (local.get $__tmp))"

genExprTupleCode :: WasmCtx -> Array Expr -> String
genExprTupleCode ctx elems = if length elems == 0 then "(call $make_unit)" else let n = length elems in let setCode = intercalate "\n" (Array.mapWithIndex (\i e -> "      (call $tuple_set (local.get $__tmp) (i32.const " <> show i <> ") " <> genExpr ctx e <> ")") elems) in "(block (result i32)\n      (local.set $__tmp (call $alloc_tuple (i32.const " <> show n <> ")))\n" <> setCode <> "\n      (local.get $__tmp))"

genExprRecordCode :: WasmCtx -> Array (Tuple String Expr) -> String
genExprRecordCode ctx fields = let sortedFields = Array.sortWith (\(Tuple name _) -> name) fields in let n = length sortedFields in let setCode = intercalate "\n" (Array.mapWithIndex (\i (Tuple _ e) -> "      (call $tuple_set (local.get $__tmp) (i32.const " <> show i <> ") " <> genExpr ctx e <> ")") sortedFields) in "(block (result i32)\n      (local.set $__tmp (call $alloc_tuple (i32.const " <> show n <> ")))\n" <> setCode <> "\n      (local.get $__tmp))"

-- | Known field indices based on alphabetical ordering
fieldIndex :: String -> Int
fieldIndex "column" = 0
fieldIndex "input" = 1
fieldIndex "line" = 2
fieldIndex "pos" = 3
fieldIndex "tokenType" = 3
fieldIndex "value" = 4
fieldIndex "arity" = 0
fieldIndex "body" = 0
fieldIndex "clauses" = 0
fieldIndex "declarations" = 0
fieldIndex "expr" = 0
fieldIndex "fields" = 0
fieldIndex "imports" = 1
fieldIndex "moduleName" = 2
fieldIndex "name" = 2
fieldIndex "params" = 3
fieldIndex "pattern" = 3
fieldIndex "patterns" = 3
fieldIndex "tag" = 4
fieldIndex "type" = 4
fieldIndex "typeParams" = 4
fieldIndex "init" = 0
fieldIndex "rest" = 1
fieldIndex _ = 0

-- | Infer record size from field names being updated
inferRecordSize :: Array String -> Int
inferRecordSize fields = if Array.any (\f -> f == "pos" || f == "column" || f == "line" || f == "input") fields then 4 else if Array.any (\f -> f == "tokenType" || f == "value") fields then 5 else let maxIdx = Array.foldl (\acc f -> max acc (fieldIndex f)) 0 fields in maxIdx + 1

genRecordUpdateFieldCode :: WasmCtx -> Array (Tuple String Expr) -> Int -> String
genRecordUpdateFieldCode ctx fieldUpdates idx = let maybeUpdate = Array.find (\(Tuple n _) -> fieldIndex n == idx) fieldUpdates in genRecordUpdateFieldWithMatch ctx idx maybeUpdate

genRecordUpdateFieldWithMatch :: WasmCtx -> Int -> Maybe (Tuple String Expr) -> String
genRecordUpdateFieldWithMatch ctx idx (Just (Tuple _ updateExpr)) = "      (call $tuple_set (local.get $__tmp) (i32.const " <> show idx <> ") " <> genExpr ctx updateExpr <> ")"
genRecordUpdateFieldWithMatch _ idx Nothing = "      (call $tuple_set (local.get $__tmp) (i32.const " <> show idx <> ") (call $tuple_get (local.get $__rec_base) (i32.const " <> show idx <> ")))"

-- | Generate expression
genExpr :: WasmCtx -> Expr -> String
genExpr ctx (ExprVar name) = genExprVar ctx name

genExpr _ (ExprQualified modName name) = "(call " <> mangleName (modName <> "_" <> name) <> ")"

genExpr ctx (ExprLit (LitString s)) = genExprLitString (Map.lookup s ctx.stringTable)

genExpr _ (ExprLit lit) = genLiteral lit

genExpr ctx (ExprApp func arg) = let Tuple baseFunc allArgs = flattenApp (ExprApp func arg) in genExprAppBaseFunc ctx baseFunc allArgs

genExpr ctx (ExprLambda pats body) = genLambdaMatch ctx pats body (Array.find (\l -> refEqExpr l.expr (ExprLambda pats body)) ctx.lambdas)

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

genExpr ctx (ExprList elems) = genExprListCode ctx elems

genExpr ctx (ExprTuple elems) = genExprTupleCode ctx elems

genExpr ctx (ExprRecord fields) = genExprRecordCode ctx fields

genExpr ctx (ExprRecordAccess expr field) = let recCode = genExpr ctx expr in let idx = fieldIndex field in "(call $tuple_get " <> recCode <> " (i32.const " <> show idx <> "))"

genExpr ctx (ExprRecordUpdate baseExpr fieldUpdates) = let updateFieldNames = map (\(Tuple name _) -> name) fieldUpdates in let recordSize = inferRecordSize updateFieldNames in let baseCode = genExpr ctx baseExpr in let fieldCodes = intercalate "\n" (map (genRecordUpdateFieldCode ctx fieldUpdates) (Array.range 0 (recordSize - 1))) in "(block (result i32)\n      (local.set $__rec_base " <> baseCode <> ")\n      (local.set $__tmp (call $alloc_tuple (i32.const " <> show recordSize <> ")))\n" <> fieldCodes <> "\n      (local.get $__tmp))"

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

-- | Helper for && operator
genBinOpAnd :: String -> String -> String
genBinOpAnd lCode rCode = "(if (result i32) (call $unbox_bool " <> lCode <> ")\n      (then " <> rCode <> ")\n      (else (call $make_bool (i32.const 0))))"

-- | Helper for || operator
genBinOpOr :: String -> String -> String
genBinOpOr lCode rCode = "(if (result i32) (call $unbox_bool " <> lCode <> ")\n      (then (call $make_bool (i32.const 1)))\n      (else " <> rCode <> "))"

-- | Helper for : (Cons) operator
genBinOpCons :: String -> String -> String
genBinOpCons lCode rCode = "(block (result i32)\n      (local.set $__tmp (call $alloc_tuple (i32.const 3)))\n      (call $tuple_set (local.get $__tmp) (i32.const 0) (call $make_ctor (i32.const 1) (i32.const 2)))\n      (call $tuple_set (local.get $__tmp) (i32.const 1) " <> lCode <> ")\n      (call $tuple_set (local.get $__tmp) (i32.const 2) " <> rCode <> ")\n      (local.get $__tmp))"

-- | Helper for qualified operators
genBinOpQualified :: String -> String -> String -> String -> String
genBinOpQualified modName funcName lCode rCode = let callName = "$" <> modName <> "_" <> funcName in "(call $rt_apply_closure (call $rt_apply_closure (call " <> callName <> ") " <> lCode <> ") " <> rCode <> ")"

-- | Helper for unknown operators
genBinOpUnknown :: String -> String -> String -> String
genBinOpUnknown op lCode rCode = "(call $rt_apply_closure (call $rt_apply_closure (call $" <> op <> ") " <> lCode <> ") " <> rCode <> ")"

-- | Handle fallback case for unknown operators
genBinOpFallback :: String -> String -> String -> String
genBinOpFallback op lCode rCode = genBinOpFallbackWithIndex op lCode rCode (String.indexOf (StringPattern.Pattern ".") op)

genBinOpFallbackWithIndex :: String -> String -> String -> Maybe Int -> String
genBinOpFallbackWithIndex op lCode rCode (Just idx) = let modName = String.take idx op in let funcName = String.drop (idx + 1) op in genBinOpQualified modName funcName lCode rCode
genBinOpFallbackWithIndex op lCode rCode Nothing = genBinOpUnknown op lCode rCode

-- | Generate binary operator
genBinOp :: WasmCtx -> String -> Expr -> Expr -> String
genBinOp ctx op l r = let lCode = genExpr ctx l in let rCode = genExpr ctx r in genBinOpWithCodes op lCode rCode

genBinOpWithCodes :: String -> String -> String -> String
genBinOpWithCodes "+" lCode rCode = "(call $make_int (i32.add (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
genBinOpWithCodes "-" lCode rCode = "(call $make_int (i32.sub (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
genBinOpWithCodes "*" lCode rCode = "(call $make_int (i32.mul (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
genBinOpWithCodes "/" lCode rCode = "(call $make_int (i32.div_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
genBinOpWithCodes "mod" lCode rCode = "(call $make_int (i32.rem_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
genBinOpWithCodes "==" lCode rCode = "(call $rt_generic_eq " <> lCode <> " " <> rCode <> ")"
genBinOpWithCodes "/=" lCode rCode = "(call $rt_generic_ne " <> lCode <> " " <> rCode <> ")"
genBinOpWithCodes "<" lCode rCode = "(call $make_bool (i32.lt_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
genBinOpWithCodes ">" lCode rCode = "(call $make_bool (i32.gt_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
genBinOpWithCodes "<=" lCode rCode = "(call $make_bool (i32.le_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
genBinOpWithCodes ">=" lCode rCode = "(call $make_bool (i32.ge_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
genBinOpWithCodes "&&" lCode rCode = genBinOpAnd lCode rCode
genBinOpWithCodes "||" lCode rCode = genBinOpOr lCode rCode
genBinOpWithCodes "<>" lCode rCode = "(call $rt_string_append " <> lCode <> " " <> rCode <> ")"
genBinOpWithCodes ":" lCode rCode = genBinOpCons lCode rCode
genBinOpWithCodes "$" lCode rCode = "(call $rt_apply_closure " <> lCode <> " " <> rCode <> ")"
genBinOpWithCodes "<<<" lCode _ = lCode
genBinOpWithCodes ">>>" _ rCode = rCode
genBinOpWithCodes op lCode rCode = genBinOpFallback op lCode rCode

-- | Common monad check code - checks for Nothing or Left
monadCheckCode :: String
monadCheckCode = "(i32.or\n              (i32.and (i32.eq (i32.and (local.get $__tmp) (i32.const 3)) (i32.const 3))\n                       (i32.eq (call $get_ctor_tag (local.get $__tmp)) (i32.const 0)))\n              (i32.and (i32.eq (i32.and (local.get $__tmp) (i32.const 3)) (i32.const 2))\n                       (i32.and (call $is_ctor (call $tuple_get (local.get $__tmp) (i32.const 0)))\n                                (i32.eq (call $get_ctor_tag (call $tuple_get (local.get $__tmp) (i32.const 0))) (i32.const 0)))))"

-- | Generate binding for PatVar with Just lookup result
genDoBindPatVarJust :: String -> String -> String -> String
genDoBindPatVarJust name exprCode restCode = "(block (result i32)\n        (local.set $__tmp " <> exprCode <> ")\n        ;; Check if Nothing (tag 3, ctor 0) OR Left (tag 2, tuple[0] ctor 0)\n        (if (result i32) " <> monadCheckCode <> "\n          (then (local.get $__tmp)) ;; return Nothing/Left unchanged\n          (else\n            ;; Extract value: from Just (direct) or Right (tuple field 1)\n            (local.set " <> mangleName name <> " (if (result i32) (i32.eq (i32.and (local.get $__tmp) (i32.const 3)) (i32.const 3))\n              (then (call $tuple_get (local.get $__tmp) (i32.const 1))) ;; This shouldn't happen - Nothing has no value\n              (else (call $tuple_get (local.get $__tmp) (i32.const 1)))))\n            " <> restCode <> ")))"

-- | Generate binding for PatVar with Nothing lookup result
genDoBindPatVarNothing :: String -> String -> String
genDoBindPatVarNothing exprCode restCode = "(block (result i32)\n        (local.set $__tmp " <> exprCode <> ")\n        ;; Check if Nothing (tag 3, ctor 0) OR Left (tag 2, tuple[0] ctor 0)\n        (if (result i32) " <> monadCheckCode <> "\n          (then (local.get $__tmp))\n          (else " <> restCode <> ")))"

-- | Generate extract binding for tuple field
genTupleExtractBinding :: Int -> Pattern -> String
genTupleExtractBinding idx (PatVar pname) = "            (local.set " <> mangleName pname <> " (call $tuple_get (local.get $__rec_base) (i32.const " <> show (idx + 1) <> ")))"
genTupleExtractBinding _ _ = ""

-- | Generate binding for Tuple pattern
genDoBindTuple :: Array Pattern -> String -> String -> String
genDoBindTuple pats exprCode restCode = let extractBindings = intercalate "\n" (Array.mapWithIndex genTupleExtractBinding pats) in "(block (result i32)\n        (local.set $__tmp " <> exprCode <> ")\n        ;; Check if Nothing (tag 3, ctor 0) OR Left (tag 2, tuple[0] ctor 0)\n        (if (result i32) " <> monadCheckCode <> "\n          (then (local.get $__tmp)) ;; return Nothing/Left unchanged\n          (else\n            ;; Extract from Right (tuple field 1), then extract tuple fields\n            (local.set $__rec_base (call $tuple_get (local.get $__tmp) (i32.const 1)))\n" <> extractBindings <> "\n            " <> restCode <> ")))"

-- | Generate binding for fallback pattern
genDoBindFallback :: String -> String -> String
genDoBindFallback exprCode restCode = "(block (result i32)\n        (local.set $__tmp " <> exprCode <> ")\n        ;; Check if Nothing (tag 3, ctor 0) OR Left (tag 2, tuple[0] ctor 0)\n        (if (result i32) " <> monadCheckCode <> "\n          (then (local.get $__tmp))\n          (else " <> restCode <> ")))"

-- | Add local binding to context
addLocalToCtx :: String -> WasmCtx -> WasmCtx
addLocalToCtx n c = if Map.member n c.locals then c else c { locals = Map.insert n c.localCount c.locals, localCount = c.localCount + 1 }

-- | Handle PatVar bind with context
genDoBindPatVar :: WasmCtx -> String -> String -> String -> String
genDoBindPatVar ctx' name exprCode restCode = genDoBindPatVarWithLookup (Map.lookup name ctx'.locals) name exprCode restCode

genDoBindPatVarWithLookup :: Maybe Int -> String -> String -> String -> String
genDoBindPatVarWithLookup (Just _) name exprCode restCode = genDoBindPatVarJust name exprCode restCode
genDoBindPatVarWithLookup Nothing _ exprCode restCode = genDoBindPatVarNothing exprCode restCode

-- | Handle DoBind with pattern
genDoBindPat :: WasmCtx -> Pattern -> String -> String -> String
genDoBindPat ctx' (PatVar name) exprCode restCode = genDoBindPatVar ctx' name exprCode restCode
genDoBindPat _ (PatCon "Tuple" pats) exprCode restCode = genDoBindTuple pats exprCode restCode
genDoBindPat _ _ exprCode restCode = genDoBindFallback exprCode restCode

-- | Handle DoBind statement
genDoBind :: WasmCtx -> Pattern -> Expr -> Array DoStatement -> String
genDoBind ctx pat e rest = let vars = patternVars pat in let ctx' = foldr addLocalToCtx ctx vars in let exprCode = genExpr ctx e in let restCode = genDoStmts ctx' rest in genDoBindPat ctx' pat exprCode restCode

-- | Handle single DoStatement (last in list)
genDoStmtLast :: WasmCtx -> DoStatement -> String
genDoStmtLast ctx (DoExpr e) = genExpr ctx e
genDoStmtLast ctx (DoLet binds) = genExpr ctx (ExprLet binds (ExprLit (LitInt 0)))
genDoStmtLast ctx (DoBind _ e) = genExpr ctx e

-- | Handle DoStatement with rest
genDoStmtWithRest :: WasmCtx -> DoStatement -> Array DoStatement -> String
genDoStmtWithRest ctx (DoExpr e) rest = "(block (result i32)\n        (drop " <> genExpr ctx e <> ")\n        " <> genDoStmts ctx rest <> ")"
genDoStmtWithRest ctx (DoLet binds) rest = genLetBinds ctx binds (ExprDo rest)
genDoStmtWithRest ctx (DoBind pat e) rest = genDoBind ctx pat e rest

-- | Handle uncons result
genDoStmtsUncons :: WasmCtx -> Maybe { head :: DoStatement, tail :: Array DoStatement } -> String
genDoStmtsUncons _ Nothing = "(call $make_unit)"
genDoStmtsUncons ctx (Just unconsed) = genDoStmtsUnconsed ctx unconsed.head unconsed.tail

genDoStmtsUnconsed :: WasmCtx -> DoStatement -> Array DoStatement -> String
genDoStmtsUnconsed ctx stmt rest = if Array.null rest then genDoStmtLast ctx stmt else genDoStmtWithRest ctx stmt rest

-- | Generate do statements
genDoStmts :: WasmCtx -> Array DoStatement -> String
genDoStmts ctx stmts = genDoStmtsUncons ctx (Array.uncons stmts)
