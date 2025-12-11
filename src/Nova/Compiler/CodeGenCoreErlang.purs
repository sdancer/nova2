module Nova.Compiler.CodeGenCoreErlang where

import Prelude
import Data.Array (length, (:))
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Tuple (Tuple(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (foldr, foldl)
import Nova.Compiler.Ast (Module, Declaration(..), FunctionDeclaration, DataType, DataConstructor, ForeignImport, TypeExpr(..), Expr(..), Pattern(..), Literal(..), LetBind, CaseClause, DoStatement(..), ImportItem(..), GuardedExpr, GuardClause(..), NewtypeDecl)
-- getUsedVars is defined locally below to avoid cross-module import issues

-- Core Erlang Code Generation
-- Core Erlang is a simpler intermediate language used by the BEAM compiler.
-- It has explicit let bindings, case expressions, and function applications.

-- Code generation context
type CoreCtx =
  { moduleName :: String
  , moduleFuncs :: Set String
  , locals :: Set String
  , funcArities :: Array { name :: String, arity :: Int }
  , varCounter :: Int  -- For generating unique variable names
  , imports :: Map String String  -- Imported name -> source module
  , aliasMap :: Map String String  -- Module alias -> full module name (e.g., "Array" -> "Data.Array")
  }

emptyCtx :: String -> CoreCtx
emptyCtx modName =
  { moduleName: modName
  , moduleFuncs: Set.empty
  , locals: Set.empty
  , funcArities: []
  , varCounter: 0
  , imports: Map.empty
  , aliasMap: Map.empty
  }

-- | Prelude functions that map to Erlang BIFs or stdlib
-- Returns Just (module, func, needsPartialApp) where needsPartialApp indicates
-- if we need to wrap it in a lambda for partial application
type PreludeFuncInfo = { mod :: String, func :: String, arity :: Int }

getPreludeFunc :: String -> Maybe PreludeFuncInfo
getPreludeFunc "show" = Just { mod: "erlang", func: "integer_to_list", arity: 1 }  -- Simple case: assume Int
getPreludeFunc "foldl" = Nothing  -- Don't use lists:foldl - argument order is different
getPreludeFunc "foldr" = Just { mod: "lists", func: "foldr", arity: 3 }  -- Use lists implementation
getPreludeFunc "map" = Just { mod: "lists", func: "map", arity: 2 }
getPreludeFunc "filter" = Just { mod: "lists", func: "filter", arity: 2 }
getPreludeFunc "length" = Just { mod: "erlang", func: "length", arity: 1 }
getPreludeFunc "reverse" = Just { mod: "lists", func: "reverse", arity: 1 }
getPreludeFunc "concat" = Just { mod: "lists", func: "concat", arity: 1 }
getPreludeFunc "head" = Just { mod: "erlang", func: "hd", arity: 1 }
getPreludeFunc "tail" = Just { mod: "erlang", func: "tl", arity: 1 }
getPreludeFunc "take" = Just { mod: "lists", func: "sublist", arity: 2 }
getPreludeFunc "drop" = Just { mod: "lists", func: "nthtail", arity: 2 }
getPreludeFunc "not" = Just { mod: "erlang", func: "not", arity: 1 }
getPreludeFunc "negate" = Just { mod: "erlang", func: "-", arity: 1 }
getPreludeFunc "mod" = Just { mod: "erlang", func: "rem", arity: 2 }
-- Monad operations - for Either we use Right as pure
getPreludeFunc "pure" = Nothing  -- handled specially as Right constructor
getPreludeFunc _ = Nothing

-- | Check if a name is a prelude function
isPreludeFunc :: String -> Boolean
isPreludeFunc name = case getPreludeFunc name of
  Just _ -> true
  Nothing -> false

-- | Generate a fresh variable name
freshVar :: CoreCtx -> { var :: String, ctx :: CoreCtx }
freshVar ctx =
  { var: "_cor" <> show ctx.varCounter
  , ctx: ctx { varCounter = ctx.varCounter + 1 }
  }

-- | Add locals from pattern
addLocalsFromPattern :: Pattern -> CoreCtx -> CoreCtx
addLocalsFromPattern (PatVar name) ctx = ctx { locals = Set.insert name ctx.locals }
addLocalsFromPattern PatWildcard ctx = ctx
addLocalsFromPattern (PatLit _) ctx = ctx
addLocalsFromPattern (PatCon _ pats) ctx = foldr addLocalsFromPattern ctx pats
addLocalsFromPattern (PatRecord fields) ctx = foldr (\(Tuple _ p) c -> addLocalsFromPattern p c) ctx fields
addLocalsFromPattern (PatList pats) ctx = foldr addLocalsFromPattern ctx pats
addLocalsFromPattern (PatCons hd tl) ctx = addLocalsFromPattern tl (addLocalsFromPattern hd ctx)
addLocalsFromPattern (PatAs name pat) ctx = addLocalsFromPattern pat (ctx { locals = Set.insert name ctx.locals })
addLocalsFromPattern (PatParens p) ctx = addLocalsFromPattern p ctx

-- | Collect free variable names from an expression (for dependency analysis)
-- Only collects names that are in the provided 'candidates' set
freeVarsInExprFor :: Set String -> Set String -> Expr -> Set String
freeVarsInExprFor candidates bound expr = case expr of
  ExprVar name ->
    if Set.member name candidates && not (Set.member name bound)
    then Set.singleton name
    else Set.empty
  ExprLit _ -> Set.empty
  ExprApp f arg -> Set.union (freeVarsInExprFor candidates bound f) (freeVarsInExprFor candidates bound arg)
  ExprLambda pats body ->
    let patBound = foldr addPatternVars bound pats
    in freeVarsInExprFor candidates patBound body
  ExprLet binds body ->
    let bindNames = Set.fromFoldable (List.mapMaybe (\b -> getPatternVarName b.pattern) binds)
        newBound = Set.union bound bindNames
        bindVars = foldr (\b s -> Set.union s (freeVarsInExprFor candidates newBound b.value)) Set.empty binds
    in Set.union bindVars (freeVarsInExprFor candidates newBound body)
  ExprCase scrut clauses ->
    let scrutVars = freeVarsInExprFor candidates bound scrut
        clauseVars = foldr (\c s ->
          let clauseBound = addPatternVars c.pattern bound
              guardVars = case c.guard of
                Nothing -> Set.empty
                Just g -> freeVarsInExprFor candidates clauseBound g
              bodyVars = freeVarsInExprFor candidates clauseBound c.body
          in Set.union s (Set.union guardVars bodyVars)) Set.empty clauses
    in Set.union scrutVars clauseVars
  ExprIf cond thn els ->
    Set.union (freeVarsInExprFor candidates bound cond) (Set.union (freeVarsInExprFor candidates bound thn) (freeVarsInExprFor candidates bound els))
  ExprDo _ -> Set.empty  -- Do notation is complex, skip for now
  ExprList exprs -> foldr (\e s -> Set.union s (freeVarsInExprFor candidates bound e)) Set.empty exprs
  ExprTuple exprs -> foldr (\e s -> Set.union s (freeVarsInExprFor candidates bound e)) Set.empty exprs
  ExprRecord fields -> foldr (\(Tuple _ e) s -> Set.union s (freeVarsInExprFor candidates bound e)) Set.empty fields
  ExprRecordAccess e _ -> freeVarsInExprFor candidates bound e
  ExprRecordUpdate e updates -> Set.union (freeVarsInExprFor candidates bound e) (foldr (\(Tuple _ v) s -> Set.union s (freeVarsInExprFor candidates bound v)) Set.empty updates)
  ExprTyped e _ -> freeVarsInExprFor candidates bound e
  ExprBinOp _ l r -> Set.union (freeVarsInExprFor candidates bound l) (freeVarsInExprFor candidates bound r)
  ExprUnaryOp _ e -> freeVarsInExprFor candidates bound e
  ExprParens e -> freeVarsInExprFor candidates bound e
  ExprSection _ -> Set.empty
  ExprSectionLeft e _ -> freeVarsInExprFor candidates bound e
  ExprSectionRight _ e -> freeVarsInExprFor candidates bound e
  ExprQualified _ _ -> Set.empty
  where
    addPatternVars :: Pattern -> Set String -> Set String
    addPatternVars (PatVar n) s = Set.insert n s
    addPatternVars PatWildcard s = s
    addPatternVars (PatLit _) s = s
    addPatternVars (PatCon _ pats) s = foldr addPatternVars s pats
    addPatternVars (PatRecord fields) s = foldr (\(Tuple _ p) acc -> addPatternVars p acc) s fields
    addPatternVars (PatList pats) s = foldr addPatternVars s pats
    addPatternVars (PatCons hd tl) s = addPatternVars hd (addPatternVars tl s)
    addPatternVars (PatAs n p) s = addPatternVars p (Set.insert n s)
    addPatternVars (PatParens p) s = addPatternVars p s

-- | Topologically sort value bindings based on dependencies
-- Returns bindings in order such that each binding comes after all bindings it depends on
topoSortBinds :: Array LetBind -> Array LetBind
topoSortBinds binds =
  let -- Build map of name -> binding
      bindMap = Map.fromFoldable (Array.mapMaybe (\b ->
        case getPatternVarName b.pattern of
          Nothing -> Nothing
          Just n -> Just (Tuple n b)) binds)
      -- Get all binding names
      bindNames = Set.fromFoldable (Map.keys bindMap)
      -- For each binding, find its dependencies (other bindings it references)
      deps = map (\b ->
        let name = fromMaybe "" (getPatternVarName b.pattern)
            -- Pass bindNames as candidates - only look for references to other bindings
            freeVars = freeVarsInExprFor bindNames Set.empty b.value
            -- freeVars already only contains bindings (by construction)
            localDeps = freeVars
        in { name, bind: b, deps: localDeps }) binds
      -- Kahn's algorithm for topological sort
      sorted = kahnSort deps
  in sorted
  where
    kahnSort :: Array { name :: String, bind :: LetBind, deps :: Set String } -> Array LetBind
    kahnSort items =
      let go :: Array LetBind -> Array { name :: String, bind :: LetBind, deps :: Set String } -> Array LetBind
          go acc [] = acc
          go acc remaining =
            -- Find items with no unresolved dependencies
            let processed = Set.fromFoldable (map (\b -> fromMaybe "" (getPatternVarName b.pattern)) acc)
                ready = Array.filter (\item ->
                  let unresolvedDeps = Set.difference item.deps processed
                  in Set.isEmpty unresolvedDeps) remaining
                notReady = Array.filter (\item ->
                  let unresolvedDeps = Set.difference item.deps processed
                  in not (Set.isEmpty unresolvedDeps)) remaining
            in if Array.null ready
               then -- Cycle detected or no progress - return remaining in original order
                    acc <> map _.bind remaining
               else go (acc <> map _.bind ready) notReady
      in go [] items

-- | Convert name to Core Erlang atom (quoted)
-- Escapes single quotes and backslashes inside the atom
atom :: String -> String
atom s = "'" <> escapeAtom s <> "'"
  where
  escapeAtom str =
    let s1 = String.replaceAll (String.Pattern "\\") (String.Replacement "\\\\") str
        s2 = String.replaceAll (String.Pattern "'") (String.Replacement "\\'") s1
    in s2

-- | Convert name to Core Erlang variable (uppercase first letter)
coreVar :: String -> String
coreVar name =
  let first = String.take 1 name
      rest = String.drop 1 name
  in String.toUpper first <> toSnake rest
  where
    toSnake s = String.replaceAll (String.Pattern "'") (String.Replacement "_") s

-- | Generate module
genModule :: Module -> String
genModule m =
  let modName = translateModuleName m.name
      -- Collect all function declarations
      allFuncs = Array.mapMaybe getFunc (Array.fromFoldable m.declarations)
      -- Collect all foreign imports
      allForeignImports = Array.mapMaybe getForeignFunc (Array.fromFoldable m.declarations)
      -- Collect all data types
      allDataTypes = Array.mapMaybe getDataType (Array.fromFoldable m.declarations)
      -- Collect all newtypes
      allNewtypes = Array.mapMaybe getNewtype (Array.fromFoldable m.declarations)
      -- Collect all imports
      allImports = Array.concatMap getImports (Array.fromFoldable m.declarations)
      importMap = Map.fromFoldable allImports
      -- Build alias map from import declarations
      aliasMapArr = Array.mapMaybe getAliasMapping (Array.fromFoldable m.declarations)
      aliasMp = Map.fromFoldable aliasMapArr
      -- Group functions by name/arity
      grouped = groupFunctions allFuncs
      -- Extract unique name/arity pairs for exports (including foreign imports)
      uniqueFuncs = Array.nubByEq (\a b -> a.name == b.name && a.arity == b.arity)
                      (map (\g -> { name: g.name, arity: g.arity }) grouped)
      foreignExports = map (\fi -> { name: fi.functionName, arity: countTypeArity fi.typeSignature }) allForeignImports
      -- Collect constructor exports from data types and newtypes
      constructorExports = Array.concatMap getConstructorExports allDataTypes <>
                           Array.concatMap getNewtypeExports allNewtypes
      allExports = uniqueFuncs <> foreignExports <> constructorExports
      -- Generate curried wrapper exports for all arities 0 to N-1
      curriedExports = Array.concatMap genCurriedExports allExports
      allExportsWithCurried = allExports <> curriedExports
      exports = String.joinWith ", " (map (\f -> atom f.name <> "/" <> show f.arity) allExportsWithCurried)
      ctx = (emptyCtx modName) { moduleFuncs = Set.fromFoldable (map _.name allExports)
                               , funcArities = allExports
                               , imports = importMap
                               , aliasMap = aliasMp }
      -- Generate function definitions (merging multiple clauses)
      funcDefs = String.joinWith "\n\n" (map (genFunctionGroup ctx) grouped)
      -- Generate foreign import function definitions
      foreignDefs = String.joinWith "\n\n" (map (genForeignImport m.name) allForeignImports)
      -- Generate data type comments and constructor functions
      dtDefs = String.joinWith "\n\n" (Array.mapMaybe (genDeclNonFunc ctx) (Array.fromFoldable m.declarations))
      -- Generate curried wrapper functions for all lower arities
      curriedWrappers = String.joinWith "\n\n" (Array.concatMap genCurriedWrappers allExports)
  in "module " <> atom modName <> " [" <> exports <> "]\n" <>
     "  attributes []\n" <>
     dtDefs <> (if dtDefs == "" then "" else "\n\n") <>
     funcDefs <> (if funcDefs == "" || foreignDefs == "" then "" else "\n\n") <>
     foreignDefs <> (if curriedWrappers == "" then "" else "\n\n") <>
     curriedWrappers <> "\nend\n"
  where
    getFunc (DeclFunction f) = Just f
    getFunc _ = Nothing
    getForeignFunc (DeclForeignImport fi) = Just fi
    getForeignFunc _ = Nothing
    getDataType (DeclDataType dt) = Just dt
    getDataType _ = Nothing
    getNewtype (DeclNewtype nt) = Just nt
    getNewtype _ = Nothing
    getImports (DeclImport imp) = importItemsToTuples imp.moduleName (Array.fromFoldable imp.items)
    getImports _ = []
    -- Get constructor exports for a data type
    getConstructorExports dt =
      map (\c -> { name: c.name, arity: List.length c.fields }) (Array.fromFoldable dt.constructors)
    -- Get constructor export for a newtype (always arity 1)
    getNewtypeExports nt = [{ name: nt.constructor, arity: 1 }]
    -- Extract alias -> full module name mapping from import declarations
    getAliasMapping (DeclImport imp) =
      let fullName = imp.moduleName
          -- If alias is specified, use it; otherwise derive from last component of module name
          aliasName = case imp.alias of
            Just a -> a
            Nothing -> getLastComponent fullName
      in Just (Tuple aliasName fullName)
    getAliasMapping _ = Nothing
    -- Get the last component of a dotted module name (e.g., "Data.Array" -> "Array")
    getLastComponent name = case String.lastIndexOf (String.Pattern ".") name of
      Just idx -> String.drop (idx + 1) name
      Nothing -> name
    genDeclNonFunc ctx (DeclDataType dt) = Just (genDataType ctx dt)
    genDeclNonFunc _ (DeclNewtype nt) = Just (genNewtype nt)
    genDeclNonFunc _ _ = Nothing

-- | Generate curried export entries for all arities from 0 to N-1
genCurriedExports :: { name :: String, arity :: Int } -> Array { name :: String, arity :: Int }
genCurriedExports { name, arity } =
  if arity < 1 then []
  else map (\a -> { name, arity: a }) (safeRange 0 (arity - 1))

-- | Generate curried wrapper functions for all arities from 0 to N-1
-- For a function foo/3, generates:
--   foo/2 = fun(A0, A1) -> fun(A2) -> apply 'foo'/3(A0, A1, A2) end end
--   foo/1 = fun(A0) -> fun(A1) -> fun(A2) -> apply 'foo'/3(A0, A1, A2) end end end
--   foo/0 = fun() -> fun(A0) -> fun(A1) -> fun(A2) -> apply 'foo'/3(A0, A1, A2) end end end end
genCurriedWrappers :: { name :: String, arity :: Int } -> Array String
genCurriedWrappers { name, arity } =
  if arity < 1 then []
  else map (genCurriedWrapper name arity) (safeRange 0 (arity - 1))

-- | Safe range that returns empty array if end < start
safeRange :: Int -> Int -> Array Int
safeRange start end = if end < start then [] else Array.range start end

-- | Generate a single curried wrapper at a specific lower arity
-- Core Erlang requires `let <Var> = fun ... in Var` for returned functions
genCurriedWrapper :: String -> Int -> Int -> String
genCurriedWrapper name fullArity wrapperArity =
  let -- Parameters for the wrapper function
      wrapperParams = map (\i -> "_A" <> show i) (safeRange 0 (wrapperArity - 1))
      wrapperParamsStr = if wrapperArity == 0 then "" else String.joinWith ", " wrapperParams
      -- Remaining parameters that become nested lambdas
      remainingCount = fullArity - wrapperArity
      remainingParams = map (\i -> "_A" <> show (wrapperArity + i)) (safeRange 0 (remainingCount - 1))
      -- All parameters for the inner call
      allParams = wrapperParams <> remainingParams
      allParamsStr = String.joinWith ", " allParams
      -- Build nested let/fun structure from inside out
      innerCall = "apply " <> atom name <> "/" <> show fullArity <> "(" <> allParamsStr <> ")"
      -- Build nested let <_Fi> = fun (param) -> body in _Fi
      buildNested :: Int -> String -> String -> String
      buildNested idx param body =
        let varName = "_F" <> show idx
        in "let <" <> varName <> "> = fun (" <> param <> ") ->\n        " <> body <> "\n      in " <> varName
      -- Fold from last remaining param to first, building nested structure
      nestedLets = Array.foldl (\{body, idx} param ->
        {body: buildNested idx param body, idx: idx - 1})
        {body: innerCall, idx: remainingCount - 1}
        (Array.reverse remainingParams)
  in atom name <> "/" <> show wrapperArity <> " =\n  fun (" <> wrapperParamsStr <> ") ->\n    " <> nestedLets.body

-- | Convert import items to (name, module) tuples
importItemsToTuples :: String -> Array ImportItem -> Array (Tuple String String)
importItemsToTuples modName items = Array.concatMap toTuples items
  where
    toTuples (ImportValue name) = [Tuple name modName]
    toTuples (ImportType name _) = [Tuple name modName]  -- Types are also tracked

-- | A group of function clauses with same name/arity
type FunctionGroup =
  { name :: String
  , arity :: Int
  , clauses :: Array FunctionDeclaration
  }

-- | Group function declarations by name and arity
groupFunctions :: Array FunctionDeclaration -> Array FunctionGroup
groupFunctions funcs =
  let -- Get all unique name/arity pairs
      keys = Array.nubByEq (\a b -> a.name == b.name && a.arity == b.arity)
               (map (\f -> { name: f.name, arity: List.length f.parameters }) funcs)
      -- Group clauses for each key
      mkGroup k = { name: k.name, arity: k.arity, clauses: Array.filter (\f -> f.name == k.name && List.length f.parameters == k.arity) funcs }
  in map mkGroup keys

-- | Generate a function group (may have multiple clauses)
genFunctionGroup :: CoreCtx -> FunctionGroup -> String
genFunctionGroup ctx group =
  case Array.uncons group.clauses of
    Nothing -> ""
    Just { head: first, tail: [] } ->
      -- Single clause - generate directly (may have patterns or may be simple)
      if hasComplexPattern first.parameters
      then genMergedFunction ctx group
      else genFunction ctx first
    Just _ ->
      -- Multiple clauses - merge into single function with case
      genMergedFunction ctx group

-- | Check if parameters contain complex patterns (not just variables)
hasComplexPattern :: List Pattern -> Boolean
hasComplexPattern pats = List.any isComplex pats
  where
    isComplex (PatVar _) = false
    isComplex PatWildcard = false
    isComplex _ = true

-- | Check if a single pattern is complex (not just a variable)
hasComplexPatternSingle :: Pattern -> Boolean
hasComplexPatternSingle (PatVar _) = false
hasComplexPatternSingle PatWildcard = false
hasComplexPatternSingle _ = true

-- | Generate merged function with case expression
genMergedFunction :: CoreCtx -> FunctionGroup -> String
genMergedFunction ctx group =
  let arity = group.arity
      -- Generate fresh parameter names
      paramNames = map (\i -> "_P" <> show i) (Array.range 0 (arity - 1))
      paramsStr = String.joinWith ", " paramNames
      -- Generate case clauses from each function clause
      caseClauses = map (genFunctionClauseAsCase ctx paramNames) group.clauses
  in atom group.name <> "/" <> show arity <> " =\n" <>
     "  fun (" <> paramsStr <> ") ->\n" <>
     "    case {" <> paramsStr <> "} of\n" <>
     String.joinWith "\n" caseClauses <> "\n" <>
     "    end"

-- | Generate a function clause as a case clause
genFunctionClauseAsCase :: CoreCtx -> Array String -> FunctionDeclaration -> String
genFunctionClauseAsCase ctx _paramNames func =
  let patsResult = genPatsWithCounter (Array.fromFoldable func.parameters) 0
      patTuple = "{" <> String.joinWith ", " patsResult.strs <> "}"
      ctxWithParams = foldr addLocalsFromPattern ctx func.parameters
      -- Check if function has guards
      body = if List.null func.guards
             then genExpr ctxWithParams func.body
             else genGuardedExprs ctxWithParams (Array.fromFoldable func.guards)
  in "      <" <> patTuple <> "> when 'true' ->\n        " <> body

-- | Translate module name - preserve exact PureScript name (e.g., "Nova.Compiler.Ast")
-- The name is quoted as an atom in Core Erlang, so dots are preserved
translateModuleName :: String -> String
translateModuleName name = name

-- | Resolve a module alias to its full name using the context's aliasMap
-- If the alias is not found, return the name as-is (it may already be a full name)
resolveModuleName :: CoreCtx -> String -> String
resolveModuleName ctx alias = case Map.lookup alias ctx.aliasMap of
  Just fullName -> fullName
  Nothing -> alias  -- May already be a full name or external module

-- NOTE: genDecl replaced by genFunctionGroup and genDeclNonFunc in genModule

-- | Generate function definition
genFunction :: CoreCtx -> FunctionDeclaration -> String
genFunction ctx func =
  let params = Array.fromFoldable (map genPattern func.parameters)
      paramsStr = String.joinWith ", " params
      arity = List.length func.parameters
      ctxWithParams = foldr addLocalsFromPattern ctx func.parameters
      -- Check if function has guards (body is __guarded__ placeholder)
      body = if List.null func.guards
             then genExpr ctxWithParams func.body
             else genGuardedExprs ctxWithParams (Array.fromFoldable func.guards)
  in atom func.name <> "/" <> show arity <> " =\n" <>
     "  fun (" <> paramsStr <> ") ->\n" <>
     "    " <> body

-- | Generate guarded expressions as nested case expressions
genGuardedExprs :: CoreCtx -> Array GuardedExpr -> String
genGuardedExprs ctx guards =
  case Array.uncons guards of
    Nothing -> atom "error_no_matching_guard"
    Just { head: g, tail: rest } ->
      genOneGuardedExpr ctx g rest

-- | Generate one guarded expression with fallback to rest
genOneGuardedExpr :: CoreCtx -> GuardedExpr -> Array GuardedExpr -> String
genOneGuardedExpr ctx guardedExpr rest =
  case List.uncons guardedExpr.guards of
    Nothing ->
      -- No guard clauses, just use body (shouldn't happen)
      genExpr ctx guardedExpr.body
    Just { head: clause, tail: moreClauses } ->
      case clause of
        GuardExpr expr ->
          -- Boolean guard: case expr of true -> body; _ -> try next
          let fallback = if Array.null rest && List.null moreClauses
                         then atom "error_no_matching_guard"
                         else if List.null moreClauses
                              then genGuardedExprs ctx rest
                              else genOneGuardedExpr ctx { guards: moreClauses, body: guardedExpr.body } rest
          in "case " <> genExpr ctx expr <> " of\n" <>
             "        <'true'> when 'true' -> " <> genExpr ctx guardedExpr.body <> "\n" <>
             "        <_> when 'true' -> " <> fallback <> "\n" <>
             "      end"
        GuardPat pat bindExpr ->
          -- Pattern guard: case bindExpr of pat -> body; _ -> try next
          let ctxWithBind = addLocalsFromPattern pat ctx
              innerBody = if List.null moreClauses
                          then genExpr ctxWithBind guardedExpr.body
                          else genOneGuardedExpr ctxWithBind { guards: moreClauses, body: guardedExpr.body } rest
              fallback = if Array.null rest
                         then atom "error_no_matching_guard"
                         else genGuardedExprs ctx rest
          in "case " <> genExpr ctx bindExpr <> " of\n" <>
             "        <" <> genPattern pat <> "> when 'true' -> " <> innerBody <> "\n" <>
             "        <_> when 'true' -> " <> fallback <> "\n" <>
             "      end"

-- | Generate data type constructors as atoms
genDataType :: CoreCtx -> DataType -> String
genDataType _ dt =
  let comment = "% Data type: " <> dt.name <> "\n" <>
                "% Constructors: " <> String.joinWith ", " (Array.fromFoldable (map _.name dt.constructors))
      constructorDefs = map genConstructor (Array.fromFoldable dt.constructors)
  in comment <> "\n\n" <> String.joinWith "\n\n" constructorDefs
  where
    genConstructor :: DataConstructor -> String
    genConstructor c =
      let arity = List.length c.fields
          params = if arity <= 0 then [] else map (\i -> "V" <> show i) (Array.range 0 (arity - 1))
          paramStr = String.joinWith ", " params
          tag = constructorTag c.name
          body = if arity == 0
                 then "'" <> tag <> "'"
                 else "{'" <> tag <> "', " <> String.joinWith ", " params <> "}"
      in "'" <> c.name <> "'/" <> show arity <> " =\n  fun (" <> paramStr <> ") ->\n    " <> body

-- | Convert constructor name to tag - preserve original name for 1:1 mapping
constructorTag :: String -> String
constructorTag name = name

-- | Generate newtype constructor function
genNewtype :: NewtypeDecl -> String
genNewtype nt =
  let comment = "% Newtype: " <> nt.name <> "\n" <>
                "% Constructor: " <> nt.constructor
      -- Newtype constructor just wraps the value in a tuple
      body = "'" <> nt.constructor <> "'/1 =\n  fun (V0) ->\n    {'" <> nt.constructor <> "', V0}"
  in comment <> "\n\n" <> body

-- | Generate pattern (wrapper that hides counter)
genPattern :: Pattern -> String
genPattern pat = (genPatternWithCounter pat 0).str

-- | Generate pattern with unique wildcard counter
-- Returns { str: pattern string, counter: updated counter }
genPatternWithCounter :: Pattern -> Int -> { str :: String, counter :: Int }
genPatternWithCounter (PatVar name) n =
  -- Treat PatVar "_" and "_x" as wildcard (use if instead of guard for self-hosting)
  if String.take 1 name == "_"
  then { str: "_W" <> show n, counter: n + 1 }
  else { str: coreVar name, counter: n }
genPatternWithCounter PatWildcard n = { str: "_W" <> show n, counter: n + 1 }
genPatternWithCounter (PatLit lit) n = { str: genLiteral lit, counter: n }
genPatternWithCounter (PatCon name pats) n =
  -- Strip module qualifier from constructor name (e.g., "Ast.DeclModule" -> "DeclModule")
  let baseName = case String.lastIndexOf (String.Pattern ".") name of
        Nothing -> name
        Just idx -> String.drop (idx + 1) name
  -- Special handling for List constructors - use native Erlang lists
  in if baseName == "Nil"
     then { str: "[]", counter: n }
     else if baseName == "Cons" && List.length pats == 2
     then case List.uncons pats of
       Just { head: h, tail: rest } ->
         case List.head rest of
           Just t ->
             let r1 = genPatternWithCounter h n
                 r2 = genPatternWithCounter t r1.counter
             in { str: "[" <> r1.str <> "|" <> r2.str <> "]", counter: r2.counter }
           Nothing -> { str: "[]", counter: n }  -- Shouldn't happen
       Nothing -> { str: "[]", counter: n }  -- Shouldn't happen
     else if List.null pats
     then { str: atom (toSnakeCase baseName), counter: n }
     else let result = genPatsWithCounter (Array.fromFoldable pats) n
          in { str: "{" <> atom (toSnakeCase baseName) <> ", " <> String.joinWith ", " result.strs <> "}", counter: result.counter }
genPatternWithCounter (PatRecord fields) n =
  let result = foldl genFieldPat { strs: [], counter: n } fields
  in { str: "~{" <> String.joinWith "," result.strs <> "}~", counter: result.counter }
  where
    genFieldPat acc (Tuple label pat) =
      let r = genPatternWithCounter pat acc.counter
      in { strs: acc.strs <> [atom (toSnakeCase label) <> ":=" <> r.str], counter: r.counter }
genPatternWithCounter (PatList pats) n =
  let result = genPatsWithCounter (Array.fromFoldable pats) n
  in { str: "[" <> String.joinWith ", " result.strs <> "]", counter: result.counter }
genPatternWithCounter (PatCons hd tl) n =
  let r1 = genPatternWithCounter hd n
      r2 = genPatternWithCounter tl r1.counter
  in { str: "[" <> r1.str <> " | " <> r2.str <> "]", counter: r2.counter }
genPatternWithCounter (PatAs name pat) n =
  let r = genPatternWithCounter pat n
  in { str: coreVar name <> " = " <> r.str, counter: r.counter }
genPatternWithCounter (PatParens p) n = genPatternWithCounter p n

-- | Generate multiple patterns with counter threading
genPatsWithCounter :: Array Pattern -> Int -> { strs :: Array String, counter :: Int }
genPatsWithCounter pats n =
  foldl step { strs: [], counter: n } pats
  where
    step acc pat =
      let r = genPatternWithCounter pat acc.counter
      in { strs: acc.strs <> [r.str], counter: r.counter }

-- | Generate expression
genExpr :: CoreCtx -> Expr -> String
genExpr _ (ExprLit lit) = genLiteral lit

genExpr ctx (ExprVar name) =
  -- Handle 'otherwise' as 'true' and 'unit' as atom
  if name == "otherwise"
  then "'true'"
  else if name == "unit"
  then "'unit'"
  else
  -- Check if it's a qualified name (contains a dot like "Array.elem")
  case String.indexOf (String.Pattern ".") name of
    Just idx ->
      -- Qualified name - call with no args to get value or curried wrapper
      let modAlias = String.take idx name
          fullModName = resolveModuleName ctx modAlias
          funcName = String.drop (idx + 1) name
      in "call " <> atom (translateModuleName fullModName) <> ":" <> atom funcName <> "()"
    Nothing ->
      if Set.member name ctx.locals
      then coreVar name
      else case getPreludeFunc name of
        -- Prelude function as first-class value - wrap in lambda
        Just info ->
          let paramNames = map (\i -> "_Pf" <> show i) (Array.range 0 (info.arity - 1))
              paramsStr = String.joinWith ", " paramNames
          in "fun (" <> paramsStr <> ") -> call " <> atom info.mod <> ":" <> atom info.func <>
             "(" <> paramsStr <> ")"
        Nothing ->
          if Set.member name ctx.moduleFuncs
          then -- Module function reference as value - wrap in lambda
               let arity = lookupArity name ctx
               in if arity == 0
                  then "apply " <> atom name <> "/0()"
                  else let paramNames = map (\i -> "_Mf" <> show i) (safeRange 0 (arity - 1))
                           paramsStr = String.joinWith ", " paramNames
                       in "fun (" <> paramsStr <> ") -> apply " <> atom name <> "/" <> show arity <>
                          "(" <> paramsStr <> ")"
          else case Map.lookup name ctx.imports of
               -- Imported value/function as value - call /0 to get value or curried wrapper
               Just srcMod ->
                 let modName = translateModuleName srcMod
                 in "call " <> atom modName <> ":" <> atom name <> "()"
               Nothing ->
                 if isConstructorName name
                 -- Nullary data constructor - use as atom
                 -- Special case: Nil is native empty list
                 then if name == "Nil" then "[]" else atom (toSnakeCase name)
                 else coreVar name
  where
    isConstructorName s = case String.take 1 s of
      c -> c >= "A" && c <= "Z"

genExpr ctx (ExprQualified modAlias funcName) =
  -- When used as a value (not applied)
  let fullModName = resolveModuleName ctx modAlias
  in if isConstructorName funcName
     -- Constructor used as a value: generate a curried function wrapper
     -- For binary constructor like TyExprApp: fun(A) -> fun(B) -> {TyExprApp, A, B} end end
     -- For unary like Just: fun(A) -> {Just, A} end
     -- Since we don't know arity, generate curried wrapper via module call
     then "call " <> atom (translateModuleName fullModName) <> ":" <> atom funcName <> "()"
     -- Regular function: call with no arguments
     else "call " <> atom (translateModuleName fullModName) <> ":" <> atom funcName <> "()"
  where
    isConstructorName s = case String.take 1 s of
      c -> c >= "A" && c <= "Z"

genExpr ctx (ExprApp f arg) =
  let { func, args } = collectArgs (ExprApp f arg)
  in case func of
    ExprVar name ->
      -- Check if it's a qualified name (like "Array.elem" from backtick syntax)
      case String.indexOf (String.Pattern ".") name of
        Just idx ->
          -- Resolve module alias to full name
          let modAlias = String.take idx name
              fullModName = resolveModuleName ctx modAlias
              funcName = String.drop (idx + 1) name
          in "call " <> atom (translateModuleName fullModName) <> ":" <> atom funcName <>
             "(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
        Nothing ->
          -- Handle special monad functions
          if name == "pure"
          then -- pure is Right for Either monad, but pureP for Parser monad
               if ctx.moduleName == "Nova.Compiler.CstParser"
               then "apply 'pureP'/1(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
               else "{" <> atom "Right" <> ", " <> String.joinWith ", " (map (genExpr ctx) args) <> "}"
          else
          -- Check if it's a prelude function
          case getPreludeFunc name of
            Just info ->
              -- Generate call to Erlang/stdlib function
              "call " <> atom info.mod <> ":" <> atom info.func <>
              "(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
            Nothing ->
              if Set.member name ctx.locals
              then "apply " <> coreVar name <>
                   "(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
              else if Set.member name ctx.moduleFuncs
              then let declaredArity = lookupArity name ctx
                       numArgs = length args
                   in if declaredArity == 0 && numArgs > 0
                      then -- Function returns a function - call it first, then apply result
                           "let <_Fn0> = apply " <> atom name <> "/0()\n" <>
                           "      in apply _Fn0(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
                      else if numArgs == declaredArity
                      then -- Exact match - call directly
                           "apply " <> atom name <> "/" <> show declaredArity <>
                           "(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
                      else if numArgs > declaredArity
                      then -- Over-application: function returns a curried function
                           -- Call with declared arity first, then apply result to remaining args
                           let directArgs = Array.take declaredArity args
                               extraArgs = Array.drop declaredArity args
                               baseCall = "apply " <> atom name <> "/" <> show declaredArity <>
                                         "(" <> String.joinWith ", " (map (genExpr ctx) directArgs) <> ")"
                           in genCurriedApply ctx baseCall extraArgs
                      else -- Partial application - generate a closure
                           let remaining = declaredArity - numArgs
                               paramNames = map (\i -> "_Pc" <> show i) (Array.range 0 (remaining - 1))
                               paramsStr = String.joinWith ", " paramNames
                               allArgs = String.joinWith ", " (map (genExpr ctx) args <> paramNames)
                           in "fun (" <> paramsStr <> ") -> apply " <> atom name <> "/" <> show declaredArity <>
                              "(" <> allArgs <> ")"
              else case Map.lookup name ctx.imports of
                   -- Imported function application
                   Just srcMod ->
                     let modName = translateModuleName srcMod
                         -- Library modules use uncurried, compiler modules use curried
                         isLibModule = String.take 5 srcMod == "Data." ||
                                       String.take 8 srcMod == "Control." ||
                                       String.take 5 srcMod == "Nova." && String.take 14 srcMod /= "Nova.Compiler."
                     in if isLibModule then
                          -- Library module: call with all args (uncurried)
                          "call " <> atom modName <> ":" <> atom name <>
                          "(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
                        else
                          -- Compiler module: call with first arg, apply remaining (curried)
                          case Array.uncons args of
                            Nothing ->
                              "call " <> atom modName <> ":" <> atom name <> "()"
                            Just { head: firstArg, tail: restArgs } ->
                              let baseCall = "call " <> atom modName <> ":" <> atom name <>
                                            "(" <> genExpr ctx firstArg <> ")"
                              in genCurriedApply ctx baseCall restArgs
                   Nothing ->
                     if isConstructorName name
                     then -- Data constructor application
                          -- Special case: Nil and Cons use native Erlang lists
                          if name == "Nil"
                          then "[]"
                          else if name == "Cons" && length args == 2
                          then case args of
                            [h, t] -> "[" <> genExpr ctx h <> "|" <> genExpr ctx t <> "]"
                            _ -> "{" <> atom (toSnakeCase name) <> ", " <> String.joinWith ", " (map (genExpr ctx) args) <> "}"
                          else if length args == 0
                          then atom (toSnakeCase name)
                          else "{" <> atom (toSnakeCase name) <> ", " <> String.joinWith ", " (map (genExpr ctx) args) <> "}"
                     else "apply " <> coreVar name <>
                          "(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
    ExprQualified modAlias funcName ->
      -- Resolve module alias to full name
      let fullModName = resolveModuleName ctx modAlias
          modName = translateModuleName fullModName
          -- All modules use uncurried calls in Core Erlang generation
          -- since we generate uncurried function definitions
      in "call " <> atom modName <> ":" <> atom funcName <>
         "(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
    _ ->
      "apply " <> genExpr ctx func <>
      "(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
  where
    isConstructorName s = case String.take 1 s of
      c -> c >= "A" && c <= "Z"

genExpr ctx (ExprLambda pats body) =
  -- Generate curried lambdas: \x y -> e becomes fun(x) -> fun(y) -> e end end
  case List.uncons pats of
    Nothing -> genExpr ctx body  -- No params, just the body
    Just { head: pat, tail: restPats } ->
      -- Check if this is the last param or if there are more
      if List.null restPats
      then
        -- Single param - generate one fun
        if hasComplexPatternSingle pat
        then
          let { var, ctx: ctx' } = freshVar ctx
              ctxWithParam = addLocalsFromPattern pat ctx'
              bodyCode = genExpr ctxWithParam body
              patResult = genPatternWithCounter pat 0
          in "fun (" <> var <> ") ->\n" <>
             "      case " <> var <> " of\n" <>
             "        <" <> patResult.str <> "> when 'true' -> " <> bodyCode <> "\n" <>
             "      end"
        else
          let patResult = genPatternWithCounter pat 0
              ctxWithParam = addLocalsFromPattern pat ctx
          in "fun (" <> patResult.str <> ") ->\n      " <> genExpr ctxWithParam body
      else
        -- Multiple params - curry: fun(x) -> (recursive for rest) end
        if hasComplexPatternSingle pat
        then
          let { var, ctx: ctx' } = freshVar ctx
              ctxWithParam = addLocalsFromPattern pat ctx'
              innerLambda = genExpr ctxWithParam (ExprLambda restPats body)
              patResult = genPatternWithCounter pat 0
          in "fun (" <> var <> ") ->\n" <>
             "      case " <> var <> " of\n" <>
             "        <" <> patResult.str <> "> when 'true' -> " <> innerLambda <> "\n" <>
             "      end"
        else
          let patResult = genPatternWithCounter pat 0
              ctxWithParam = addLocalsFromPattern pat ctx
              innerLambda = genExpr ctxWithParam (ExprLambda restPats body)
          in "fun (" <> patResult.str <> ") ->\n      " <> innerLambda

genExpr ctx (ExprLet binds body) =
  genLetBindsWithBody ctx (Array.fromFoldable binds) body

genExpr ctx (ExprIf cond thenE elseE) =
  "case " <> genExpr ctx cond <> " of\n" <>
  "      <" <> atom "true" <> "> when 'true' -> " <> genExpr ctx thenE <> "\n" <>
  "      <" <> atom "false" <> "> when 'true' -> " <> genExpr ctx elseE <> "\n" <>
  "    end"

genExpr ctx (ExprCase scrutinee clauses) =
  -- Handle case expressions with proper fallback for guarded wildcards
  let scrutExpr = genExpr ctx scrutinee
  in "case " <> scrutExpr <> " of\n" <>
     genCaseClausesWithFallback ctx scrutExpr clauses <> "\n" <>
     "    end"

genExpr ctx (ExprBinOp ":" l r) =
  "[" <> genExpr ctx l <> " | " <> genExpr ctx r <> "]"

genExpr ctx (ExprBinOp "<>" l r) =
  -- String/list append
  "call 'erlang':'++'(" <> genExpr ctx l <> ", " <> genExpr ctx r <> ")"

-- The # operator is "applyFlipped": x # f = f x
-- We must use let-binding since apply can't directly take a call result
genExpr ctx (ExprBinOp "#" l r) =
  "let <_HashF> = " <> genExpr ctx r <>
  " in apply _HashF(" <> genExpr ctx l <> ")"

-- The $ operator is function application: f $ x = f x
-- Special case: if left side is a constructor, generate tuple instead of apply
genExpr ctx (ExprBinOp "$" l r) =
  case l of
    ExprVar name | isConstructorName name ->
      -- Constructor application: Right $ x -> {'Right', x}
      "{" <> atom name <> ", " <> genExpr ctx r <> "}"
    _ ->
      "let <_DollarF> = " <> genExpr ctx l <>
      " in apply _DollarF(" <> genExpr ctx r <> ")"
  where
    isConstructorName s = case String.take 1 s of
      c -> c >= "A" && c <= "Z"

-- The <|> operator is Parser alternative - call alt function
genExpr ctx (ExprBinOp "<|>" l r) =
  "apply 'alt'/2(" <> genExpr ctx l <> ", " <> genExpr ctx r <> ")"

-- The <$> operator is functor map - for Parser: f <$> p = bindP(p, \x -> pureP(f x))
genExpr ctx (ExprBinOp "<$>" l r) =
  if ctx.moduleName == "Nova.Compiler.CstParser"
  then "apply 'bindP'/2(" <> genExpr ctx r <> ", fun (_MapX) ->\n      apply 'pureP'/1(apply " <> genExpr ctx l <> "(_MapX)))"
  else "call 'erlang':'<$>'(" <> genExpr ctx l <> ", " <> genExpr ctx r <> ")"

-- The *> operator is sequence-right - for Parser: p1 *> p2 = bindP(p1, \_ -> p2)
genExpr ctx (ExprBinOp "*>" l r) =
  if ctx.moduleName == "Nova.Compiler.CstParser"
  then "apply 'thenP'/2(" <> genExpr ctx l <> ", " <> genExpr ctx r <> ")"
  else "call 'erlang':'*>'(" <> genExpr ctx l <> ", " <> genExpr ctx r <> ")"

genExpr ctx (ExprBinOp op l r) =
  -- Check if it's a qualified operator like "Array.elem"
  case String.indexOf (String.Pattern ".") op of
    Just idx ->
      -- Resolve module alias to full name
      let modAlias = String.take idx op
          fullModName = resolveModuleName ctx modAlias
          funcName = String.drop (idx + 1) op
      in "call " <> atom (translateModuleName fullModName) <> ":" <> atom funcName <> "(" <> genExpr ctx l <> ", " <> genExpr ctx r <> ")"
    Nothing ->
      -- Special handling for short-circuit operators (not functions in Erlang)
      case op of
        "&&" ->
          -- l && r -> case l of true -> r ; _ -> false end
          "case " <> genExpr ctx l <> " of <'true'> when 'true' -> " <> genExpr ctx r <>
          " <_> when 'true' -> 'false' end"
        "||" ->
          -- l || r -> case l of true -> true ; _ -> r end
          "case " <> genExpr ctx l <> " of <'true'> when 'true' -> 'true'" <>
          " <_> when 'true' -> " <> genExpr ctx r <> " end"
        _ ->
          let erlOp = translateOp op
          in "call 'erlang':'" <> erlOp <> "'(" <> genExpr ctx l <> ", " <> genExpr ctx r <> ")"

genExpr ctx (ExprUnaryOp "-" e) =
  "call 'erlang':'-'(" <> genExpr ctx e <> ")"

genExpr ctx (ExprUnaryOp "!" e) =
  "call 'erlang':'not'(" <> genExpr ctx e <> ")"

genExpr ctx (ExprUnaryOp _ e) = genExpr ctx e

genExpr ctx (ExprList elems) =
  -- Core Erlang uses nested cons: [1|[2|[3]]]
  genCoreList ctx elems

genExpr ctx (ExprTuple elems) =
  "{" <> String.joinWith ", " (Array.fromFoldable (map (genExpr ctx) elems)) <> "}"

genExpr ctx (ExprRecord fields) =
  -- Records as maps: ~{'key'=>Val}~
  "~{" <> String.joinWith "," (Array.fromFoldable (map genField fields)) <> "}~"
  where
    genField (Tuple label expr) = atom (toSnakeCase label) <> "=>" <> genExpr ctx expr

genExpr ctx (ExprRecordAccess expr field) =
  -- Check if this is a record accessor function like _.id
  case expr of
    ExprSection "_" ->
      -- Generate a lambda: fun (_Ra) -> call 'maps':'get'('field', _Ra)
      "fun (_Ra) -> call 'maps':'get'(" <> atom (toSnakeCase field) <> ", _Ra)"
    _ ->
      "call 'maps':'get'(" <> atom (toSnakeCase field) <> ", " <> genExpr ctx expr <> ")"

genExpr ctx (ExprRecordUpdate expr fields) =
  let updates = String.joinWith "," (Array.fromFoldable (map genFieldUpdate fields))
  in "call 'maps':'merge'(" <> genExpr ctx expr <> ", ~{" <> updates <> "}~)"
  where
    genFieldUpdate (Tuple label val) = atom (toSnakeCase label) <> "=>" <> genExpr ctx val

genExpr ctx (ExprParens e) = genExpr ctx e

genExpr ctx (ExprDo stmts) = genDoStmts ctx (Array.fromFoldable stmts)

genExpr ctx (ExprTyped e _) = genExpr ctx e

genExpr _ctx (ExprSection op) =
  "fun (X, Y) -> call 'erlang':'" <> translateOp op <> "'(X, Y)"

genExpr ctx (ExprSectionLeft e op) =
  "fun (X) -> call 'erlang':'" <> translateOp op <> "'(" <> genExpr ctx e <> ", X)"

genExpr ctx (ExprSectionRight op e) =
  "fun (X) -> call 'erlang':'" <> translateOp op <> "'(X, " <> genExpr ctx e <> ")"

-- | Generate expression for letrec context - uses uncurried lambdas to match declared arity
genExprLetrec :: CoreCtx -> Expr -> String
genExprLetrec ctx expr = case expr of
  ExprLambda pats body ->
    -- Generate uncurried lambda: fun (X, Y, Z) -> body end
    if hasComplexPattern pats
    then
      let arity = List.length pats
          paramNames = map (\i -> "_L" <> show i) (Array.range 0 (arity - 1))
          paramsStr = String.joinWith ", " paramNames
          patResult = genPatsWithCounter (Array.fromFoldable pats) 0
          patTuple = "{" <> String.joinWith ", " patResult.strs <> "}"
          ctxWithParams = foldr addLocalsFromPattern ctx pats
          bodyCode = genExpr ctxWithParams body
      in "fun (" <> paramsStr <> ") ->\n" <>
         "      case {" <> paramsStr <> "} of\n" <>
         "        <" <> patTuple <> "> when 'true' -> " <> bodyCode <> "\n" <>
         "      end"
    else
      let patResult = genPatsWithCounter (Array.fromFoldable pats) 0
          ctxWithParams = foldr addLocalsFromPattern ctx pats
      in "fun (" <> String.joinWith ", " patResult.strs <> ") ->\n      " <> genExpr ctxWithParams body
  _ -> genExpr ctx expr

-- | Check if an expression references a variable name
exprContainsVar :: String -> Expr -> Boolean
exprContainsVar name expr = case expr of
  ExprVar v -> v == name
  ExprQualified _ _ -> false
  ExprLit _ -> false
  ExprApp f a -> exprContainsVar name f || exprContainsVar name a
  ExprLambda _ body -> exprContainsVar name body
  ExprLet binds body -> List.any (\b -> exprContainsVar name b.value) binds || exprContainsVar name body
  ExprIf c t e -> exprContainsVar name c || exprContainsVar name t || exprContainsVar name e
  ExprCase scrut clauses -> exprContainsVar name scrut || List.any (\cl -> exprContainsVar name cl.body) clauses
  ExprDo stmts -> List.any (doStmtContainsVar name) stmts
  ExprBinOp _ l r -> exprContainsVar name l || exprContainsVar name r
  ExprUnaryOp _ e -> exprContainsVar name e
  ExprList elems -> List.any (exprContainsVar name) elems
  ExprTuple elems -> List.any (exprContainsVar name) elems
  ExprRecord fields -> List.any (\(Tuple _ e) -> exprContainsVar name e) fields
  ExprRecordAccess e _ -> exprContainsVar name e
  ExprRecordUpdate e updates -> exprContainsVar name e || List.any (\(Tuple _ e') -> exprContainsVar name e') updates
  ExprTyped e _ -> exprContainsVar name e
  ExprParens e -> exprContainsVar name e
  ExprSection _ -> false
  ExprSectionLeft e _ -> exprContainsVar name e
  ExprSectionRight _ e -> exprContainsVar name e

doStmtContainsVar :: String -> DoStatement -> Boolean
doStmtContainsVar name stmt = case stmt of
  DoLet binds -> List.any (\b -> exprContainsVar name b.value) binds
  DoBind _ e -> exprContainsVar name e
  DoExpr e -> exprContainsVar name e

-- | Get variable name from pattern (for simple PatVar)
getPatternVarName :: Pattern -> Maybe String
getPatternVarName (PatVar n) = Just n
getPatternVarName _ = Nothing

-- | Check if a let binding is recursive
isRecursiveBind :: LetBind -> Boolean
isRecursiveBind bind = case getPatternVarName bind.pattern of
  Just name -> exprContainsVar name bind.value
  Nothing -> false

-- | Get arity from lambda expression
getLambdaArity :: Expr -> Int
getLambdaArity (ExprLambda pats _) = List.length pats
getLambdaArity (ExprParens e) = getLambdaArity e
getLambdaArity _ = 0

-- | Check if a pattern is a simple variable (can be used in Core Erlang let)
isSimplePattern :: Pattern -> Boolean
isSimplePattern (PatVar _) = true
isSimplePattern PatWildcard = true
isSimplePattern _ = false

-- | Generate let bindings with body
-- Handles mutually recursive function bindings by grouping them in a single letrec
-- Non-function bindings are generated as regular let bindings
genLetBindsWithBody :: CoreCtx -> Array LetBind -> Expr -> String
genLetBindsWithBody ctx binds body =
  let -- Separate function bindings (lambdas with arity > 0) from value bindings
      isLambdaBind b = getLambdaArity b.value > 0
      funcBinds = Array.filter isLambdaBind binds
      valueBinds = Array.filter (not <<< isLambdaBind) binds

      -- For letrec functions, add them to moduleFuncs with their arities
      -- This makes genExpr generate proper 'apply funcname/arity(...)' calls
      letrecFuncs = Array.mapMaybe (\b ->
        case getPatternVarName b.pattern of
          Nothing -> Nothing
          Just n -> Just { name: n, arity: getLambdaArity b.value }) funcBinds

      -- Collect variables used by letrec functions
      funcNames = Set.fromFoldable (map _.name letrecFuncs)
      usedByFuncs = Set.fromFoldable (Array.concatMap (\b -> getUsedVars b.value) funcBinds)

      -- Compute transitive dependencies on letrec functions
      -- A value transitively depends on funcs if it directly uses a func name,
      -- OR if it uses another value that transitively depends on funcs.
      -- We iterate to a fixpoint.
      computeTransitiveFuncDeps :: Set String -> Set String
      computeTransitiveFuncDeps currentDeps =
        let depsOrFuncs = Set.union funcNames currentDeps
            newDeps = Set.fromFoldable $ Array.mapMaybe (\b ->
              let used = Set.fromFoldable (getUsedVars b.value)
              in if not (Set.isEmpty (Set.intersection used depsOrFuncs))
                 then getPatternVarName b.pattern
                 else Nothing
            ) valueBinds
        in if newDeps == currentDeps
           then newDeps
           else computeTransitiveFuncDeps newDeps

      -- Values that transitively depend on funcs (by name)
      valuesThatDependOnFuncs = computeTransitiveFuncDeps Set.empty

      -- Check if a value binding depends on any letrec function (directly or transitively)
      dependsOnFuncs :: LetBind -> Boolean
      dependsOnFuncs b = case getPatternVarName b.pattern of
        Just n -> Set.member n valuesThatDependOnFuncs
        Nothing ->
          -- For pattern bindings without a simple name, check directly
          let used = Set.fromFoldable (getUsedVars b.value)
          in not (Set.isEmpty (Set.intersection used (Set.union funcNames valuesThatDependOnFuncs)))

      -- Check if a value binding is used by any letrec function
      isUsedByFuncs :: LetBind -> Boolean
      isUsedByFuncs b = case getPatternVarName b.pattern of
        Just n -> Set.member n usedByFuncs
        Nothing -> false

      -- Values that depend on funcs AND are used by funcs → must become 0-arity letrec functions
      -- Values that DON'T depend on funcs → BEFORE letrec
      -- Values that depend on funcs but NOT used by funcs → AFTER letrec
      --
      -- IMPORTANT: Cyclicity must propagate! If a cyclic value depends on another value,
      -- that value must also be cyclic (in the letrec) or the cyclic value will have
      -- an unbound reference.

      -- Start with values that are directly cyclic (used by funcs AND depend on funcs)
      directlyCyclic = Array.filter (\b -> dependsOnFuncs b && isUsedByFuncs b) valueBinds
      directlyCyclicNames = Set.fromFoldable $ Array.mapMaybe (\b -> getPatternVarName b.pattern) directlyCyclic

      -- Propagate cyclicity: if a cyclic value uses another value, that value must also be cyclic
      computeCyclicClosure :: Set String -> Set String
      computeCyclicClosure currentCyclic =
        let cyclicBinds = Array.filter (\b -> case getPatternVarName b.pattern of
              Just n -> Set.member n currentCyclic
              Nothing -> false) valueBinds
            usedByCyclic = Set.fromFoldable $ Array.concatMap (\b -> getUsedVars b.value) cyclicBinds
            -- Value names that are used by cyclic values and are themselves values (not funcs)
            valueNames = Set.fromFoldable $ Array.mapMaybe (\b -> getPatternVarName b.pattern) valueBinds
            newCyclic = Set.union currentCyclic (Set.intersection usedByCyclic valueNames)
        in if newCyclic == currentCyclic
           then newCyclic
           else computeCyclicClosure newCyclic

      allCyclicNames = computeCyclicClosure directlyCyclicNames
      cyclicValues = Array.filter (\b -> case getPatternVarName b.pattern of
        Just n -> Set.member n allCyclicNames
        Nothing -> false) valueBinds

      valuesBeforeLetrec = Array.filter (\b -> not (dependsOnFuncs b)) valueBinds
      valuesAfterLetrec = Array.filter (\b -> dependsOnFuncs b && case getPatternVarName b.pattern of
        Just n -> not (Set.member n allCyclicNames)
        Nothing -> true) valueBinds

      -- Convert cyclic values to 0-arity function bindings
      cyclicAsFuncs = map (\b -> b { value = ExprLambda Nil b.value }) cyclicValues
      funcGroup1 = funcBinds <> cyclicAsFuncs
      funcGroup2 = []  -- No second function group
      depValuesGroup1 = valuesAfterLetrec  -- Values that depend on functions go after
      depValuesGroup2 = []  -- No second value group

      -- Add cyclic values to letrecFuncs with arity 0
      cyclicFuncs = Array.mapMaybe (\b ->
        case getPatternVarName b.pattern of
          Nothing -> Nothing
          Just n -> Just { name: n, arity: 0 }) cyclicValues
      allFuncs = letrecFuncs <> cyclicFuncs

      -- Add all function bindings to moduleFuncs for proper apply syntax
      ctxWithFuncs = ctx { moduleFuncs = foldr (\f s -> Set.insert f.name s) ctx.moduleFuncs allFuncs
                        , funcArities = ctx.funcArities <> allFuncs
                        }

      -- Add remaining value bindings (non-cyclic) to locals
      nonCyclicValues = valuesBeforeLetrec <> valuesAfterLetrec
      ctxWithAllBinds = foldr addValueBindToCtx ctxWithFuncs nonCyclicValues

  in if Array.null funcBinds
     then genValueBindsWithBody ctxWithAllBinds valueBinds body
     else -- Generate: independent values -> letrec1 -> depValues1 -> letrec2 -> depValues2 -> body
          let -- Group function bindings by name (for multi-clause local functions)
              groupByName :: Array LetBind -> Array { name :: String, binds :: Array LetBind, arity :: Int }
              groupByName binds_ =
                let names = Array.nub (Array.mapMaybe (\b -> getPatternVarName b.pattern) binds_)
                    mkGroup n =
                      let matching = Array.filter (\b -> getPatternVarName b.pattern == Just n) binds_
                          arity = case Array.head matching of
                            Just b -> getLambdaArity b.value
                            Nothing -> 0
                      in { name: n, binds: matching, arity: arity }
                in map mkGroup names

              genLetrec :: Array LetBind -> String -> String
              genLetrec fs bodyStr =
                if Array.null fs
                then bodyStr
                else let grouped = groupByName fs
                         defs = String.joinWith "\n       " (map (genLetrecDefGrouped ctxWithAllBinds) grouped)
                     in "letrec " <> defs <> "\n      in " <> bodyStr

              -- Build from inside out: body <- depValues2 <- letrec2 <- depValues1 <- letrec1 <- valuesBeforeLetrec
              finalBody = genExpr ctxWithAllBinds body
              afterDepValues2 = if Array.null depValuesGroup2
                                then finalBody
                                else genValueBindsWithBodyStrSorted ctxWithAllBinds (topoSortBinds depValuesGroup2) finalBody
              afterLetrec2 = genLetrec funcGroup2 afterDepValues2
              afterDepValues1 = if Array.null depValuesGroup1
                                then afterLetrec2
                                else genValueBindsWithBodyStrSorted ctxWithAllBinds (topoSortBinds depValuesGroup1) afterLetrec2
              afterLetrec1 = genLetrec funcGroup1 afterDepValues1
              -- Values that don't depend on letrec functions go BEFORE the letrec
              result = if Array.null valuesBeforeLetrec
                       then afterLetrec1
                       else genValueBindsWithBodyStrSorted ctxWithAllBinds (topoSortBinds valuesBeforeLetrec) afterLetrec1
          in result
  where
    -- Add a value binding's name to locals
    addValueBindToCtx :: LetBind -> CoreCtx -> CoreCtx
    addValueBindToCtx bind ctx' =
      let bindName = getPatternVarName bind.pattern
      in case bindName of
        Just n -> ctx' { locals = Set.insert n ctx'.locals }
        Nothing -> addLocalsFromPattern bind.pattern ctx'

    -- Generate a single function definition for letrec
    -- Uses atom with original name to match apply call syntax: 'funcName'/arity
    genLetrecDef :: CoreCtx -> LetBind -> String
    genLetrecDef ctx' bind =
      let bindName = fromMaybe "_anon" (getPatternVarName bind.pattern)
          arity = getLambdaArity bind.value
          -- Use uncurried lambda generation for letrec to match declared arity
          val = genExprLetrec ctx' bind.value
      in atom bindName <> "/" <> show arity <> " = " <> val

    -- Generate a grouped function definition for letrec
    -- Handles multiple clauses with the same function name by merging them into a case
    genLetrecDefGrouped :: CoreCtx -> { name :: String, binds :: Array LetBind, arity :: Int } -> String
    genLetrecDefGrouped ctx' group =
      case group.binds of
        [] -> ""
        [single] -> genLetrecDef ctx' single  -- Single clause - use simple generation
        multiple ->
          -- Multiple clauses - generate a function with case
          let paramNames = map (\i -> "_L" <> show i) (Array.range 0 (group.arity - 1))
              paramsStr = String.joinWith ", " paramNames
              -- Generate case clauses from each binding's lambda
              caseClauses = map (genLetrecClauseAsCase ctx' paramNames) multiple
          in atom group.name <> "/" <> show group.arity <> " = fun (" <> paramsStr <> ") ->\n" <>
             "      case {" <> paramsStr <> "} of\n" <>
             String.joinWith "\n" caseClauses <> "\n" <>
             "      end"

    -- Generate a letrec clause as a case clause
    -- Extracts patterns from the lambda and generates a case clause
    genLetrecClauseAsCase :: CoreCtx -> Array String -> LetBind -> String
    genLetrecClauseAsCase ctx' paramNames' bind =
      case bind.value of
        ExprLambda pats lambdaBody ->
          let patsResult = genPatsWithCounter (Array.fromFoldable pats) 0
              patTuple = "{" <> String.joinWith ", " patsResult.strs <> "}"
              ctxWithParams = foldr addLocalsFromPattern ctx' pats
              bodyStr = genExpr ctxWithParams lambdaBody
          in "        <" <> patTuple <> "> when 'true' -> " <> bodyStr
        ExprParens e -> genLetrecClauseAsCase ctx' paramNames' (bind { value = e })
        _ ->
          -- Shouldn't happen - we only call this for lambda bindings
          "        <_W0> when 'true' -> " <> genExpr ctx' bind.value

-- | Generate value bindings (non-lambda) with body expression
genValueBindsWithBody :: CoreCtx -> Array LetBind -> Expr -> String
genValueBindsWithBody ctx binds body =
  -- Topologically sort bindings to handle forward references
  let sortedBinds = topoSortBinds binds
  in genValueBindsWithBodyStrSorted ctx sortedBinds (genExpr ctx body)

-- | Generate value bindings with a string body (used when body is already generated)
genValueBindsWithBodyStr :: CoreCtx -> Array LetBind -> String -> String
genValueBindsWithBodyStr ctx binds bodyStr =
  -- Topologically sort bindings to handle forward references
  let sortedBinds = topoSortBinds binds
  in genValueBindsWithBodyStrSorted ctx sortedBinds bodyStr

-- | Generate sorted value bindings (internal, assumes already sorted)
genValueBindsWithBodyStrSorted :: CoreCtx -> Array LetBind -> String -> String
genValueBindsWithBodyStrSorted ctx binds bodyStr =
  case Array.uncons binds of
    Nothing -> bodyStr
    Just { head: bind, tail: rest } ->
      let pat = genPattern bind.pattern
          val = genExpr ctx bind.value
          tmpVar = "_Let" <> show ctx.varCounter
          nextCtx = ctx { varCounter = ctx.varCounter + 1 }
          continuation = genValueBindsWithBodyStrSorted ctx rest bodyStr
          continuationComplex = genValueBindsWithBodyStrSorted nextCtx rest bodyStr
      in if isSimplePattern bind.pattern
         then "let <" <> pat <> "> = " <> val <> "\n      in " <> continuation
         else -- Complex patterns: use temp var and case wrapping the continuation
              "let <" <> tmpVar <> "> = " <> val <> "\n      in case " <> tmpVar <> " of\n        <" <> pat <> "> when 'true' -> " <> continuationComplex <> "\n      end"

-- | Legacy genLetBinds for do-notation (returns prefix + context)
genLetBinds :: CoreCtx -> Array LetBind -> { fst :: String, snd :: CoreCtx }
genLetBinds ctx binds =
  foldr genBind { fst: "", snd: ctx } binds
  where
    genBind bind acc =
      let pat = genPattern bind.pattern
          bindNames = getPatternVarName bind.pattern
          isRec = isRecursiveBind bind
          arity = getLambdaArity bind.value
          ctxForVal = case bindNames of
            Just n | isRec ->
              acc.snd { moduleFuncs = Set.insert n acc.snd.moduleFuncs
                      , funcArities = acc.snd.funcArities <> [{ name: n, arity: arity }]
                      }
            _ -> acc.snd
          val = genExpr ctxForVal bind.value
          newCtx = if isRec
                   then ctxForVal
                   else addLocalsFromPattern bind.pattern acc.snd
          funcName = case bindNames of
            Just n -> toSnakeCase n
            Nothing -> "_anon"
      in if isRec
         then { fst: "letrec " <> atom funcName <> "/" <> show arity <> " = " <> val <> "\n      in " <> acc.fst, snd: newCtx }
         else { fst: "let <" <> pat <> "> = " <> val <> "\n      in " <> acc.fst, snd: newCtx }

-- | Check if a pattern is a wildcard (matches anything)
isWildcardPattern :: Pattern -> Boolean
isWildcardPattern (PatVar _) = true
isWildcardPattern PatWildcard = true
isWildcardPattern _ = false

-- | Generate case clauses with proper fallback for guarded patterns
genCaseClausesWithFallback :: CoreCtx -> String -> List CaseClause -> String
genCaseClausesWithFallback ctx scrutExpr clauses = case List.uncons clauses of
  Nothing -> "      <_> when 'true' -> primop 'match_fail'({'case_clause', 'no_clause'})"
  Just { head: clause, tail: rest } ->
    -- If this is a guarded wildcard, chain with rest as fallback
    if isWildcardPattern clause.pattern && isJust clause.guard
    then genGuardedWildcardClause ctx scrutExpr clause rest
    else
      -- For guarded non-wildcards, we need to collect all clauses with the same pattern
      -- to avoid generating duplicate Core Erlang case clauses
      let { samePattern, different } = splitSamePatternClauses clause.pattern rest
          -- Build fallback chain from all clauses with same pattern (after current) + the different ones
          ctxWithVars = addLocalsFromPattern clause.pattern ctx
          fallback = if List.null samePattern && (List.null different || isNothing clause.guard)
                     then Nothing
                     else Just (genFallbackForSamePattern ctxWithVars scrutExpr samePattern different)
      in genCaseClause ctx clause fallback <>
         -- Only continue with clauses that have DIFFERENT patterns
         (if List.null different then "" else "\n" <> genCaseClausesWithFallback ctx scrutExpr different)

-- | Split clauses into those with the same pattern (must have guards) and those with different patterns
splitSamePatternClauses :: Pattern -> List CaseClause -> { samePattern :: List CaseClause, different :: List CaseClause }
splitSamePatternClauses pat clauses =
  let isSameGuarded c = genPattern c.pattern == genPattern pat && isJust c.guard
      samePattern = List.takeWhile isSameGuarded clauses
      different = List.drop (List.length samePattern) clauses
  in { samePattern, different }

-- | Generate fallback for clauses with the same pattern, then try different patterns
genFallbackForSamePattern :: CoreCtx -> String -> List CaseClause -> List CaseClause -> String
genFallbackForSamePattern ctx scrutExpr samePattern different = case List.uncons samePattern of
  Nothing ->
    -- No more same-pattern clauses, try different patterns
    genFallbackForDifferent ctx scrutExpr different
  Just { head: clause, tail: rest } ->
    -- Generate nested guard check for this clause
    let guardExpr = case clause.guard of
          Just g -> genExpr ctx g
          Nothing -> "'true'"
        body = genExpr ctx clause.body
        fallback = genFallbackForSamePattern ctx scrutExpr rest different
    in "case " <> guardExpr <> " of\n" <>
       "            <'true'> when 'true' -> " <> body <> "\n" <>
       "            <_> when 'true' -> " <> fallback <> "\n" <>
       "          end"

-- | Generate fallback that tries remaining clauses with different patterns
genFallbackForDifferent :: CoreCtx -> String -> List CaseClause -> String
genFallbackForDifferent ctx scrutExpr clauses = case List.uncons clauses of
  Nothing -> "primop 'match_fail'({'case_clause', 'no_match'})"
  Just { head: clause, tail: rest } ->
    if isWildcardPattern clause.pattern
    then
      -- Wildcard - either guarded or unguarded
      case clause.guard of
        Just g ->
          let body = genExpr ctx clause.body
              fallback = genFallbackForDifferent ctx scrutExpr rest
          in "case " <> genExpr ctx g <> " of\n" <>
             "            <'true'> when 'true' -> " <> body <> "\n" <>
             "            <_> when 'true' -> " <> fallback <> "\n" <>
             "          end"
        Nothing -> genExpr ctx clause.body
    else
      -- Non-wildcard pattern - re-match the scrutinee against remaining clauses
      -- Generate a nested case expression
      "case " <> scrutExpr <> " of\n" <>
      genCaseClausesWithFallback ctx scrutExpr clauses <> "\n" <>
      "          end"

-- | Generate a guarded wildcard clause with fallback to remaining clauses
genGuardedWildcardClause :: CoreCtx -> String -> CaseClause -> List CaseClause -> String
genGuardedWildcardClause ctx scrutExpr clause rest =
  let pat = genPattern clause.pattern
      ctxWithVars = addLocalsFromPattern clause.pattern ctx
      guardExpr = case clause.guard of
        Just g -> genExpr ctxWithVars g
        Nothing -> "'true'"  -- shouldn't happen
      body = genExpr ctxWithVars clause.body
      -- Fallback: try remaining clauses (wildcards can match anything, so all are "different")
      fallback = if List.null rest
                 then "primop 'match_fail'({'case_clause', 'guard_failed'})"
                 else genFallbackForDifferent ctxWithVars scrutExpr rest
  in "      <" <> pat <> "> when 'true' ->\n" <>
     "        case " <> guardExpr <> " of\n" <>
     "          <'true'> when 'true' -> " <> body <> "\n" <>
     "          <_> when 'true' -> " <> fallback <> "\n" <>
     "        end"

-- | Generate case clause with optional fallback for guard failure
genCaseClause :: CoreCtx -> CaseClause -> Maybe String -> String
genCaseClause ctx clause fallback =
  let pat = genPattern clause.pattern
      ctxWithVars = addLocalsFromPattern clause.pattern ctx
      body = case clause.guard of
        Nothing -> genExpr ctxWithVars clause.body
        Just g ->
          -- For guarded patterns, when guard fails, try fallback or raise error
          let fallbackCode = case fallback of
                Just fb -> fb
                Nothing -> "primop 'match_fail'({'case_clause', " <> genExpr ctxWithVars g <> "})"
          in "case " <> genExpr ctxWithVars g <> " of\n" <>
             "          <'true'> when 'true' -> " <> genExpr ctxWithVars clause.body <> "\n" <>
             "          <_> when 'true' -> " <> fallbackCode <> "\n" <>
             "        end"
  in "      <" <> pat <> "> when 'true' ->\n        " <> body

-- | Generate do-notation (desugar to nested lets/applies)
-- Handles Parser monad (in CstParser) and Maybe/Either monads elsewhere
genDoStmts :: CoreCtx -> Array DoStatement -> String
genDoStmts ctx stmts =
  if isParserModule ctx.moduleName
  then genDoStmtsParser ctx stmts
  else genDoStmtsMaybeEither ctx stmts
  where
    isParserModule modName = modName == "Nova.Compiler.CstParser"

-- | Generate do-notation for Parser monad using bindP
genDoStmtsParser :: CoreCtx -> Array DoStatement -> String
genDoStmtsParser ctx stmts = case Array.uncons stmts of
  Nothing -> "apply 'pureP'/1('unit')"
  Just { head: DoExpr e, tail: [] } -> genExpr ctx e
  Just { head: DoExpr e, tail: rest } ->
    -- For Parser: e *> rest becomes bindP(e, \_ -> rest)
    let { var, ctx: ctx' } = freshVar ctx
        restCode = genDoStmtsParser ctx' rest
    in "apply 'bindP'/2(" <> genExpr ctx e <> ", fun (" <> var <> ") ->\n      " <> restCode <> ")"
  Just { head: DoBind pat e, tail: rest } ->
    let ctxWithBind = addLocalsFromPattern pat ctx
        bindExpr = genExpr ctx e
        patCode = genPattern pat
    in if isSimplePattern pat
       then -- Simple pattern: use directly in fun parameter
            let restCode = genDoStmtsParser ctxWithBind rest
            in "apply 'bindP'/2(" <> bindExpr <> ", fun (" <> patCode <> ") ->\n      " <> restCode <> ")"
       else -- Complex pattern: use temp var and case expression
            let { var, ctx: ctx' } = freshVar ctx
                ctxWithBind' = addLocalsFromPattern pat ctx'
                restCode = genDoStmtsParser ctxWithBind' rest
            in "apply 'bindP'/2(" <> bindExpr <> ", fun (" <> var <> ") ->\n      case " <> var <> " of\n        <" <> patCode <> "> when 'true' -> " <> restCode <> "\n      end)"
  Just { head: DoLet binds, tail: rest } ->
    genDoLetWithBodyParser ctx (Array.fromFoldable binds) (Array.fromFoldable rest)

-- | Generate do-notation for Maybe/Either monads
genDoStmtsMaybeEither :: CoreCtx -> Array DoStatement -> String
genDoStmtsMaybeEither ctx stmts = case Array.uncons stmts of
  Nothing -> atom "unit"
  Just { head: DoExpr e, tail: [] } -> genExpr ctx e
  Just { head: DoExpr e, tail: rest } ->
    let { var, ctx: ctx' } = freshVar ctx
    in "let <" <> var <> "> = " <> genExpr ctx e <> "\n      in " <> genDoStmtsMaybeEither ctx' rest
  Just { head: DoBind pat e, tail: rest } ->
    let ctxWithBind = addLocalsFromPattern pat ctx
        bindExpr = genExpr ctx e
        -- Handle both Maybe and Either monads:
        -- Maybe: Nothing propagates, {Just, x} extracts x
        -- Either: {Left, err} propagates, {Right, x} extracts x
        restCode = genDoStmtsMaybeEither ctxWithBind rest
        patCode = genPattern pat
    in "case " <> bindExpr <> " of\n" <>
       "      <'Nothing'> when 'true' ->\n        'Nothing'\n" <>
       "      <{'Left', _Err}> when 'true' ->\n        {'Left', _Err}\n" <>
       "      <{'Just', " <> patCode <> "}> when 'true' ->\n        " <> restCode <> "\n" <>
       "      <{'Right', " <> patCode <> "}> when 'true' ->\n        " <> restCode <> "\n      end"
  Just { head: DoLet binds, tail: rest } ->
    genDoLetWithBody ctx (Array.fromFoldable binds) (Array.fromFoldable rest)

-- | Generate DoLet with proper complex pattern handling
genDoLetWithBody :: CoreCtx -> Array LetBind -> Array DoStatement -> String
genDoLetWithBody ctx binds rest =
  case Array.uncons binds of
    Nothing -> genDoStmts ctx rest
    Just { head: bind, tail: moreBinds } ->
      let pat = genPattern bind.pattern
          bindNames = getPatternVarName bind.pattern
          isRec = isRecursiveBind bind
          arity = getLambdaArity bind.value
          ctxForVal = case bindNames of
            Just n | isRec ->
              ctx { moduleFuncs = Set.insert n ctx.moduleFuncs
                  , funcArities = ctx.funcArities <> [{ name: n, arity: arity }]
                  }
            _ -> ctx
          val = genExpr ctxForVal bind.value
          newCtx = if isRec
                   then ctxForVal
                   else addLocalsFromPattern bind.pattern ctx
          funcName = case bindNames of
            Just n -> toSnakeCase n
            Nothing -> "_anon"
          tmpVar = "_DLet" <> show newCtx.varCounter
          nextCtx = newCtx { varCounter = newCtx.varCounter + 1 }
          -- Continuation is the rest of the binds plus remaining do statements
          continuation = genDoLetWithBody newCtx moreBinds rest
          continuationComplex = genDoLetWithBody nextCtx moreBinds rest
      in if isRec
         then "letrec " <> atom funcName <> "/" <> show arity <> " = " <> val <> "\n      in " <> continuation
         else if isSimplePattern bind.pattern
              then "let <" <> pat <> "> = " <> val <> "\n      in " <> continuation
              else "let <" <> tmpVar <> "> = " <> val <> "\n      in case " <> tmpVar <> " of\n        <" <> pat <> "> when 'true' -> " <> continuationComplex <> "\n      end"

-- | Generate DoLet for Parser context (same logic, but uses genDoStmtsParser for continuation)
genDoLetWithBodyParser :: CoreCtx -> Array LetBind -> Array DoStatement -> String
genDoLetWithBodyParser ctx binds rest =
  case Array.uncons binds of
    Nothing -> genDoStmtsParser ctx rest
    Just { head: bind, tail: moreBinds } ->
      let pat = genPattern bind.pattern
          bindNames = getPatternVarName bind.pattern
          isRec = isRecursiveBind bind
          arity = getLambdaArity bind.value
          ctxForVal = case bindNames of
            Just n | isRec ->
              ctx { moduleFuncs = Set.insert n ctx.moduleFuncs
                  , funcArities = ctx.funcArities <> [{ name: n, arity: arity }]
                  }
            _ -> ctx
          val = genExpr ctxForVal bind.value
          newCtx = if isRec
                   then ctxForVal
                   else addLocalsFromPattern bind.pattern ctx
          funcName = case bindNames of
            Just n -> toSnakeCase n
            Nothing -> "_anon"
          tmpVar = "_DLet" <> show newCtx.varCounter
          nextCtx = newCtx { varCounter = newCtx.varCounter + 1 }
          continuation = genDoLetWithBodyParser newCtx moreBinds rest
          continuationComplex = genDoLetWithBodyParser nextCtx moreBinds rest
      in if isRec
         then "letrec " <> atom funcName <> "/" <> show arity <> " = " <> val <> "\n      in " <> continuation
         else if isSimplePattern bind.pattern
              then "let <" <> pat <> "> = " <> val <> "\n      in " <> continuation
              else "let <" <> tmpVar <> "> = " <> val <> "\n      in case " <> tmpVar <> " of\n        <" <> pat <> "> when 'true' -> " <> continuationComplex <> "\n      end"

-- | Generate literal
genLiteral :: Literal -> String
genLiteral (LitInt n) = show n
genLiteral (LitNumber n) = show n
genLiteral (LitString s) = "\"" <> escapeString s <> "\""
genLiteral (LitChar c) = "$" <> escapeChar c
genLiteral (LitBool true) = atom "true"
genLiteral (LitBool false) = atom "false"

-- | Escape string for Core Erlang
-- Order matters: escape backslashes FIRST, then other chars, then special chars last
escapeString :: String -> String
escapeString s =
  -- First escape existing backslashes
  let s1 = String.replaceAll (String.Pattern "\\") (String.Replacement "\\\\") s
      -- Then escape quotes
      s2 = String.replaceAll (String.Pattern "\"") (String.Replacement "\\\"") s1
      -- Then escape special characters (these add new backslashes that shouldn't be doubled)
      s3 = String.replaceAll (String.Pattern "\n") (String.Replacement "\\n") s2
      s4 = String.replaceAll (String.Pattern "\t") (String.Replacement "\\t") s3
      s5 = String.replaceAll (String.Pattern "\r") (String.Replacement "\\r") s4
  in s5

-- | Escape character
escapeChar :: Char -> String
escapeChar c = case c of
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  '\\' -> "\\\\"
  _ -> SCU.singleton c

-- | Translate operator to Erlang BIF name
translateOp :: String -> String
translateOp "+" = "+"
translateOp "-" = "-"
translateOp "*" = "*"
translateOp "/" = "div"
translateOp "==" = "=:="
translateOp "/=" = "=/="
translateOp "<" = "<"
translateOp ">" = ">"
translateOp "<=" = "=<"
translateOp ">=" = ">="
translateOp "&&" = "andalso"
translateOp "||" = "orelse"
translateOp op = op

-- | Preserve original name for 1:1 mapping with source code
toSnakeCase :: String -> String
toSnakeCase s = s

-- | Generate curried apply for over-application
-- When a function is called with more args than its declared arity,
-- we call with the declared arity first, then apply the result to remaining args.
-- E.g., func/1 called with 3 args becomes: let <_Oa0> = apply 'func'/1(a) in apply (apply _Oa0(b))(c)
genCurriedApply :: CoreCtx -> String -> Array Expr -> String
genCurriedApply ctx baseCall extraArgs =
  if Array.length extraArgs == 0
  then baseCall
  else
    -- Nest the applies: apply (apply (apply _Oa0(arg1))(arg2))(arg3)
    let applyOne acc arg = "apply " <> acc <> "(" <> genExpr ctx arg <> ")"
    in "let <_Oa0> = " <> baseCall <> "\n      in " <> Array.foldl applyOne "_Oa0" extraArgs

-- | Generate Core Erlang list
-- For small lists, use flat [e1, e2, ...] syntax which is valid Core Erlang
-- For large lists or those with cons patterns, use nested cons
genCoreList :: CoreCtx -> List Expr -> String
genCoreList ctx elems =
  if List.length elems <= 50
  then "[" <> String.joinWith ", " (map (genExpr ctx) (List.toUnfoldable elems :: Array Expr)) <> "]"
  else case List.uncons elems of
    Nothing -> "[]"
    Just { head: h, tail: Nil } -> "[" <> genExpr ctx h <> "]"
    Just { head: h, tail: t } -> "[" <> genExpr ctx h <> "|" <> genCoreList ctx t <> "]"

-- | Collect arguments from curried application
collectArgs :: Expr -> { func :: Expr, args :: Array Expr }
collectArgs expr = go expr []
  where
    go (ExprApp f a) acc = go f (a : acc)
    go f acc = { func: f, args: acc }

-- | Look up function arity
lookupArity :: String -> CoreCtx -> Int
lookupArity name ctx =
  case Array.find (\f -> f.name == name) ctx.funcArities of
    Just f -> f.arity
    Nothing -> 0

-- | Extract all variable names used in an expression (for dependency analysis)
getUsedVars :: Expr -> Array String
getUsedVars (ExprVar n) = [n]
getUsedVars (ExprApp f a) = getUsedVars f <> getUsedVars a
getUsedVars (ExprLambda _ body) = getUsedVars body
getUsedVars (ExprLet binds body) = listConcatMap (\b -> getUsedVars b.value) binds <> getUsedVars body
getUsedVars (ExprIf c t e) = getUsedVars c <> getUsedVars t <> getUsedVars e
getUsedVars (ExprCase scrut clauses) = getUsedVars scrut <> listConcatMap (\cl -> getUsedVars cl.body) clauses
getUsedVars (ExprBinOp _ l r) = getUsedVars l <> getUsedVars r
getUsedVars (ExprList elems) = listConcatMap getUsedVars elems
getUsedVars (ExprRecord fields) = listConcatMap (\(Tuple _ v) -> getUsedVars v) fields
getUsedVars (ExprRecordAccess rec _) = getUsedVars rec
getUsedVars (ExprRecordUpdate rec fields) = getUsedVars rec <> listConcatMap (\(Tuple _ v) -> getUsedVars v) fields
getUsedVars (ExprDo stmts) = listConcatMapArray getUsedVarsStmt stmts
  where
    getUsedVarsStmt (DoLet binds) = listConcatMap (\b -> getUsedVars b.value) binds
    getUsedVarsStmt (DoBind _ e) = getUsedVars e
    getUsedVarsStmt (DoExpr e) = getUsedVars e
getUsedVars (ExprTuple elems) = listConcatMap getUsedVars elems
getUsedVars (ExprTyped e _) = getUsedVars e
getUsedVars (ExprParens e) = getUsedVars e
getUsedVars (ExprSection _) = []
getUsedVars (ExprSectionLeft e _) = getUsedVars e
getUsedVars (ExprSectionRight _ e) = getUsedVars e
getUsedVars (ExprQualified _ _) = []
getUsedVars (ExprUnaryOp _ e) = getUsedVars e
getUsedVars _ = []

-- | Helper: concatMap for List returning Array
listConcatMap :: forall a b. (a -> Array b) -> List a -> Array b
listConcatMap f lst = foldl (\acc x -> acc <> f x) [] lst

-- | Helper: concatMap for Array containing DoStatement
listConcatMapArray :: forall a b. (a -> Array b) -> List a -> Array b
listConcatMapArray f lst = foldl (\acc x -> acc <> f x) [] lst

-- =====================================================
-- FFI Support for BEAM
-- Maps foreign import declarations to Erlang BIFs/stdlib
-- =====================================================

-- | Count the arity of a type expression (number of function arrows)
countTypeArity :: TypeExpr -> Int
countTypeArity (TyExprArrow _ result) = 1 + countTypeArity result
countTypeArity (TyExprParens inner) = countTypeArity inner
countTypeArity (TyExprConstrained _ inner) = countTypeArity inner
countTypeArity (TyExprForAll _ inner) = countTypeArity inner
countTypeArity _ = 0

-- | Generate a foreign import function definition
-- Uses inlineImpl if provided, otherwise generates ffi_not_implemented placeholder
genForeignImport :: String -> ForeignImport -> String
genForeignImport _modName fi =
  let funcName = fi.functionName
      arity = countTypeArity fi.typeSignature
      params = if arity <= 0 then [] else map (\i -> "V" <> show i) (Array.range 0 (arity - 1))
      paramStr = String.joinWith ", " params
      -- Use inline implementation if provided, otherwise generate error
      body = case fi.inlineImpl of
        Just impl -> substituteParams impl params
        Nothing -> "'ffi_not_implemented'"
  in "'" <> funcName <> "'/" <> show arity <> " = fun (" <> paramStr <> ") -> " <> body

-- | Substitute $0, $1, etc. with actual parameter names (V0, V1, etc.)
substituteParams :: String -> Array String -> String
substituteParams impl params =
  foldl (\s i -> String.replaceAll (String.Pattern ("$" <> show i)) (String.Replacement (fromMaybe ("V" <> show i) (Array.index params i))) s)
        impl
        (Array.range 0 (Array.length params - 1))
