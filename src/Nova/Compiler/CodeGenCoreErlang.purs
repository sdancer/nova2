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
import Nova.Compiler.Ast (Module, Declaration(..), FunctionDeclaration, DataType, Expr(..), Pattern(..), Literal(..), LetBind, CaseClause, DoStatement(..), ImportItem(..), GuardedExpr, GuardClause(..))
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
  }

emptyCtx :: String -> CoreCtx
emptyCtx modName =
  { moduleName: modName
  , moduleFuncs: Set.empty
  , locals: Set.empty
  , funcArities: []
  , varCounter: 0
  , imports: Map.empty
  }

-- | Prelude functions that map to Erlang BIFs or stdlib
-- Returns Just (module, func, needsPartialApp) where needsPartialApp indicates
-- if we need to wrap it in a lambda for partial application
type PreludeFuncInfo = { mod :: String, func :: String, arity :: Int }

getPreludeFunc :: String -> Maybe PreludeFuncInfo
getPreludeFunc "show" = Just { mod: "erlang", func: "integer_to_list", arity: 1 }  -- Simple case: assume Int
getPreludeFunc "foldl" = Just { mod: "functor", func: "foldl", arity: 3 }  -- Polymorphic foldl for lists and maps
getPreludeFunc "foldr" = Just { mod: "functor", func: "foldr", arity: 3 }  -- Polymorphic foldr for lists and maps
getPreludeFunc "map" = Just { mod: "functor", func: "map", arity: 2 }  -- Polymorphic map for lists and maps
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
      bindMap = Map.fromFoldable (Array.mapMaybe (\b -> getPatternVarName b.pattern # map (\n -> Tuple n b)) binds)
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
      -- Collect all imports
      allImports = Array.concatMap getImports (Array.fromFoldable m.declarations)
      importMap = Map.fromFoldable allImports
      -- Group functions by name/arity
      grouped = groupFunctions allFuncs
      -- Extract unique name/arity pairs for exports
      uniqueFuncs = Array.nubByEq (\a b -> a.name == b.name && a.arity == b.arity)
                      (map (\g -> { name: g.name, arity: g.arity }) grouped)
      exports = String.joinWith ", " (map (\f -> atom f.name <> "/" <> show f.arity) uniqueFuncs)
      ctx = (emptyCtx modName) { moduleFuncs = Set.fromFoldable (map _.name uniqueFuncs)
                               , funcArities = uniqueFuncs
                               , imports = importMap }
      -- Generate function definitions (merging multiple clauses)
      funcDefs = String.joinWith "\n\n" (map (genFunctionGroup ctx) grouped)
      -- Generate data type comments
      dtComments = String.joinWith "\n\n" (Array.mapMaybe (genDeclNonFunc ctx) (Array.fromFoldable m.declarations))
  in "module " <> atom modName <> " [" <> exports <> "]\n" <>
     "  attributes []\n" <>
     dtComments <> (if dtComments == "" then "" else "\n\n") <>
     funcDefs <> "\nend\n"
  where
    getFunc (DeclFunction f) = Just f
    getFunc _ = Nothing
    getImports (DeclImport imp) = importItemsToTuples imp.moduleName (Array.fromFoldable imp.items)
    getImports _ = []
    genDeclNonFunc ctx (DeclDataType dt) = Just (genDataType ctx dt)
    genDeclNonFunc _ _ = Nothing

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
      paramNames = Array.range 0 (arity - 1) # map (\i -> "_P" <> show i)
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

-- | Translate module name (e.g., "Test.SumTest" -> "test_sum_test")
-- Special mappings to avoid shadowing Erlang built-in modules
translateModuleName :: String -> String
translateModuleName name =
  let baseName = String.toLower (String.replaceAll (String.Pattern ".") (String.Replacement "_") name)
  in case baseName of
    -- Avoid shadowing Erlang's built-in string module
    "string" -> "nova_string"
    -- Add other special mappings as needed
    _ -> baseName

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
  "% Data type: " <> dt.name <> "\n" <>
  "% Constructors: " <> String.joinWith ", " (Array.fromFoldable (map _.name dt.constructors))

-- | Generate pattern (wrapper that hides counter)
genPattern :: Pattern -> String
genPattern pat = (genPatternWithCounter pat 0).str

-- | Generate pattern with unique wildcard counter
-- Returns { str: pattern string, counter: updated counter }
genPatternWithCounter :: Pattern -> Int -> { str :: String, counter :: Int }
genPatternWithCounter (PatVar name) n | String.take 1 name == "_" = { str: "_W" <> show n, counter: n + 1 }  -- Treat PatVar "_" and "_x" as wildcard
genPatternWithCounter (PatVar name) n = { str: coreVar name, counter: n }
genPatternWithCounter PatWildcard n = { str: "_W" <> show n, counter: n + 1 }
genPatternWithCounter (PatLit lit) n = { str: genLiteral lit, counter: n }
genPatternWithCounter (PatCon name pats) n =
  -- Strip module qualifier from constructor name (e.g., "Ast.DeclModule" -> "DeclModule")
  let baseName = case String.lastIndexOf (String.Pattern ".") name of
        Nothing -> name
        Just idx -> String.drop (idx + 1) name
  in if List.null pats
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
  -- Handle 'otherwise' as 'true'
  if name == "otherwise"
  then "'true'"
  else
  -- Check if it's a qualified name (contains a dot like "Array.elem")
  case String.indexOf (String.Pattern ".") name of
    Just idx ->
      -- Qualified name - wrap in lambda for use as first-class value
      let modName = translateModuleName (String.take idx name)
          funcName = String.drop (idx + 1) name
      in "fun (_Qv0, _Qv1) -> call " <> atom modName <> ":" <> atom funcName <> "(_Qv0, _Qv1)"
    Nothing ->
      if Set.member name ctx.locals
      then coreVar name
      else case getPreludeFunc name of
        -- Prelude function as first-class value - wrap in lambda
        Just info ->
          let paramNames = Array.range 0 (info.arity - 1) # map (\i -> "_Pf" <> show i)
              paramsStr = String.joinWith ", " paramNames
          in "fun (" <> paramsStr <> ") -> call " <> atom info.mod <> ":" <> atom info.func <>
             "(" <> paramsStr <> ")"
        Nothing ->
          if Set.member name ctx.moduleFuncs
          then -- Module function reference
               let arity = lookupArity name ctx
               in if arity == 0
                  -- 0-arity = value, just call it to get the value
                  then "apply " <> atom name <> "/0()"
                  -- Higher arity = function, wrap in lambda for first-class use
                  else let paramNames = Array.range 0 (arity - 1) # map (\i -> "_Mf" <> show i)
                           paramsStr = String.joinWith ", " paramNames
                       in "fun (" <> paramsStr <> ") -> apply " <> atom name <> "/" <> show arity <>
                          "(" <> paramsStr <> ")"
          else case Map.lookup name ctx.imports of
               -- Imported value/function - when referenced as a value (not applied),
               -- assume it's a 0-arity value and call it directly
               Just srcMod ->
                 let modName = translateModuleName srcMod
                 in "call " <> atom modName <> ":" <> atom name <> "()"
               Nothing ->
                 if isConstructorName name
                 -- Nullary data constructor - use as atom
                 then atom (toSnakeCase name)
                 else coreVar name
  where
    isConstructorName s = case String.take 1 s of
      c -> c >= "A" && c <= "Z"

genExpr _ctx (ExprQualified modName funcName) =
  -- When used as a value (not applied), assume 0-arity call
  "call " <> atom (translateModuleName modName) <> ":" <> atom funcName <> "()"

genExpr ctx (ExprApp f arg) =
  let { func, args } = collectArgs (ExprApp f arg)
  in case func of
    ExprVar name ->
      -- Check if it's a qualified name (like "Array.elem" from backtick syntax)
      case String.indexOf (String.Pattern ".") name of
        Just idx ->
          let modName = translateModuleName (String.take idx name)
              funcName = String.drop (idx + 1) name
          in "call " <> atom modName <> ":" <> atom funcName <>
             "(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
        Nothing ->
          -- Handle special monad functions
          if name == "pure"
          then -- pure is Right for Either monad
               "{" <> atom "_right" <> ", " <> String.joinWith ", " (map (genExpr ctx) args) <> "}"
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
                               paramNames = Array.range 0 (remaining - 1) # map (\i -> "_Pc" <> show i)
                               paramsStr = String.joinWith ", " paramNames
                               allArgs = String.joinWith ", " (map (genExpr ctx) args <> paramNames)
                           in "fun (" <> paramsStr <> ") -> apply " <> atom name <> "/" <> show declaredArity <>
                              "(" <> allArgs <> ")"
              else case Map.lookup name ctx.imports of
                   -- Imported function application
                   Just srcMod ->
                     let modName = translateModuleName srcMod
                     in "call " <> atom modName <> ":" <> atom name <>
                        "(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
                   Nothing ->
                     if isConstructorName name
                     then -- Data constructor application
                          if length args == 0
                          then atom (toSnakeCase name)
                          else "{" <> atom (toSnakeCase name) <> ", " <> String.joinWith ", " (map (genExpr ctx) args) <> "}"
                     else "apply " <> coreVar name <>
                          "(" <> String.joinWith ", " (map (genExpr ctx) args) <> ")"
    ExprQualified modName funcName ->
      "call " <> atom (translateModuleName modName) <> ":" <> atom funcName <>
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

genExpr ctx (ExprBinOp op l r) =
  -- Check if it's a qualified operator like "Array.elem"
  case String.indexOf (String.Pattern ".") op of
    Just idx ->
      let modName = translateModuleName (String.take idx op)
          funcName = String.drop (idx + 1) op
      in "call " <> atom modName <> ":" <> atom funcName <> "(" <> genExpr ctx l <> ", " <> genExpr ctx r <> ")"
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
    ExprVar "_" ->
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
          paramNames = Array.range 0 (arity - 1) # map (\i -> "_L" <> show i)
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
      letrecFuncs = Array.mapMaybe (\b -> getPatternVarName b.pattern # map (\n -> { name: n, arity: getLambdaArity b.value })) funcBinds

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
      cyclicFuncs = Array.mapMaybe (\b -> getPatternVarName b.pattern # map (\n -> { name: n, arity: 0 })) cyclicValues
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
          let paramNames = Array.range 0 (group.arity - 1) # map (\i -> "_L" <> show i)
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
-- Handles both Maybe (_nothing/_just) and Either (_left/_right) monads
genDoStmts :: CoreCtx -> Array DoStatement -> String
genDoStmts ctx stmts = case Array.uncons stmts of
  Nothing -> atom "unit"
  Just { head: DoExpr e, tail: [] } -> genExpr ctx e
  Just { head: DoExpr e, tail: rest } ->
    let { var, ctx: ctx' } = freshVar ctx
    in "let <" <> var <> "> = " <> genExpr ctx e <> "\n      in " <> genDoStmts ctx' rest
  Just { head: DoBind pat e, tail: rest } ->
    let ctxWithBind = addLocalsFromPattern pat ctx
        bindExpr = genExpr ctx e
        -- Handle both Maybe and Either monads:
        -- Maybe: _nothing propagates, {_just, x} extracts x
        -- Either: {_left, err} propagates, {_right, x} extracts x
        restCode = genDoStmts ctxWithBind rest
        patCode = genPattern pat
    in "case " <> bindExpr <> " of\n" <>
       "      <'_nothing'> when 'true' ->\n        '_nothing'\n" <>
       "      <{'_left', _Err}> when 'true' ->\n        {'_left', _Err}\n" <>
       "      <{'_just', " <> patCode <> "}> when 'true' ->\n        " <> restCode <> "\n" <>
       "      <{'_right', " <> patCode <> "}> when 'true' ->\n        " <> restCode <> "\n      end"
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

-- | Convert to snake_case
toSnakeCase :: String -> String
toSnakeCase s =
  let chars = SCU.toCharArray s
  in SCU.fromCharArray (Array.concatMap convertChar chars)
  where
    convertChar c =
      if isUpper c
      then ['_', toLower c]
      else [c]
    isUpper c = c >= 'A' && c <= 'Z'
    toLower c =
      fromMaybe c (SCU.charAt 0 (String.toLower (SCU.singleton c)))

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
