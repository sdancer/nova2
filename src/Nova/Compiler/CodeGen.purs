module Nova.Compiler.CodeGen where

import Prelude
import Data.Array (intercalate, length, (:))
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Set (Set)
import Data.Set as Set
import Data.Foldable (foldr, foldl)
import Nova.Compiler.Ast (Module, Declaration(..), FunctionDeclaration, GuardedExpr, GuardClause(..), DataType, DataConstructor, TypeAlias, NewtypeDecl, InfixDecl, ForeignImport, TypeClass, TypeClassInstance, Associativity(..), Expr(..), Pattern(..), Literal(..), LetBind, CaseClause, DoStatement(..), TypeExpr(..))

-- | Code generation context
type GenCtx =
  { moduleFuncs :: Set String  -- Names of module-level functions
  , locals :: Set String       -- Local variables (params, let-bindings)
  , funcArities :: Array { name :: String, arity :: Int }  -- Module function arities
  , localArities :: Array { name :: String, arity :: Int }  -- Local function arities (from let bindings)
  }

emptyCtx :: GenCtx
emptyCtx = { moduleFuncs: Set.empty, locals: Set.empty, funcArities: [], localArities: [] }

-- | Look up arity of a local variable (lambda)
lookupLocalArity :: String -> GenCtx -> Maybe Int
lookupLocalArity name ctx =
  case Array.find (\x -> x.name == name) ctx.localArities of
    Just rec -> Just rec.arity
    Nothing -> Nothing

-- | Add local arity tracking
addLocalArity :: String -> Int -> GenCtx -> GenCtx
addLocalArity name arity ctx =
  ctx { localArities = { name, arity } : ctx.localArities }

-- | Add local variables from a pattern
addLocalsFromPattern :: Pattern -> GenCtx -> GenCtx
addLocalsFromPattern (PatVar name) ctx = ctx { locals = Set.insert name ctx.locals }
addLocalsFromPattern PatWildcard ctx = ctx
addLocalsFromPattern (PatLit _) ctx = ctx
addLocalsFromPattern (PatCon _ pats) ctx = foldr addLocalsFromPattern ctx pats
addLocalsFromPattern (PatRecord fields) ctx = foldr (\(Tuple _ p) c -> addLocalsFromPattern p c) ctx fields
addLocalsFromPattern (PatList pats) ctx = foldr addLocalsFromPattern ctx pats
addLocalsFromPattern (PatCons hd tl) ctx = addLocalsFromPattern tl (addLocalsFromPattern hd ctx)
addLocalsFromPattern (PatAs name pat) ctx = addLocalsFromPattern pat (ctx { locals = Set.insert name ctx.locals })
addLocalsFromPattern (PatParens p) ctx = addLocalsFromPattern p ctx

-- | Collect free variables (variable references) from an expression
freeVarsExpr :: Expr -> Set String
freeVarsExpr (ExprVar name) = Set.singleton name
freeVarsExpr (ExprLit _) = Set.empty
freeVarsExpr (ExprApp f arg) = Set.union (freeVarsExpr f) (freeVarsExpr arg)
freeVarsExpr (ExprLambda pats body) =
  let bound = foldr (\p s -> Set.union (patternVars p) s) Set.empty pats
  in Set.difference (freeVarsExpr body) bound
freeVarsExpr (ExprLet binds body) =
  let bound = foldr (\b s -> Set.union (patternVars b.pattern) s) Set.empty binds
      bindVars = foldr (\b s -> Set.union (freeVarsExpr b.value) s) Set.empty binds
  in Set.union (Set.difference bindVars bound) (Set.difference (freeVarsExpr body) bound)
freeVarsExpr (ExprIf c t e) = Set.union (freeVarsExpr c) (Set.union (freeVarsExpr t) (freeVarsExpr e))
freeVarsExpr (ExprCase scrut clauses) =
  let scrutVars = freeVarsExpr scrut
      clauseVars = foldr (\cl s -> Set.union (freeVarsClause cl) s) Set.empty clauses
  in Set.union scrutVars clauseVars
freeVarsExpr (ExprBinOp _ l r) = Set.union (freeVarsExpr l) (freeVarsExpr r)
freeVarsExpr (ExprUnaryOp _ e) = freeVarsExpr e
freeVarsExpr (ExprList elems) = foldr (\e s -> Set.union (freeVarsExpr e) s) Set.empty elems
freeVarsExpr (ExprTuple elems) = foldr (\e s -> Set.union (freeVarsExpr e) s) Set.empty elems
freeVarsExpr (ExprRecord fields) = foldr (\(Tuple _ e) s -> Set.union (freeVarsExpr e) s) Set.empty fields
freeVarsExpr (ExprRecordAccess e _) = freeVarsExpr e
freeVarsExpr (ExprRecordUpdate e fields) =
  foldr (\(Tuple _ v) s -> Set.union (freeVarsExpr v) s) (freeVarsExpr e) fields
freeVarsExpr (ExprParens e) = freeVarsExpr e
freeVarsExpr (ExprDo stmts) = freeVarsDo stmts Set.empty
freeVarsExpr (ExprQualified _ _) = Set.empty  -- qualified names are not free vars
freeVarsExpr (ExprTyped e _) = freeVarsExpr e
freeVarsExpr (ExprSection _) = Set.empty  -- operator section like (+ 1)
freeVarsExpr (ExprSectionLeft e _) = freeVarsExpr e  -- left section like (1 +)
freeVarsExpr (ExprSectionRight _ e) = freeVarsExpr e  -- right section like (+ 1)

-- | Helper: free vars in a case clause (body minus pattern-bound vars)
freeVarsClause :: CaseClause -> Set String
freeVarsClause cl =
  let bound = patternVars cl.pattern
      guardVars = case cl.guard of
        Nothing -> Set.empty
        Just g -> freeVarsExpr g
      bodyVars = freeVarsExpr cl.body
  in Set.difference (Set.union guardVars bodyVars) bound

-- | Helper: get all vars used in clause body and guard (including pattern vars)
-- | This is used to determine which pattern variables are actually used
usedVarsInClause :: CaseClause -> Set String
usedVarsInClause cl =
  let guardVars = case cl.guard of
        Nothing -> Set.empty
        Just g -> freeVarsExpr g
      bodyVars = freeVarsExpr cl.body
  in Set.union guardVars bodyVars

-- | Helper: free vars in do statements
freeVarsDo :: List DoStatement -> Set String -> Set String
freeVarsDo stmts bound = case stmts of
  Nil -> Set.empty
  Cons (DoExpr e) rest ->
    Set.union (Set.difference (freeVarsExpr e) bound) (freeVarsDo rest bound)
  Cons (DoBind pat e) rest ->
    let exprVars = Set.difference (freeVarsExpr e) bound
        newBound = Set.union bound (patternVars pat)
    in Set.union exprVars (freeVarsDo rest newBound)
  Cons (DoLet binds) rest ->
    let bindPatVars = foldr (\b s -> Set.union (patternVars b.pattern) s) Set.empty binds
        bindVars = foldr (\b s -> Set.union (Set.difference (freeVarsExpr b.value) bound) s) Set.empty binds
        newBound = Set.union bound bindPatVars
    in Set.union bindVars (freeVarsDo rest newBound)

-- | Get variable names bound by a pattern
patternVars :: Pattern -> Set String
patternVars (PatVar name) = Set.singleton name
patternVars PatWildcard = Set.empty
patternVars (PatLit _) = Set.empty
patternVars (PatCon _ pats) = foldr (\p s -> Set.union (patternVars p) s) Set.empty pats
patternVars (PatRecord fields) = foldr (\(Tuple _ p) s -> Set.union (patternVars p) s) Set.empty fields
patternVars (PatList pats) = foldr (\p s -> Set.union (patternVars p) s) Set.empty pats
patternVars (PatCons hd tl) = Set.union (patternVars hd) (patternVars tl)
patternVars (PatAs name pat) = Set.insert name (patternVars pat)
patternVars (PatParens p) = patternVars p

-- | Collect function names from declarations (includes data constructors)
collectModuleFuncs :: Array Declaration -> Set String
collectModuleFuncs decls = foldr go Set.empty decls
  where
    go (DeclFunction func) acc = Set.insert func.name acc
    go (DeclDataType dt) acc = foldr (\con s -> Set.insert con.name s) acc dt.constructors
    go _ acc = acc

-- | Collect function arities from declarations
collectFuncArities :: Array Declaration -> Array { name :: String, arity :: Int }
collectFuncArities decls = Array.mapMaybe go decls
  where
    go (DeclFunction func) = Just { name: func.name, arity: List.length func.parameters }
    go _ = Nothing

-- | Look up the arity of a function
lookupArity :: String -> GenCtx -> Maybe Int
lookupArity name ctx = map _.arity (Array.find (\f -> f.name == name) ctx.funcArities)

-- | Generate Elixir code from a module
genModule :: Module -> String
genModule mod =
  let decls = Array.fromFoldable mod.declarations
      ctx = { moduleFuncs: collectModuleFuncs decls
            , funcArities: collectFuncArities decls
            , locals: Set.empty
            , localArities: []
            }
  in "defmodule " <> elixirModuleName mod.name <> " do\n" <>
     intercalate "\n\n" (map (genDeclaration ctx) decls) <>
     "\nend\n"

-- | Convert module name to Elixir format
elixirModuleName :: String -> String
elixirModuleName name = String.replaceAll (String.Pattern ".") (String.Replacement ".") name

-- | Generate code for a declaration
genDeclaration :: GenCtx -> Declaration -> String
genDeclaration ctx (DeclFunction func) = genFunction ctx func
genDeclaration _ (DeclDataType dt) = genDataType dt
genDeclaration _ (DeclNewtype nt) = genNewtype nt
genDeclaration _ (DeclTypeAlias ta) = genTypeAlias ta
genDeclaration _ (DeclImport imp) =
  "  # import " <> imp.moduleName
genDeclaration _ (DeclTypeSig _) = ""  -- Type sigs are comments in Elixir
genDeclaration _ (DeclInfix inf) = genInfix inf
genDeclaration _ (DeclForeignImport fi) = genForeignImport fi
genDeclaration _ (DeclTypeClass tc) = genTypeClass tc
genDeclaration _ (DeclTypeClassInstance inst) = genTypeClassInstance inst
genDeclaration _ _ = "  # unsupported declaration"

-- | Generate foreign import - delegates to an Elixir FFI module
genForeignImport :: ForeignImport -> String
genForeignImport fi =
  let funcName = snakeCase fi.functionName
      -- Foreign imports map to Nova.FFI.<ModuleName>.<function>
      -- If moduleName is empty, use a default FFI module
      ffiModule = case fi.moduleName of
        "" -> "Nova.FFI"
        m -> "Nova.FFI." <> m
      aliasName = case fi.alias of
        Just a -> snakeCase a
        Nothing -> funcName
  in "  def " <> aliasName <> "(arg), do: " <> ffiModule <> "." <> funcName <> "(arg)"

-- | Generate function definition
genFunction :: GenCtx -> FunctionDeclaration -> String
genFunction ctx func =
  -- Handle point-free function aliases: dropNewlines = skipNewlines
  -- When body is just a variable reference to another function with known arity > 0
  case handlePointFreeAlias ctx func of
    Just code -> code
    Nothing ->
      let -- Add parameters to locals
          ctxWithParams = foldr addLocalsFromPattern ctx func.parameters
          params = intercalate ", " (Array.fromFoldable (map genPattern func.parameters))
      in if List.null func.guards
         then
           let body = genExprCtx ctxWithParams 2 func.body
           in "  def " <> snakeCase func.name <> "(" <> params <> ") do\n" <>
              body <> "\n" <>
              "  end"
         else
           -- Generate guarded function
           -- Check if first guard has pattern guards - needs special handling
           let bodyCode = genGuardedFunctionBody ctxWithParams 2 (Array.fromFoldable func.guards)
           in "  def " <> snakeCase func.name <> "(" <> params <> ") do\n" <>
              bodyCode <> "\n" <>
              "  end"

-- | Handle point-free function aliases like: dropNewlines = skipNewlines
-- | When a function has no parameters and its body is a reference to another function,
-- | generate a wrapper that passes through arguments
handlePointFreeAlias :: GenCtx -> FunctionDeclaration -> Maybe String
handlePointFreeAlias ctx func =
  if not (List.null func.parameters) then Nothing
  else case func.body of
    ExprVar refName ->
      case lookupArity refName ctx of
        Just arity | arity > 0 ->
          let argNames = map (\i -> "auto_arg" <> show i) (Array.range 0 (arity - 1))
              argsStr = intercalate ", " argNames
          in Just ("  def " <> snakeCase func.name <> "(" <> argsStr <> ") do\n    " <>
                   snakeCase refName <> "(" <> argsStr <> ")\n  end")
        _ -> Nothing
    _ ->
      -- For partial applications like Array.filter(fn), we need to generate
      -- a function that takes the remaining arg and passes both to the function
      case func.body of
        ExprApp (ExprQualified mod fn) arg ->
          if isPartialAppOfArity2 func.body
          then
            let argCode = genExprCtx ctx 0 arg
                funcName = translateQualified mod fn
            in Just ("  def " <> snakeCase func.name <> "(auto_arg0) do\n    " <>
                     funcName <> "(" <> argCode <> ", auto_arg0)\n  end")
          else Nothing
        ExprApp (ExprVar fn) arg ->
          if isPartialAppOfArity2 func.body
          then
            let argCode = genExprCtx ctx 0 arg
            in Just ("  def " <> snakeCase func.name <> "(auto_arg0) do\n    " <>
                     snakeCase fn <> "(" <> argCode <> ", auto_arg0)\n  end")
          else Nothing
        _ -> Nothing

-- | Check if expression is a partial application of a known 2-arity function
-- | like Array.filter, Array.map, etc. applied to one argument
isPartialAppOfArity2 :: Expr -> Boolean
isPartialAppOfArity2 (ExprApp (ExprQualified "Array" fn) _) =
  fn == "filter" || fn == "map" || fn == "find" || fn == "any" || fn == "all" ||
  fn == "takeWhile" || fn == "dropWhile" || fn == "sortBy" || fn == "groupBy"
isPartialAppOfArity2 (ExprApp (ExprVar fn) _) =
  fn == "filter" || fn == "map" || fn == "find" || fn == "any" || fn == "all"
isPartialAppOfArity2 _ = false

-- | Generate the body of a guarded function
-- | When the first guard has pattern guards, generate case expressions
-- | Otherwise use cond for all guards
genGuardedFunctionBody :: GenCtx -> Int -> Array GuardedExpr -> String
genGuardedFunctionBody ctx indent guards =
  case Array.uncons guards of
    Nothing -> ind indent <> "nil"  -- No guards, shouldn't happen
    Just { head: firstGuard, tail: restGuards } ->
      let patGuards = Array.filter isPatGuard (Array.fromFoldable firstGuard.guards)
          exprGuards = Array.filter (\g -> not (isPatGuard g)) (Array.fromFoldable firstGuard.guards)
      in if Array.null patGuards
         then
           -- No pattern guards in first clause, use simple cond for all
           let guardClauses = map (genGuardedExprForCond ctx (indent + 1)) guards
           in ind indent <> "cond do\n" <> intercalate "\n" guardClauses <> "\n" <> ind indent <> "end"
         else
           -- First clause has pattern guards - generate nested case
           genPatternGuardCase ctx indent firstGuard restGuards
  where
    isPatGuard (GuardPat _ _) = true
    isPatGuard _ = false

-- | Generate a pattern guard as a case expression with fallthrough to remaining guards
genPatternGuardCase :: GenCtx -> Int -> GuardedExpr -> Array GuardedExpr -> String
genPatternGuardCase ctx indent firstGuard restGuards =
  let patGuards = Array.filter isPatGuard (Array.fromFoldable firstGuard.guards)
      exprGuards = Array.filter (\g -> not (isPatGuard g)) (Array.fromFoldable firstGuard.guards)
      -- Build the case statement(s) for pattern guards
      -- For now, handle the common case of a single pattern guard
  in case patGuards of
    [GuardPat pat scrutineeExpr] ->
      let scrutinee = genExprCtx ctx 0 scrutineeExpr
          patStr = genPattern pat
          -- Add variables from pattern to context
          ctxWithPat = addLocalsFromPattern pat ctx
          bodyExpr = genExprCtx ctxWithPat 0 firstGuard.body
          -- Generate the when clause if there are expression guards
          whenClause = if Array.null exprGuards
                       then ""
                       else " when " <> genGuardClausesSimple ctxWithPat exprGuards
          -- Generate fallthrough with remaining guards
          fallthrough = if Array.null restGuards
                        then ind (indent + 2) <> "nil"
                        else genGuardedFunctionBody ctx (indent + 2) restGuards
      in ind indent <> "case " <> scrutinee <> " do\n" <>
         ind (indent + 1) <> patStr <> whenClause <> " ->\n" <>
         ind (indent + 2) <> bodyExpr <> "\n" <>
         ind (indent + 1) <> "_ ->\n" <>
         fallthrough <> "\n" <>
         ind indent <> "end"
    -- Multiple pattern guards - chain them
    _ ->
      -- Fall back to generating nested cases for multiple pattern guards
      genNestedPatternGuards ctx indent patGuards exprGuards firstGuard.body restGuards
  where
    isPatGuard (GuardPat _ _) = true
    isPatGuard _ = false

-- | Generate nested case statements for multiple pattern guards
genNestedPatternGuards :: GenCtx -> Int -> Array GuardClause -> Array GuardClause -> Expr -> Array GuardedExpr -> String
genNestedPatternGuards ctx indent patGuards exprGuards body restGuards =
  case Array.uncons patGuards of
    Nothing ->
      -- No more pattern guards, check expression guards and emit body
      if Array.null exprGuards
      then genExprCtx ctx indent body
      else
        let fallthrough = if Array.null restGuards
                          then ind (indent + 1) <> "nil"
                          else genGuardedFunctionBody ctx (indent + 1) restGuards
        in ind indent <> "if " <> genGuardClausesSimple ctx exprGuards <> " do\n" <>
           genExprCtx ctx (indent + 1) body <> "\n" <>
           ind indent <> "else\n" <>
           fallthrough <> "\n" <>
           ind indent <> "end"
    Just { head: GuardPat pat scrutineeExpr, tail: remainingPatGuards } ->
      let scrutinee = genExprCtx ctx 0 scrutineeExpr
          patStr = genPattern pat
          ctxWithPat = addLocalsFromPattern pat ctx
          innerCode = genNestedPatternGuards ctxWithPat (indent + 2) remainingPatGuards exprGuards body restGuards
          fallthrough = if Array.null restGuards
                        then ind (indent + 1) <> "_ -> nil"
                        else ind (indent + 1) <> "_ ->\n" <> genGuardedFunctionBody ctx (indent + 2) restGuards
      in ind indent <> "case " <> scrutinee <> " do\n" <>
         ind (indent + 1) <> patStr <> " ->\n" <>
         innerCode <> "\n" <>
         fallthrough <> "\n" <>
         ind indent <> "end"
    Just { head: GuardExpr _, tail: _ } ->
      -- Expression guard in pattern guard list - shouldn't happen but handle gracefully
      genGuardedFunctionBody ctx indent restGuards

-- | Generate guard clauses as a simple boolean expression (for when clauses)
genGuardClausesSimple :: GenCtx -> Array GuardClause -> String
genGuardClausesSimple ctx clauses =
  let exprs = Array.mapMaybe extractExprGuard clauses
  in intercalate " and " (map (\e -> genExprCtx ctx 0 e) exprs)
  where
    extractExprGuard (GuardExpr e) = Just e
    extractExprGuard _ = Nothing

-- | Generate a guarded expression clause for cond (no pattern guards)
genGuardedExprForCond :: GenCtx -> Int -> GuardedExpr -> String
genGuardedExprForCond ctx indent ge =
  let exprGuards = Array.fromFoldable ge.guards
      bodyExpr = genExprCtx ctx 0 ge.body
      guardExpr = genGuardClauses ctx exprGuards
  in ind indent <> guardExpr <> " ->\n" <> ind (indent + 1) <> bodyExpr

-- | Generate a guarded expression clause for cond
-- | Pattern guards need special handling - they become nested case expressions
genGuardedExpr :: GenCtx -> Int -> GuardedExpr -> String
genGuardedExpr ctx indent ge =
  let patGuards = Array.filter isPatGuard (Array.fromFoldable ge.guards)
      exprGuards = Array.filter (\g -> not (isPatGuard g)) (Array.fromFoldable ge.guards)
      bodyExpr = genExprCtx ctx 0 ge.body
      indentStr = repeatStr indent " "
  in if Array.null patGuards
     then
       -- No pattern guards, simple case
       let guardExpr = genGuardClauses ctx exprGuards
       in indentStr <> guardExpr <> " ->\n" <> indentStr <> "  " <> bodyExpr
     else
       -- Has pattern guards - generate with expression
       let withClauses = map (genWithClause ctx) patGuards
           exprGuardCode = if Array.null exprGuards then "" else ",\n" <> indentStr <> "       true <- (" <> genGuardClauses ctx exprGuards <> ")"
       in indentStr <> "true ->\n" <>
          indentStr <> "  with " <> intercalate (",\n" <> indentStr <> "       ") withClauses <> exprGuardCode <> " do\n" <>
          indentStr <> "    " <> bodyExpr <> "\n" <>
          indentStr <> "  else\n" <>
          indentStr <> "    _ -> :__guard_failed__\n" <>
          indentStr <> "  end"
  where
    isPatGuard (GuardPat _ _) = true
    isPatGuard _ = false

-- | Generate a with clause for pattern guards
genWithClause :: GenCtx -> GuardClause -> String
genWithClause ctx (GuardPat pat expr) = genPattern pat <> " <- " <> genExprCtx ctx 0 expr
genWithClause ctx (GuardExpr expr) = "true <- (" <> genExprCtx ctx 0 expr <> ")"

-- | Generate guard clauses combined with &&
genGuardClauses :: GenCtx -> Array GuardClause -> String
genGuardClauses ctx clauses =
  if Array.null clauses
  then "true"
  else intercalate " and " (map (genGuardClause ctx) clauses)

-- | Generate a single guard clause (only for expression guards, not pattern guards)
genGuardClause :: GenCtx -> GuardClause -> String
genGuardClause ctx (GuardExpr expr) = "(" <> genExprCtx ctx 0 expr <> ")"
genGuardClause ctx (GuardPat pat expr) =
  -- This shouldn't be called for pattern guards in the new flow
  "match?(" <> genPattern pat <> ", " <> genExprCtx ctx 0 expr <> ")"

-- | Repeat a string n times
repeatStr :: Int -> String -> String
repeatStr n s = if n <= 0 then "" else s <> repeatStr (n - 1) s

-- | Generate pattern code
genPattern :: Pattern -> String
genPattern (PatVar name) = snakeCase name
genPattern PatWildcard = "_"
genPattern (PatLit lit) = genLiteral lit
genPattern (PatCon name pats) =
  -- Handle qualified constructor names (e.g., Ast.PatVar -> pat_var)
  let conName = case String.lastIndexOf (String.Pattern ".") name of
        Just i -> String.drop (i + 1) name
        Nothing -> name
      -- Always use snake_case for consistency with generated data type constructors
      atomName = snakeCase conName
  in case conName of
     -- Special handling for List constructors -> native Elixir lists
     "Nil" -> "[]"
     "Cons" -> case List.fromFoldable pats of
       List.Cons h (List.Cons t List.Nil) -> "[" <> genPattern h <> " | " <> genPattern t <> "]"
       _ -> "{:cons, " <> intercalate ", " (Array.fromFoldable (map genPattern pats)) <> "}"
     _ -> if List.null pats
          then ":" <> atomName
          else "{:" <> snakeCase conName <> ", " <> intercalate ", " (Array.fromFoldable (map genPattern pats)) <> "}"
genPattern (PatRecord fields) =
  "%{" <> intercalate ", " (Array.fromFoldable (map genFieldPattern fields)) <> "}"
  where
    genFieldPattern (Tuple label pat) = snakeCase label <> ": " <> genPattern pat
genPattern (PatList pats) =
  "[" <> intercalate ", " (Array.fromFoldable (map genPattern pats)) <> "]"
genPattern (PatCons head tail) =
  "[" <> genPattern head <> " | " <> genPattern tail <> "]"
genPattern (PatAs name pat) =
  genPattern pat <> " = " <> snakeCase name
genPattern (PatParens p) = "(" <> genPattern p <> ")"

-- | Generate pattern code, prefixing unused variables with _
-- | Takes a set of used variables; any PatVar not in the set gets _ prefix
genPatternWithUsed :: Set String -> Pattern -> String
genPatternWithUsed used (PatVar name) =
  -- Special case: if name starts with _, treat as wildcard when unused
  -- This avoids generating __ which is invalid in Elixir
  if Set.member name used
  then snakeCase name
  else if String.take 1 name == "_"
       then "_"  -- Convert to true wildcard
       else "_" <> snakeCase name
genPatternWithUsed _ PatWildcard = "_"
genPatternWithUsed _ (PatLit lit) = genLiteral lit
genPatternWithUsed used (PatCon name pats) =
  let conName = case String.lastIndexOf (String.Pattern ".") name of
        Just i -> String.drop (i + 1) name
        Nothing -> name
      -- Always use snake_case for consistency with generated data type constructors
      atomName = snakeCase conName
  in case conName of
     -- Special handling for List constructors -> native Elixir lists
     "Nil" -> "[]"
     "Cons" -> case List.fromFoldable pats of
       List.Cons h (List.Cons t List.Nil) -> "[" <> genPatternWithUsed used h <> " | " <> genPatternWithUsed used t <> "]"
       _ -> "{:cons, " <> intercalate ", " (Array.fromFoldable (map (genPatternWithUsed used) pats)) <> "}"
     _ -> if List.null pats
          then ":" <> atomName
          else "{:" <> snakeCase conName <> ", " <> intercalate ", " (Array.fromFoldable (map (genPatternWithUsed used) pats)) <> "}"
genPatternWithUsed used (PatRecord fields) =
  "%{" <> intercalate ", " (Array.fromFoldable (map genFieldPattern fields)) <> "}"
  where
    genFieldPattern (Tuple label pat) = snakeCase label <> ": " <> genPatternWithUsed used pat
genPatternWithUsed used (PatList pats) =
  "[" <> intercalate ", " (Array.fromFoldable (map (genPatternWithUsed used) pats)) <> "]"
genPatternWithUsed used (PatCons head tail) =
  "[" <> genPatternWithUsed used head <> " | " <> genPatternWithUsed used tail <> "]"
genPatternWithUsed used (PatAs name pat) =
  let prefix = if Set.member name used then "" else "_"
  in genPatternWithUsed used pat <> " = " <> prefix <> snakeCase name
genPatternWithUsed used (PatParens p) = "(" <> genPatternWithUsed used p <> ")"

-- | Generate chained application for curried functions: f a b -> f.(a).(b)
genChainedApp :: String -> Array Expr -> GenCtx -> Int -> String
genChainedApp funcName args ctx indent =
  case Array.uncons args of
    Nothing -> funcName  -- No args
    Just { head: firstArg, tail: restArgs } ->
      -- Generate chained application for curried functions: f.(a).(b)
      let firstApp = funcName <> ".(" <> genExpr' ctx indent firstArg <> ")"
      in foldl (\acc arg -> acc <> ".(" <> genExpr' ctx indent arg <> ")") firstApp restArgs

-- | Collect arguments from curried application
collectArgs :: Expr -> { func :: Expr, args :: Array Expr }
collectArgs expr = go expr []
  where
    go (ExprApp f a) acc = go f (a : acc)
    go f acc = { func: f, args: acc }

-- | Check if a name is a module-level function (not a local)
isModuleFunc :: GenCtx -> String -> Boolean
isModuleFunc ctx name = Set.member name ctx.moduleFuncs && not (Set.member name ctx.locals)

-- | Generate expression code (backwards compatible)
genExpr :: Int -> Expr -> String
genExpr = genExprCtx emptyCtx

-- | Generate expression code with context
genExprCtx :: GenCtx -> Int -> Expr -> String
genExprCtx ctx indent expr = ind indent <> genExpr' ctx indent expr

-- | Check if a name is a known data constructor that should be inlined
isDataConstructor :: String -> Boolean
isDataConstructor name = Array.elem name
  [ "Tuple", "Tuple2", "Tuple3", "Tuple4", "Tuple5"
  , "Just", "Nothing"
  , "Left", "Right"
  , "Cons", "Nil"
  -- Type constructors
  , "TyVar", "TyCon", "TyRecord"
  -- Token types
  , "TokKeyword", "TokIdentifier", "TokNumber", "TokString", "TokChar"
  , "TokOperator", "TokDelimiter", "TokNewline", "TokUnrecognized"
  -- Expression constructors
  , "ExprVar", "ExprLit", "ExprApp", "ExprLambda", "ExprLet", "ExprIf"
  , "ExprCase", "ExprBinOp", "ExprList", "ExprRecord", "ExprRecordAccess"
  , "ExprParens", "ExprDo", "ExprQualified", "ExprRecordUpdate", "ExprTyped"
  , "ExprUnaryOp", "ExprTuple", "ExprSection", "ExprSectionLeft", "ExprSectionRight"
  , "ExprNegate"
  -- Pattern constructors
  , "PatVar", "PatWildcard", "PatLit", "PatCon", "PatRecord", "PatList"
  , "PatCons", "PatAs", "PatParens", "PatTyped"
  -- Literal constructors
  , "LitInt", "LitString", "LitChar", "LitBool", "LitNumber"
  -- Declaration constructors
  , "DeclFunction", "DeclTypeSig", "DeclDataType", "DeclTypeAlias"
  , "DeclModule", "DeclImport", "DeclTypeClass", "DeclTypeClassInstance"
  , "DeclInfix", "DeclForeignImport", "DeclType"
  -- Type expression constructors
  , "TyExprCon", "TyExprVar", "TyExprApp", "TyExprArrow", "TyExprRecord"
  , "TyExprForAll", "TyExprTuple", "TyExprConstrained", "TyExprParens"
  -- Unify error constructors
  , "OccursCheck", "TypeMismatch", "ArityMismatch", "RecordFieldMismatch"
  -- Type checker error constructors
  , "UnifyErr", "UnboundVariable", "NotImplemented"
  -- Do statement constructors
  , "DoLet", "DoBind", "DoExpr"
  -- Guard clause constructors
  , "GuardExpr", "GuardPat"
  -- Newtype constructors (single-arg wrappers)
  , "Parser"
  ]

-- | Get the arity of an AST constructor
getAstConstructorArity :: String -> Int
getAstConstructorArity name =
  -- Nullary constructors
  if name == "PatWildcard" || name == "ImportAll" || name == "ImportNone"
  then 0
  -- 3-arg constructor
  else if name == "ExprIf"
  then 3
  -- 2-arg constructors
  else if isArity2Constructor name
  then 2
  -- Default: 1-arg
  else 1

-- | 2-argument AST constructors
isArity2Constructor :: String -> Boolean
isArity2Constructor name = Array.elem name arity2Constructors

arity2Constructors :: Array String
arity2Constructors =
  [ "ExprApp", "ExprCase", "ExprBinOp", "ExprRecordAccess", "ExprRecordUpdate"
  , "ExprQualified", "ExprTyped", "ExprUnaryOp", "ExprLambda", "ExprLet"
  , "PatCon", "PatCons", "PatAs"
  , "TyExprApp", "TyExprArrow", "TyExprRecord", "TyExprForAll", "TyExprConstrained"
  , "DoBind", "GuardPat", "ImportType"
  ]

-- | Check if a data constructor belongs to Nova.Compiler.Ast module
-- | These constructors need to be qualified when used in other modules
isAstConstructor :: String -> Boolean
isAstConstructor name = Array.elem name
  [ -- Expression constructors
    "ExprVar", "ExprLit", "ExprApp", "ExprLambda", "ExprLet", "ExprIf"
  , "ExprCase", "ExprBinOp", "ExprList", "ExprRecord", "ExprRecordAccess"
  , "ExprParens", "ExprDo", "ExprQualified", "ExprRecordUpdate", "ExprTyped"
  , "ExprUnaryOp", "ExprTuple", "ExprSection", "ExprSectionLeft", "ExprSectionRight"
  , "ExprNegate"
  -- Pattern constructors
  , "PatVar", "PatWildcard", "PatLit", "PatCon", "PatRecord", "PatList"
  , "PatCons", "PatAs", "PatParens", "PatTyped"
  -- Literal constructors
  , "LitInt", "LitString", "LitChar", "LitBool", "LitNumber"
  -- Declaration constructors
  , "DeclFunction", "DeclTypeSig", "DeclDataType", "DeclTypeAlias"
  , "DeclModule", "DeclImport", "DeclTypeClass", "DeclTypeClassInstance"
  , "DeclInfix", "DeclForeignImport", "DeclType"
  -- Type expression constructors
  , "TyExprCon", "TyExprVar", "TyExprApp", "TyExprArrow", "TyExprRecord"
  , "TyExprForAll", "TyExprTuple", "TyExprConstrained", "TyExprParens"
  -- Do statement constructors
  , "DoLet", "DoBind", "DoExpr"
  -- Guard clause constructors
  , "GuardExpr", "GuardPat"
  -- Token types (from Tokenizer but used in Ast)
  , "TokKeyword", "TokIdentifier", "TokNumber", "TokString", "TokChar"
  , "TokOperator", "TokDelimiter", "TokNewline", "TokUnrecognized"
  ]

-- | Check if a name is a prelude function that maps to Nova.Runtime
isPreludeFunc :: String -> Boolean
isPreludeFunc name = Array.elem name
  [ "show", "map", "foldl", "foldr", "foldM", "filter"
  , "intercalate", "identity", "const", "compose"
  , "pure", "otherwise", "length", "zip", "tuple"
  , "just", "nothing", "left", "right"
  , "fromMaybe", "maybe", "either", "isJust", "isNothing"
  , "fst", "snd"  -- Tuple accessors
  ]

-- | Check if a name is a nullary (zero-argument) data constructor
-- | These should be represented as atoms in Elixir
isNullaryConstructor :: String -> Boolean
isNullaryConstructor name = Array.elem name
  [ -- Token types (nullary constructors)
    "TokKeyword", "TokIdentifier", "TokNumber", "TokString", "TokChar"
  , "TokOperator", "TokDelimiter", "TokNewline", "TokUnrecognized"
  -- Pattern constructors that are nullary
  , "PatWildcard"
  -- Import specs
  , "ImportAll", "ImportNone"
  -- Declaration kinds
  , "KindFunction", "KindDataType", "KindTypeAlias", "KindTypeClass"
  , "KindInstance", "KindForeignImport"
  -- Associativity
  , "AssocLeft", "AssocRight", "AssocNone"
  -- Layout delimiters (from CstLayout)
  , "LytRoot", "LytTopDecl", "LytTopDeclHead", "LytDeclGuard"
  , "LytCase", "LytCaseBinders", "LytCaseGuard", "LytLambdaBinders"
  , "LytParen", "LytBrace", "LytSquare", "LytIf", "LytThen"
  , "LytProperty", "LytForall", "LytTick", "LytLet", "LytLetStmt"
  , "LytWhere", "LytOf", "LytDo", "LytAdo"
  ]

-- | Known 2-argument functions that may be partially applied
-- | When called with 1 arg, wrap the call in a closure
isBinaryFunc :: String -> Boolean
isBinaryFunc name = Array.elem name
  [ "applySubst", "lookupSubst", "singleSubst", "composeSubst"
  , "extendEnv", "lookupEnv", "unify", "bindVar"
  ]

-- | Functions that are commonly imported from Nova.Compiler.Types
-- | These need to be qualified in generated Elixir
isTypesModuleFunc :: String -> Boolean
isTypesModuleFunc name = Array.elem name
  [ "emptySubst", "singleSubst", "composeSubst", "applySubst"
  , "freeTypeVars", "freeTypeVarsScheme", "freeTypeVarsEnv"
  , "lookupSubst", "extendEnv", "lookupEnv", "applySubstToEnv"
  , "freshVar", "generalize", "instantiate", "mkScheme"
  , "mkTVar", "mkTCon", "mkTCon0", "tyVar", "tyCon", "tyRecord"
  , "tInt", "tString", "tBool", "tChar", "tArray", "tArrow"
  , "tMaybe", "tEither", "tTuple", "tMap", "tSet", "tList", "tNumber"
  , "emptyEnv", "builtinPrelude"
  -- Export/import functions
  , "emptyExports", "mergeTypeExport", "mergeExportsToEnv", "registerModule", "lookupModule"
  ]

-- | Values (zero-arity) from Types module that should be called with ()
isTypesModuleValue :: String -> Boolean
isTypesModuleValue name = Array.elem name
  [ "emptySubst", "emptyEnv", "emptyExports"
  , "tInt", "tString", "tBool", "tChar", "tNumber"  -- Primitive types are values
  ]

-- | Get the arity of a Types module function
typesModuleFuncArity :: String -> Int
typesModuleFuncArity name =
  if isTypesModuleValue name then 0
  else if Array.elem name typesArity1Funcs then 1
  else if Array.elem name typesArity2Funcs then 2
  else 1  -- Default to 1

typesArity1Funcs :: Array String
typesArity1Funcs = ["singleSubst", "mkTVar", "mkTCon0", "tArray", "tMaybe", "tSet", "tList", "tTuple"]

typesArity2Funcs :: Array String
typesArity2Funcs = ["composeSubst", "applySubst", "mkTCon", "tArrow", "tEither", "tMap", "extendEnv", "lookupEnv", "freshVar", "generalize", "instantiate"]

-- | Functions from Nova.Compiler.Unify module
isUnifyModuleFunc :: String -> Boolean
isUnifyModuleFunc name = Array.elem name
  [ "unify", "unifyMany", "bindVar", "occurs", "unifyRecords", "unifyField"
  ]

-- | Get the arity of an Unify module function
unifyModuleFuncArity :: String -> Int
unifyModuleFuncArity name = case name of
  "unify" -> 2
  "unifyMany" -> 2
  "bindVar" -> 2
  "occurs" -> 2
  "unifyRecords" -> 2
  "unifyField" -> 4
  _ -> 2

-- | Translate qualified module calls to Nova.* modules
translateQualified :: String -> String -> String
translateQualified mod name =
  -- Special case translations for specific module.function combinations
  case Tuple mod name of
    Tuple "Int" "fromString" -> "Nova.String.to_int"
    Tuple "Data.Int" "fromString" -> "Nova.String.to_int"
    Tuple "Number" "fromString" -> "Nova.String.to_float"
    Tuple "Data.Number" "fromString" -> "Nova.String.to_float"
    _ ->
      let elixirMod = case mod of
            "Map" -> "Nova.Map"
            "Data.Map" -> "Nova.Map"
            "Set" -> "Nova.Set"
            "Data.Set" -> "Nova.Set"
            "Array" -> "Nova.Array"
            "Data.Array" -> "Nova.Array"
            "List" -> "Nova.List"
            "Data.List" -> "Nova.List"
            "String" -> "Nova.String"
            "Data.String" -> "Nova.String"
            "Data.String.CodeUnits" -> "Nova.String"
            "SCU" -> "Nova.String"
            "CU" -> "Nova.String"
            -- Nova compiler modules
            "Ast" -> "Nova.Compiler.Ast"
            "Nova.Compiler.Ast" -> "Nova.Compiler.Ast"
            "Types" -> "Nova.Compiler.Types"
            "Nova.Compiler.Types" -> "Nova.Compiler.Types"
            "Unify" -> "Nova.Compiler.Unify"
            "Nova.Compiler.Unify" -> "Nova.Compiler.Unify"
            "Tokenizer" -> "Nova.Compiler.Tokenizer"
            "Nova.Compiler.Tokenizer" -> "Nova.Compiler.Tokenizer"
            "Parser" -> "Nova.Compiler.Parser"
            "Nova.Compiler.Parser" -> "Nova.Compiler.Parser"
            "TypeChecker" -> "Nova.Compiler.TypeChecker"
            "Nova.Compiler.TypeChecker" -> "Nova.Compiler.TypeChecker"
            "CodeGen" -> "Nova.Compiler.CodeGen"
            "Nova.Compiler.CodeGen" -> "Nova.Compiler.CodeGen"
            _ -> elixirModuleName mod
      in elixirMod <> "." <> snakeCase name

-- | Generate a partial application wrapper
-- | When a function with `arity` args is called with `numArgs` args,
-- | generate a curried closure for the remaining args
genPartialApp :: String -> String -> Int -> Int -> String
genPartialApp funcName appliedArgs numArgs arity =
  let remaining = arity - numArgs
      -- Generate parameter names for remaining args
      extraParams = map (\i -> "auto_p" <> show i) (Array.range 0 (remaining - 1))
      -- Generate curried lambda headers: fn auto_p0 -> fn auto_p1 -> ...
      curriedHeader = intercalate " " (map (\p -> "fn " <> p <> " ->") extraParams)
      curriedEnds = intercalate "" (map (\_ -> " end") extraParams)
      allArgsStr = if numArgs == 0
                   then intercalate ", " extraParams
                   else appliedArgs <> ", " <> intercalate ", " extraParams
  in curriedHeader <> " " <> funcName <> "(" <> allArgsStr <> ")" <> curriedEnds

-- | Generate a data constructor application with proper arity
genConstructorApp :: GenCtx -> Int -> String -> Array Expr -> String
genConstructorApp ctx indent name args =
  let genArgs = map (genExpr' ctx indent) args
      -- For Ast constructors, use qualified function call
      astPrefix = if isAstConstructor name then "Nova.Compiler.Ast." else ""
      -- Default: generate tagged tuple {:name, arg1, arg2, ...}
      defaultGen = if isAstConstructor name
                   then astPrefix <> snakeCase name <> "(" <> intercalate ", " genArgs <> ")"
                   else "{:" <> snakeCase name <> ", " <> intercalate ", " genArgs <> "}"
      defaultGenNoArgs = if isAstConstructor name
                         then astPrefix <> snakeCase name <> "()"
                         else ":" <> snakeCase name
  in case name of
    -- Tuple constructors - generate native Elixir tuples tagged with :tuple
    "Tuple" -> case genArgs of
      [a, b] -> "{:tuple, " <> a <> ", " <> b <> "}"
      _ -> "{:tuple, " <> intercalate ", " genArgs <> "}"
    "Tuple2" -> "{:tuple, " <> intercalate ", " genArgs <> "}"
    "Tuple3" -> "{:tuple3, " <> intercalate ", " genArgs <> "}"
    "Tuple4" -> "{:tuple4, " <> intercalate ", " genArgs <> "}"
    "Tuple5" -> "{:tuple5, " <> intercalate ", " genArgs <> "}"
    -- Maybe constructors
    "Just" -> "{:just, " <> intercalate ", " genArgs <> "}"
    "Nothing" -> ":nothing"
    -- Either constructors
    "Left" -> "{:left, " <> intercalate ", " genArgs <> "}"
    "Right" -> "{:right, " <> intercalate ", " genArgs <> "}"
    -- List constructors
    "Cons" -> case genArgs of
      [h, t] -> "[" <> h <> " | " <> t <> "]"
      _ -> "Nova.Runtime.cons(" <> intercalate ", " genArgs <> ")"
    "Nil" -> "[]"
    -- All other data constructors: use module prefix for Ast types
    _ -> if Array.null genArgs then defaultGenNoArgs else defaultGen

genExpr' :: GenCtx -> Int -> Expr -> String
genExpr' ctx _ (ExprVar name) =
  -- Handle special constants and functions
  -- First check if it's a local variable - locals take precedence
  if Set.member name ctx.locals
  then snakeCase name
  else case name of
    "Nothing" -> ":nothing"
    "nothing" -> ":nothing"
    "Nil" -> "[]"
    "otherwise" -> "true"
    "True" -> "true"
    "False" -> "false"
    "not" -> "(&Kernel.not/1)"  -- PureScript's not is a function
    "mod" -> "(&rem/2)"  -- PureScript's mod is Elixir's rem (only when not local)
    "__guarded__" -> ":__guarded__"  -- Placeholder for guarded where functions
    _ ->
      -- Handle qualified names (e.g., Array.elem from backtick syntax)
      if String.contains (String.Pattern ".") name
      then
        let parts = String.split (String.Pattern ".") name
            len = Array.length parts
        in if len > 1
           then let modParts = Array.take (len - 1) parts
                    funcName = case Array.last parts of
                      Just n -> n
                      Nothing -> name
                in "(&" <> translateQualified (intercalate "." modParts) funcName <> "/2)"
           else snakeCase name
      -- Handle nullary data constructors as atoms (e.g., LytRoot -> :lyt_root)
      else if isNullaryConstructor name
      then ":" <> snakeCase name
      -- If it's a module function used as a value (not in call position),
      -- generate a function reference &func/arity
      else if isModuleFunc ctx name
      then case lookupArity name ctx of
             Just 0 -> snakeCase name <> "()"  -- Zero-arity: call it
             Just arity -> "(&" <> snakeCase name <> "/" <> show arity <> ")"  -- Generate function reference
             Nothing -> "(&" <> snakeCase name <> "/1)"  -- Default to arity 1
      else if isTypesModuleFunc name
      then let arity = typesModuleFuncArity name
           in if arity == 0
              then "Nova.Compiler.Types." <> snakeCase name <> "()"
              else "(&Nova.Compiler.Types." <> snakeCase name <> "/" <> show arity <> ")"
      else if isUnifyModuleFunc name
      then "(&Nova.Compiler.Unify." <> snakeCase name <> "/" <> show (unifyModuleFuncArity name) <> ")"
      else if isPreludeFunc name
      then "(&Nova.Runtime." <> snakeCase name <> "/1)"
      -- Handle AST constructors used as functions (e.g., PatVar, ExprVar)
      else if isAstConstructor name
      then "(&Nova.Compiler.Ast." <> snakeCase name <> "/1)"
      -- Handle other data constructors as function references
      else if isDataConstructor name && not (isNullaryConstructor name)
      then "(&" <> snakeCase name <> "/1)"
      else snakeCase name
genExpr' _ _ (ExprQualified mod name) =
  -- Check if this is an AST constructor used as a function value
  -- These need to be wrapped in lambdas because Elixir doesn't support
  -- passing module functions without explicit capture
  if isAstConstructor name
  then
    -- Generate lambda wrapper for AST constructors
    -- Most AST constructors take 1-2 arguments
    let arity = getAstConstructorArity name
    in if arity == 0
       then translateQualified mod name <> "()"
       else if arity == 1
       then "fn a -> " <> translateQualified mod name <> "(a) end"
       else "fn a, b -> " <> translateQualified mod name <> "(a, b) end"
  else if isTypesModuleFunc name
  then
    let arity = typesModuleFuncArity name
    in if arity == 0
       then "Nova.Compiler.Types." <> snakeCase name <> "()"
       else "(&Nova.Compiler.Types." <> snakeCase name <> "/" <> show arity <> ")"
  else translateQualified mod name
genExpr' _ _ (ExprLit lit) = genLiteral lit

genExpr' ctx indent (ExprApp f arg) =
  -- Collect all arguments for curried application
  let { func, args } = collectArgs (ExprApp f arg)
      genArg a = genExpr' ctx indent a
      argsStr = intercalate ", " (map genArg args)
  in case func of
    ExprVar name -> genVarApp ctx indent name args argsStr
    ExprQualified m n -> translateQualified m n <> "(" <> argsStr <> ")"
    ExprLambda _ _ -> "(" <> genExpr' ctx indent func <> ").(" <> argsStr <> ")"
    _ -> "(" <> genExpr' ctx indent func <> ").(" <> argsStr <> ")"
  where
    -- Handle function application where function is a variable
    genVarApp :: GenCtx -> Int -> String -> Array Expr -> String -> String
    genVarApp c i n exprs argsS =
      -- Handle special built-in functions first
      if n == "not" then "not(" <> argsS <> ")"
      else if n == "mod" then "rem(" <> argsS <> ")"
      -- Handle qualified names (e.g., Array.elem from backtick syntax)
      else if String.contains (String.Pattern ".") n
      then
        -- Inline qualified app logic
        let parts = String.split (String.Pattern ".") n
            len = Array.length parts
        in if len > 1
           then let modParts = Array.take (len - 1) parts
                    funcName = case Array.last parts of
                      Just fn -> fn
                      Nothing -> n
                in translateQualified (intercalate "." modParts) funcName <> "(" <> argsS <> ")"
           else snakeCase n <> ".(" <> argsS <> ")"
      -- Handle data constructors specially
      else if isDataConstructor n
      then genConstructorApp c i n exprs
      -- Handle module-level functions (possibly with partial application)
      else if isModuleFunc c n
      then case lookupArity n c of
             Just arity ->
               let numArgs = length exprs
               in if numArgs == arity
                  then snakeCase n <> "(" <> argsS <> ")"
                  else if numArgs < arity
                  then genPartialApp (snakeCase n) argsS numArgs arity
                  else snakeCase n <> "(" <> argsS <> ")"  -- More args than expected
             Nothing ->
               -- Unknown arity, just generate the call
               snakeCase n <> "(" <> argsS <> ")"
      -- Handle external Types module functions
      else if isTypesModuleFunc n
      then let arity = typesModuleFuncArity n
               numArgs = length exprs
           in if numArgs == arity
              then "Nova.Compiler.Types." <> snakeCase n <> "(" <> argsS <> ")"
              else if numArgs < arity
              then genPartialApp ("Nova.Compiler.Types." <> snakeCase n) argsS numArgs arity
              else "Nova.Compiler.Types." <> snakeCase n <> "(" <> argsS <> ")"
      else if isUnifyModuleFunc n
      then let arity = unifyModuleFuncArity n
               numArgs = length exprs
           in if numArgs == arity
              then "Nova.Compiler.Unify." <> snakeCase n <> "(" <> argsS <> ")"
              else if numArgs < arity
              then genPartialApp ("Nova.Compiler.Unify." <> snakeCase n) argsS numArgs arity
              else "Nova.Compiler.Unify." <> snakeCase n <> "(" <> argsS <> ")"  -- More args than expected
      else if isPreludeFunc n
      then "Nova.Runtime." <> snakeCase n <> "(" <> argsS <> ")"
      else
        -- For local variables (lambdas), use chained application for curried calls
        -- f a b -> f.(a).(b)
        genChainedApp (snakeCase n) exprs c i

genExpr' ctx indent (ExprLambda pats body) =
  -- Generate curried lambda: \a b -> x becomes fn a -> fn b -> x end end
  -- This is necessary for proper partial application in functional code
  let ctxWithParams = foldr addLocalsFromPattern ctx pats
  in genCurriedLambda ctxWithParams indent pats body
  where
    genCurriedLambda :: GenCtx -> Int -> List Pattern -> Expr -> String
    genCurriedLambda c i ps b = case List.uncons ps of
      Nothing -> genExpr' c i b  -- No params, just body
      Just { head: p, tail: rest } ->
        if List.null rest
        then "fn " <> genPattern p <> " -> " <> genExpr' c i b <> " end"
        else "fn " <> genPattern p <> " -> " <> genCurriedLambda c i rest b <> " end"

genExpr' ctx indent (ExprLet binds body) =
  let ctxWithBinds = foldr (\b c -> addLocalsFromPattern b.pattern c) ctx binds
      -- First group consecutive bindings by name (for multi-clause functions in where)
      groupedBinds = groupBindsByName (Array.fromFoldable binds)
      -- Then sort groups by dependencies
      sortedGroups = sortGroupsByDependencies groupedBinds
      -- Generate code for each group
      bindCode = intercalate "\n" (map (genBindingGroup ctx (indent + 1)) sortedGroups)
  in "\n" <> bindCode <> "\n" <> ind (indent + 1) <> genExpr' ctxWithBinds 0 body

genExpr' ctx indent (ExprIf cond then_ else_) =
  "if " <> genExpr' ctx indent cond <> " do\n" <>
  genExprCtx ctx (indent + 1) then_ <> "\n" <>
  ind indent <> "else\n" <>
  genExprCtx ctx (indent + 1) else_ <> "\n" <>
  ind indent <> "end"

genExpr' ctx indent (ExprCase scrutinee clauses) =
  -- Group consecutive wildcard-guarded clauses together for cond expression
  let groupedClauses = groupWildcardGuardedClauses (Array.fromFoldable clauses)
  in "case " <> genExpr' ctx indent scrutinee <> " do\n" <>
     intercalate "\n" (map (genCaseClauseGroup ctx (indent + 1)) groupedClauses) <> "\n" <>
     ind indent <> "end"

genExpr' ctx indent (ExprDo stmts) =
  -- Do notation becomes a series of binds/flatMaps
  genDoStmtsCtx ctx indent (Array.fromFoldable stmts)

genExpr' ctx _ (ExprBinOp ":" l r) =
  -- Cons operator needs special list syntax in Elixir
  "[" <> genExpr' ctx 0 l <> " | " <> genExpr' ctx 0 r <> "]"

genExpr' ctx _ (ExprBinOp "<>" l r) =
  -- Use runtime append for polymorphic semigroup (works for both strings and arrays)
  "Nova.Runtime.append(" <> genExpr' ctx 0 l <> ", " <> genExpr' ctx 0 r <> ")"

genExpr' ctx _ (ExprBinOp "<<<" l r) =
  -- Function composition (right-to-left): f <<< g = fn x -> f.(g.(x)) end
  "fn auto_c -> (" <> genExpr' ctx 0 l <> ").((" <> genExpr' ctx 0 r <> ").(auto_c)) end"

genExpr' ctx _ (ExprBinOp ">>>" l r) =
  -- Function composition (left-to-right): f >>> g = fn x -> g.(f.(x)) end
  "fn auto_c -> (" <> genExpr' ctx 0 r <> ").((" <> genExpr' ctx 0 l <> ").(auto_c)) end"

genExpr' ctx _ (ExprBinOp "$" l r) =
  -- $ is function application: f $ x = f(x)
  "(" <> genExpr' ctx 0 l <> ").(" <> genExpr' ctx 0 r <> ")"

genExpr' ctx _ (ExprBinOp "#" l r) =
  -- # is reverse application: x # f = f(x)
  "(" <> genExpr' ctx 0 r <> ").(" <> genExpr' ctx 0 l <> ")"

genExpr' ctx _ (ExprBinOp "<$>" l r) =
  -- Functor map: f <$> x = map(f, x)
  "Nova.Runtime.fmap(" <> genExpr' ctx 0 l <> ", " <> genExpr' ctx 0 r <> ")"

genExpr' ctx _ (ExprBinOp "<|>" l r) =
  -- Alternative: x <|> y = alt(x, y) - for Maybe, returns first Just or Nothing
  "Nova.Runtime.alt(" <> genExpr' ctx 0 l <> ", " <> genExpr' ctx 0 r <> ")"

genExpr' ctx _ (ExprBinOp "*>" l r) =
  -- Applicative sequence: x *> y = discard x then y
  "Nova.Runtime.seq(" <> genExpr' ctx 0 l <> ", " <> genExpr' ctx 0 r <> ")"

genExpr' ctx _ (ExprBinOp "<*" l r) =
  -- Applicative sequence left: x <* y = x then discard y
  "Nova.Runtime.seq_left(" <> genExpr' ctx 0 l <> ", " <> genExpr' ctx 0 r <> ")"

genExpr' ctx _ (ExprBinOp op l r) =
  -- Check for section syntax: (_ == x) or (x == _)
  -- Handle both ExprVar "_" (old parser) and ExprSection "_" (CST parser)
  let isUnderscore e = case e of
        ExprVar "_" -> true
        ExprSection "_" -> true
        _ -> false
  in case l of
    _ | isUnderscore l ->
      -- Left section: (_ op r) -> fn __x__ -> __x__ op r end
      "fn __x__ -> (__x__ " <> genBinOp op <> " " <> genExpr' ctx 0 r <> ") end"
    _ -> case r of
      _ | isUnderscore r ->
        -- Right section: (l op _) -> fn __x__ -> l op __x__ end
        "fn __x__ -> (" <> genExpr' ctx 0 l <> " " <> genBinOp op <> " __x__) end"
      _ ->
        -- Normal binary operation
        -- Check if this is a backtick function call (contains . or starts with uppercase)
        if String.contains (String.Pattern ".") op || isUpperCase (String.take 1 op)
        then
          -- Backtick syntax: x `f` y -> f(x, y)
          -- For qualified names like Array.elem, translate properly
          let funcCall = if String.contains (String.Pattern ".") op
                         then let parts = String.split (String.Pattern ".") op
                                  len = Array.length parts
                              in if len > 1
                                 then let modParts = Array.take (len - 1) parts
                                          funcName = case Array.last parts of
                                            Just n -> n
                                            Nothing -> op
                                      in translateQualified (intercalate "." modParts) funcName
                                 else snakeCase op
                         else snakeCase op
          in funcCall <> "(" <> genExpr' ctx 0 l <> ", " <> genExpr' ctx 0 r <> ")"
        else
          "(" <> genExpr' ctx 0 l <> " " <> genBinOp op <> " " <> genExpr' ctx 0 r <> ")"
  where
    isUpperCase s = case SCU.charAt 0 s of
      Just c -> c >= 'A' && c <= 'Z'
      Nothing -> false

genExpr' ctx _ (ExprUnaryOp op e) =
  genUnaryOp op <> genExpr' ctx 0 e

genExpr' ctx _ (ExprList elems) =
  "[" <> intercalate ", " (Array.fromFoldable (map (genExpr' ctx 0) elems)) <> "]"

genExpr' ctx _ (ExprTuple elems) =
  "{" <> intercalate ", " (Array.fromFoldable (map (genExpr' ctx 0) elems)) <> "}"

genExpr' ctx _ (ExprRecord fields) =
  "%{" <> intercalate ", " (Array.fromFoldable (map genRecordField fields)) <> "}"
  where
    genRecordField (Tuple label expr) = snakeCase label <> ": " <> genExpr' ctx 0 expr

genExpr' ctx _ (ExprRecordAccess rec field) =
  -- Handle _.field pattern -> & &1.field
  case rec of
    ExprVar "_" -> "& &1." <> snakeCase field
    ExprSection "_" -> "& &1." <> snakeCase field  -- Also handle ExprSection from CST parsing
    ExprRecordAccess inner innerField ->
      -- Check if nested: _.foo.bar -> & &1.foo.bar
      case collectRecordAccessChain rec of
        { base: ExprVar "_", fields } ->
          "& &1." <> intercalate "." (map snakeCase fields) <> "." <> snakeCase field
        { base: ExprSection "_", fields } ->
          "& &1." <> intercalate "." (map snakeCase fields) <> "." <> snakeCase field
        _ -> genExpr' ctx 0 rec <> "." <> snakeCase field
    _ -> genExpr' ctx 0 rec <> "." <> snakeCase field
  where
    collectRecordAccessChain :: Expr -> { base :: Expr, fields :: Array String }
    collectRecordAccessChain (ExprRecordAccess inner f) =
      let result = collectRecordAccessChain inner
      in result { fields = Array.snoc result.fields f }
    collectRecordAccessChain e = { base: e, fields: [] }

genExpr' ctx _ (ExprRecordUpdate rec fields) =
  "%{" <> genExpr' ctx 0 rec <> " | " <>
  intercalate ", " (Array.fromFoldable (map genUpdateField fields)) <> "}"
  where
    genUpdateField (Tuple label expr) = snakeCase label <> ": " <> genExpr' ctx 0 expr

genExpr' ctx indent (ExprTyped e _) = genExpr' ctx indent e
genExpr' ctx indent (ExprParens e) = "(" <> genExpr' ctx indent e <> ")"
genExpr' _ _ (ExprSection op) =
  -- Check if it's a record accessor (.field) vs a binary operator section
  case String.stripPrefix (String.Pattern ".") op of
    Just field -> "& &1." <> snakeCase field  -- Record accessor: .id -> & &1.id
    Nothing -> "fn __x__, __y__ -> (__x__ " <> genBinOp op <> " __y__) end"  -- Binary operator section: (+) -> fn x, y -> x + y

-- Left section: (1 +) => fn x -> 1 + x  (using infix syntax)
genExpr' ctx _ (ExprSectionLeft expr op) =
  "fn __x__ -> (" <> genExpr' ctx 0 expr <> " " <> genBinOp op <> " __x__) end"

-- Right section: (+ 1) => fn x -> x + 1  (using infix syntax)
genExpr' ctx _ (ExprSectionRight op expr) =
  "fn __x__ -> (__x__ " <> genBinOp op <> " " <> genExpr' ctx 0 expr <> ") end"

-- | Generate literal
genLiteral :: Literal -> String
genLiteral (LitInt n) = show n
genLiteral (LitNumber n) = show n
genLiteral (LitString s) = "\"" <> escapeString s <> "\""
genLiteral (LitChar c) =
  -- Escape special characters in Elixir char literals
  case c of
    '\n' -> "?\\n"
    '\r' -> "?\\r"
    '\t' -> "?\\t"
    '\\' -> "?\\\\"
    ' ' -> "?\\s"
    _ -> "?" <> SCU.singleton c
genLiteral (LitBool true) = "true"
genLiteral (LitBool false) = "false"

-- | Check if an expression contains a reference to a variable name
containsVar :: String -> Expr -> Boolean
containsVar name (ExprVar v) = v == name
containsVar name (ExprApp f a) = containsVar name f || containsVar name a
containsVar name (ExprLambda _ body) = containsVar name body
containsVar name (ExprLet binds body) =
  List.any (\b -> containsVar name b.value) binds || containsVar name body
containsVar name (ExprIf c t e) = containsVar name c || containsVar name t || containsVar name e
containsVar name (ExprCase scrut clauses) =
  containsVar name scrut || List.any (\cl -> containsVar name cl.body || containsVarInMaybeExpr name cl.guard) clauses
  where
    containsVarInMaybeExpr :: String -> Maybe Expr -> Boolean
    containsVarInMaybeExpr n Nothing = false
    containsVarInMaybeExpr n (Just e) = containsVar n e
containsVar name (ExprDo stmts) = List.any (containsVarInDoStmt name) stmts
containsVar name (ExprBinOp _ l r) = containsVar name l || containsVar name r
containsVar name (ExprUnaryOp _ e) = containsVar name e
containsVar name (ExprList es) = List.any (containsVar name) es
containsVar name (ExprTuple es) = List.any (containsVar name) es
containsVar name (ExprRecord fs) = List.any (\(Tuple _ e) -> containsVar name e) fs
containsVar name (ExprRecordAccess e _) = containsVar name e
containsVar name (ExprRecordUpdate e fs) = containsVar name e || List.any (\(Tuple _ ex) -> containsVar name ex) fs
containsVar name (ExprTyped e _) = containsVar name e
containsVar name (ExprParens e) = containsVar name e
containsVar _ _ = false

containsVarInDoStmt :: String -> DoStatement -> Boolean
containsVarInDoStmt name (DoLet binds) = List.any (\b -> containsVar name b.value) binds
containsVarInDoStmt name (DoBind _ e) = containsVar name e
containsVarInDoStmt name (DoExpr e) = containsVar name e

-- | Get the arity of a lambda expression
lambdaArity :: Expr -> Int
lambdaArity (ExprLambda pats _) = List.length pats
lambdaArity _ = 0

-- | Get the name bound by a let binding (if it's a simple variable pattern)
getBindName :: LetBind -> Maybe String
getBindName bind = case bind.pattern of
  PatVar n -> Just n
  _ -> Nothing

-- | Group consecutive let bindings by name
-- | Bindings with the same name (multiple pattern clauses) are grouped together
groupBindsByName :: Array LetBind -> Array (Array LetBind)
groupBindsByName binds = go binds []
  where
    go :: Array LetBind -> Array (Array LetBind) -> Array (Array LetBind)
    go arr acc = case Array.uncons arr of
      Nothing -> acc
      Just { head: b, tail: rest } ->
        let name = getBindName b
        in case name of
          Nothing -> go rest (Array.snoc acc [b])  -- Non-variable pattern, single group
          Just n ->
            -- Collect all consecutive bindings with the same name
            let spanned = Array.span (\b' -> getBindName b' == Just n) rest
                same = spanned.init
                different = spanned.rest
                group = Array.cons b same
            in go different (Array.snoc acc group)

-- | Sort groups of bindings by dependencies
-- | Groups that don't depend on other groups come first
sortGroupsByDependencies :: Array (Array LetBind) -> Array (Array LetBind)
sortGroupsByDependencies groups =
  let groupName grp = case Array.head grp of
        Nothing -> Nothing
        Just b -> getBindName b
      allNames = Array.mapMaybe groupName groups
      groupDeps grp =
        let selfName = groupName grp
            usedNames = Array.concatMap (\b -> getUsedVars b.value) grp
        in Array.filter (\n -> Just n /= selfName && Array.elem n allNames) usedNames
      groupInfo = map (\g -> { group: g, name: groupName g, deps: groupDeps g }) groups
  in topoSortBindGroups allNames groupInfo []

topoSortBindGroups :: Array String -> Array BindGroupInfo -> Array (Array LetBind) -> Array (Array LetBind)
topoSortBindGroups allNames infos resolved =
  if Array.null infos
  then resolved
  else
    let resolvedNames = Array.mapMaybe extractBindGroupName resolved
        partitioned = Array.partition (canResolveGroup resolvedNames infos) infos
        canResolve = partitioned.yes
        remaining = partitioned.no
    in if Array.null canResolve
       then resolved <> map _.group remaining
       else topoSortBindGroups allNames remaining (resolved <> map _.group canResolve)

type BindGroupInfo = { group :: Array LetBind, name :: Maybe String, deps :: Array String }

extractBindGroupName :: Array LetBind -> Maybe String
extractBindGroupName g = case Array.head g of
  Nothing -> Nothing
  Just b -> getBindName b

canResolveGroup :: Array String -> Array BindGroupInfo -> BindGroupInfo -> Boolean
canResolveGroup resolvedNames infos info =
  Array.all (depResolved resolvedNames infos) info.deps

depResolved :: Array String -> Array BindGroupInfo -> String -> Boolean
depResolved resolvedNames infos d =
  Array.elem d resolvedNames || not (Array.any (hasName d) infos)

hasName :: String -> BindGroupInfo -> Boolean
hasName d info = info.name == Just d

-- | Get variable names used in an expression
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
getUsedVars (ExprSection _) = []  -- operator section like (+ 1), no vars
getUsedVars (ExprSectionLeft e _) = getUsedVars e  -- left section like (1 +)
getUsedVars (ExprSectionRight _ e) = getUsedVars e  -- right section like (+ 1)
getUsedVars (ExprQualified _ _) = []  -- Qualified names are external
getUsedVars (ExprUnaryOp _ e) = getUsedVars e
getUsedVars _ = []

-- | Helper: concatMap for List returning Array
listConcatMap :: forall a b. (a -> Array b) -> List a -> Array b
listConcatMap f lst = foldl (\acc x -> acc <> f x) [] lst

-- | Helper: concatMap for Array containing DoStatement
listConcatMapArray :: forall a b. (a -> Array b) -> List a -> Array b
listConcatMapArray f lst = foldl (\acc x -> acc <> f x) [] lst

-- | Generate code for a group of bindings (possibly multiple pattern clauses)
genBindingGroup :: GenCtx -> Int -> Array LetBind -> String
genBindingGroup ctx indent binds = case binds of
  [] -> ""
  [single] -> genLetBindCtx ctx indent single
  -- Multiple bindings with same name - generate merged function with case
  _ -> case Array.head binds of
    Nothing -> ""
    Just firstBind -> case getBindName firstBind of
      Nothing -> intercalate "\n" (map (genLetBindCtx ctx indent) binds)
      Just name ->
        -- All bindings are lambdas with the same name - merge into one with case
        let clauses = Array.mapMaybe extractLambdaClause binds
            arity = case Array.head clauses of
              Nothing -> 0
              Just c -> length c.patterns
            isRecursive = Array.any (\b -> containsVar name b.value) binds
        in if Array.null clauses || arity == 0
           then intercalate "\n" (map (genLetBindCtx ctx indent) binds)
           else genMergedFunction ctx indent name arity clauses isRecursive

-- | Extract pattern and body from a lambda binding
extractLambdaClause :: LetBind -> Maybe { patterns :: Array Pattern, body :: Expr }
extractLambdaClause bind = case bind.value of
  ExprLambda pats body -> Just { patterns: Array.fromFoldable pats, body }
  _ -> Nothing

-- | Generate a merged function with multiple clauses as a case expression
genMergedFunction :: GenCtx -> Int -> String -> Int -> Array { patterns :: Array Pattern, body :: Expr } -> Boolean -> String
genMergedFunction ctx indent name arity clauses isRecursive =
  let -- Generate parameter names: auto_arg0, auto_arg1, etc.
      argNames = map (\i -> "auto_arg" <> show i) (Array.range 0 (arity - 1))
      argsStr = intercalate ", " argNames
      -- Generate curried lambda headers: fn a -> fn b -> ...
      curriedLambdaHeader = intercalate " " (map (\a -> "fn " <> a <> " ->") argNames)
      curriedLambdaEnds = intercalate "" (map (\_ -> " end") argNames)
      -- Generate case expression matching on tuple of args
      scrutinee = if arity == 1
                  then Array.head argNames # fromMaybe "auto_arg0"
                  else "{" <> argsStr <> "}"
      caseBody = genMergedCaseClauses ctx (indent + 1) arity clauses
      fixFn = case arity of
        1 -> "Nova.Runtime.fix"
        2 -> "Nova.Runtime.fix2"
        3 -> "Nova.Runtime.fix3"
        4 -> "Nova.Runtime.fix4"
        _ -> "Nova.Runtime.fix5"
  in if isRecursive
     then ind indent <> snakeCase name <> " = " <> fixFn <> "(fn " <> snakeCase name <> " -> " <> curriedLambdaHeader <> " case " <> scrutinee <> " do\n" <>
          caseBody <> "\n" <> ind indent <> "end" <> curriedLambdaEnds <> " end)"
     else ind indent <> snakeCase name <> " = " <> curriedLambdaHeader <> " case " <> scrutinee <> " do\n" <>
          caseBody <> "\n" <> ind indent <> "end" <> curriedLambdaEnds

-- | Generate case clauses for merged function
genMergedCaseClauses :: GenCtx -> Int -> Int -> Array { patterns :: Array Pattern, body :: Expr } -> String
genMergedCaseClauses ctx indent arity clauses =
  intercalate "\n" (map genClause clauses)
  where
    genClause :: { patterns :: Array Pattern, body :: Expr } -> String
    genClause clause =
      let patStr = if arity == 1
                   then case Array.head clause.patterns of
                          Nothing -> "_"
                          Just p -> genPattern p
                   else "{" <> intercalate ", " (map genPattern clause.patterns) <> "}"
          ctxWithPats = foldr addLocalsFromPattern ctx clause.patterns
          bodyStr = genExpr' ctxWithPats 0 clause.body
      in ind indent <> patStr <> " -> " <> bodyStr

-- | Get all variable names referenced in a let binding's value
-- | Excludes self-references (those are handled by the fix combinator)
getBindDependencies :: Array String -> LetBind -> Array String
getBindDependencies allNames bind =
  let selfName = getBindName bind
  in Array.filter (\name -> Just name /= selfName && containsVar name bind.value) allNames

-- | Sort let bindings by dependencies (topological sort)
-- | Bindings that don't depend on other bindings come first
sortBindsByDependencies :: Array LetBind -> Array LetBind
sortBindsByDependencies binds =
  let allNames = Array.mapMaybe getBindName binds
      bindInfo = map (mkBindInfo allNames) binds
  in topoSortLetBinds bindInfo []

mkBindInfo :: Array String -> LetBind -> LetBindInfo
mkBindInfo allNames b = { bind: b, name: getBindName b, deps: getBindDependencies allNames b }

type LetBindInfo = { bind :: LetBind, name :: Maybe String, deps :: Array String }

topoSortLetBinds :: Array LetBindInfo -> Array LetBind -> Array LetBind
topoSortLetBinds infos resolved =
  if Array.null infos
  then resolved
  else
    let resolvedNames = Array.mapMaybe getBindName resolved
        partitioned = Array.partition (canResolveLetBind resolvedNames infos) infos
        canResolve = partitioned.yes
        remaining = partitioned.no
    in if Array.null canResolve
       then resolved <> map _.bind remaining
       else topoSortLetBinds remaining (resolved <> map _.bind canResolve)

canResolveLetBind :: Array String -> Array LetBindInfo -> LetBindInfo -> Boolean
canResolveLetBind resolvedNames infos info =
  Array.all (letDepResolved resolvedNames infos) info.deps

letDepResolved :: Array String -> Array LetBindInfo -> String -> Boolean
letDepResolved resolvedNames infos d =
  Array.elem d resolvedNames || not (isBindInfoName d infos)

isBindInfoName :: String -> Array LetBindInfo -> Boolean
isBindInfoName name infos = Array.any (hasInfoName name) infos

hasInfoName :: String -> LetBindInfo -> Boolean
hasInfoName name info = info.name == Just name

-- | Generate let binding
genLetBind :: Int -> LetBind -> String
genLetBind = genLetBindCtx emptyCtx

genLetBindCtx :: GenCtx -> Int -> LetBind -> String
genLetBindCtx ctx indent bind =
  let varName = case bind.pattern of
        PatVar n -> Just n
        _ -> Nothing
      isRecursive = case varName of
        Just n -> containsVar n bind.value
        Nothing -> false
      arity = lambdaArity bind.value
  in case { isRecursive, varName, value: bind.value } of
    -- Recursive lambda binding: use fix combinator
    { isRecursive: true, varName: Just name, value: ExprLambda pats body } ->
      let fixFn = case arity of
            1 -> "Nova.Runtime.fix"
            2 -> "Nova.Runtime.fix2"
            3 -> "Nova.Runtime.fix3"
            4 -> "Nova.Runtime.fix4"
            _ -> "Nova.Runtime.fix5"
          -- Generate curried lambda: fn a -> fn b -> ... body ... end end
          ctxWithParams = foldr addLocalsFromPattern ctx pats
          curriedLambdaHeader = intercalate " " (Array.fromFoldable (map (\p -> "fn " <> genPattern p <> " ->") pats))
          curriedLambdaEnds = intercalate "" (Array.fromFoldable (map (\_ -> " end") pats))
          bodyCode = genExpr' ctxWithParams indent body
      in ind indent <> snakeCase name <> " = " <> fixFn <> "(fn " <> snakeCase name <> " -> " <> curriedLambdaHeader <> " " <> bodyCode <> " " <> curriedLambdaEnds <> " end)"
    -- Non-recursive binding: normal generation
    _ -> ind indent <> genPattern bind.pattern <> " = " <> genExpr' ctx indent bind.value

-- | Generate case clause
genCaseClause :: Int -> CaseClause -> String
genCaseClause = genCaseClauseCtx emptyCtx

-- | Check if an expression is safe to use in an Elixir guard
-- | Only certain expressions are allowed in guards
isGuardSafe :: Expr -> Boolean
isGuardSafe (ExprVar name) = isGuardSafeVar name
isGuardSafe (ExprLit _) = true
isGuardSafe (ExprBinOp op l r) = isGuardSafeOp op && isGuardSafe l && isGuardSafe r
isGuardSafe (ExprUnaryOp "not" e) = isGuardSafe e
isGuardSafe (ExprParens e) = isGuardSafe e
isGuardSafe (ExprApp (ExprVar f) arg) = isGuardSafeFunc f && isGuardSafe arg
isGuardSafe (ExprApp (ExprApp (ExprVar f) arg1) arg2) = isGuardSafeFunc f && isGuardSafe arg1 && isGuardSafe arg2
isGuardSafe _ = false

-- | Check if a variable is safe to use in a guard (it's a bound variable)
isGuardSafeVar :: String -> Boolean
isGuardSafeVar _ = true  -- Variables themselves are always safe

-- | Check if a binary operator is safe in guards
isGuardSafeOp :: String -> Boolean
isGuardSafeOp op = Array.elem op
  [ "==", "/=", "<", ">", "<=", ">=", "&&", "||", "+", "-", "*", "/"
  , "and", "or"
  ]

-- | Check if a function is safe to call in guards
isGuardSafeFunc :: String -> Boolean
isGuardSafeFunc name = Array.elem name
  [ "is_atom", "is_binary", "is_bitstring", "is_boolean", "is_float"
  , "is_function", "is_integer", "is_list", "is_map", "is_nil"
  , "is_number", "is_pid", "is_port", "is_reference", "is_tuple"
  , "abs", "bit_size", "byte_size", "ceil", "div", "elem", "floor"
  , "hd", "length", "map_size", "node", "rem", "round", "self"
  , "tl", "trunc", "tuple_size"
  ]

-- | Check if a guard expression contains a pattern bind (<-)
containsPatternBind :: Expr -> Boolean
containsPatternBind (ExprBinOp "<-" _ _) = true
containsPatternBind (ExprBinOp _ l r) = containsPatternBind l || containsPatternBind r
containsPatternBind (ExprParens e) = containsPatternBind e
containsPatternBind _ = false

-- | Split a guard into pattern binds and boolean conditions
-- | Guard expressions are connected by && and contain <- for pattern binds
-- | Returns (patternBinds, booleanConditions)
splitGuard :: Expr -> { patBinds :: Array { pat :: Expr, expr :: Expr }, conds :: Array Expr }
splitGuard (ExprBinOp "&&" l r) =
  let lResult = splitGuard l
      rResult = splitGuard r
  in { patBinds: lResult.patBinds <> rResult.patBinds
     , conds: lResult.conds <> rResult.conds
     }
splitGuard (ExprBinOp "<-" pat expr) =
  { patBinds: [ { pat, expr } ], conds: [] }
splitGuard (ExprParens e) = splitGuard e
splitGuard e =
  { patBinds: [], conds: [ e ] }

-- | Convert a pattern expression back to a pattern for code generation
-- | This handles patterns that were converted to expressions by the parser
exprToPattern :: Expr -> String
exprToPattern (ExprVar v) = snakeCase v
exprToPattern (ExprApp (ExprVar "Just") arg) = "{:just, " <> exprToPattern arg <> "}"
exprToPattern (ExprApp (ExprVar "Nothing") _) = ":nothing"
exprToPattern (ExprVar "Nothing") = ":nothing"
exprToPattern (ExprApp (ExprVar "Right") arg) = "{:right, " <> exprToPattern arg <> "}"
exprToPattern (ExprApp (ExprVar "Left") arg) = "{:left, " <> exprToPattern arg <> "}"
exprToPattern (ExprApp (ExprApp (ExprVar "Tuple") a) b) = "{:tuple, " <> exprToPattern a <> ", " <> exprToPattern b <> "}"
exprToPattern (ExprApp f arg) =
  -- Generic constructor application
  case f of
    ExprVar con -> "{:" <> snakeCase con <> ", " <> exprToPattern arg <> "}"
    ExprApp _ _ ->
      -- Nested application, need to flatten
      let { func, args } = collectArgs (ExprApp f arg)
      in case func of
        ExprVar con -> "{:" <> snakeCase con <> ", " <> intercalate ", " (map exprToPattern args) <> "}"
        _ -> "_"  -- Fallback
    _ -> "_"  -- Fallback
exprToPattern (ExprLit l) = genLiteral l
exprToPattern (ExprRecord fields) = "%" <> genRecordFields fields
  where
    genRecordFields fs = "{" <> intercalate ", " (Array.fromFoldable (map (\(Tuple k v) -> snakeCase k <> ": " <> exprToPattern v) fs)) <> "}"
exprToPattern (ExprParens e) = exprToPattern e
exprToPattern _ = "_"

-- | Group consecutive guarded clauses with same pattern together
-- | Multiple `pat | guard -> body` clauses become a single `pat ->` with a cond inside
-- | A trailing fallback clause is included as the catch-all
groupWildcardGuardedClauses :: Array CaseClause -> Array (Array CaseClause)
groupWildcardGuardedClauses clauses = groupGuardedGo clauses []

groupGuardedGo :: Array CaseClause -> Array (Array CaseClause) -> Array (Array CaseClause)
groupGuardedGo cs acc = case Array.uncons cs of
  Nothing -> acc
  Just { head: c, tail: rest } ->
    if clauseHasGuard c && not (isClauseGuardSafe c)
    then
      let samePat = sameClausePattern c.pattern
          spanned = Array.span (shouldGroup samePat) rest
          guarded = Array.cons c spanned.init
          groupAndRemaining = computeGroupAndRemaining c.pattern guarded spanned.rest
      in groupGuardedGo groupAndRemaining.remaining (Array.snoc acc groupAndRemaining.group)
    else
      groupGuardedGo rest (Array.snoc acc [c])

shouldGroup :: (Pattern -> Boolean) -> CaseClause -> Boolean
shouldGroup samePat cl = clauseHasGuard cl && samePat cl.pattern

computeGroupAndRemaining :: Pattern -> Array CaseClause -> Array CaseClause -> { group :: Array CaseClause, remaining :: Array CaseClause }
computeGroupAndRemaining origPat guarded restClauses = case Array.uncons restClauses of
  Just { head: next, tail: remaining } ->
    if canPatBeFallback origPat next.pattern
    then { group: Array.snoc guarded next, remaining: remaining }
    else { group: guarded, remaining: restClauses }
  Nothing -> { group: guarded, remaining: [] }

clauseHasGuard :: CaseClause -> Boolean
clauseHasGuard clause = case clause.guard of
  Just _ -> true
  Nothing -> false

isClauseGuardSafe :: CaseClause -> Boolean
isClauseGuardSafe clause = case clause.guard of
  Just g -> isGuardSafe g
  Nothing -> true

canPatBeFallback :: Pattern -> Pattern -> Boolean
canPatBeFallback origPat fallbackPat = case fallbackPat of
  PatWildcard -> true
  _ -> sameClausePattern origPat fallbackPat

sameClausePattern :: Pattern -> Pattern -> Boolean
sameClausePattern p1 p2 = case Tuple p1 p2 of
  Tuple PatWildcard PatWildcard -> true
  Tuple (PatVar _) (PatVar _) -> true
  Tuple (PatCon n1 _) (PatCon n2 _) -> n1 == n2
  Tuple (PatLit _) (PatLit _) -> true
  _ -> false

-- | Generate code for a group of case clauses
-- | Single clause: normal generation
-- | Multiple guarded clauses: generate cond expression
genCaseClauseGroup :: GenCtx -> Int -> Array CaseClause -> String
genCaseClauseGroup ctx indent clauses = case clauses of
  [] -> ""
  [single] -> genCaseClauseCtx ctx indent single
  _ ->
    -- Multiple guarded clauses
    -- Check if last clause is a wildcard - if so, we need to duplicate it for both patterns
    let lastClause = Array.last clauses
        hasWildcardFallback = case lastClause of
          Just c -> case c.pattern of
            PatWildcard -> true
            _ -> false
          Nothing -> false
    in if hasWildcardFallback
       then genWithWildcardFallback ctx indent clauses
       else genWithCondOnly ctx indent clauses

-- | Generate case group where last clause is a wildcard fallback
-- | Generates separate case arms for each pattern type
genWithWildcardFallback :: GenCtx -> Int -> Array CaseClause -> String
genWithWildcardFallback ctx indent clauses =
  let lastClause = case Array.last clauses of
        Just c -> c
        Nothing -> { pattern: PatWildcard, guard: Nothing, body: ExprLit (LitInt 0) }
      initClauses = fromMaybe [] (Array.init clauses)
      firstClause = case Array.head initClauses of
        Just c -> c
        Nothing -> lastClause
      -- Compute used vars from ALL clause bodies and guards (use usedVarsInClause, not freeVarsClause)
      usedVars = foldr (\cl s -> Set.union (usedVarsInClause cl) s) Set.empty clauses
      pat = genPatternWithUsed usedVars firstClause.pattern
      ctxWithPat = addLocalsFromPattern firstClause.pattern ctx
      fallbackBody = genExpr' ctx (indent + 1) lastClause.body
  in ind indent <> pat <> " ->\n" <>
     ind (indent + 1) <> "cond do\n" <>
     intercalate "\n" (map (genCondClause ctxWithPat (indent + 2)) initClauses) <> "\n" <>
     ind (indent + 2) <> "true -> " <> fallbackBody <> "\n" <>
     ind (indent + 1) <> "end\n" <>
     ind indent <> "_ -> " <> fallbackBody

-- | Generate case group using only cond (all clauses have same pattern)
genWithCondOnly :: GenCtx -> Int -> Array CaseClause -> String
genWithCondOnly ctx indent clauses =
  let firstClause = case Array.head clauses of
        Just c -> c
        Nothing -> { pattern: PatWildcard, guard: Nothing, body: ExprLit (LitInt 0) }
      -- Compute used vars from ALL clause bodies and guards (use usedVarsInClause, not freeVarsClause)
      usedVars = foldr (\cl s -> Set.union (usedVarsInClause cl) s) Set.empty clauses
      pat = genPatternWithUsed usedVars firstClause.pattern
      ctxWithPat = addLocalsFromPattern firstClause.pattern ctx
  in ind indent <> pat <> " ->\n" <>
     ind (indent + 1) <> "cond do\n" <>
     intercalate "\n" (map (genCondClause ctxWithPat (indent + 2)) clauses) <> "\n" <>
     ind (indent + 1) <> "end"

-- | Generate a single clause within a cond expression
genCondClause :: GenCtx -> Int -> CaseClause -> String
genCondClause ctx indent clause =
  let body = genExpr' ctx (indent + 1) clause.body
  in case clause.guard of
    Just g -> ind indent <> genExpr' ctx 0 g <> " -> " <> body
    Nothing -> ind indent <> "true -> " <> body  -- Final catch-all

genCaseClauseCtx :: GenCtx -> Int -> CaseClause -> String
genCaseClauseCtx ctx indent clause =
  let ctxWithPat = addLocalsFromPattern clause.pattern ctx
      -- Compute used variables from body and guard to mark unused pattern vars
      bodyVars = freeVarsExpr clause.body
      guardVars = case clause.guard of
        Nothing -> Set.empty
        Just g -> freeVarsExpr g
      usedVars = Set.union bodyVars guardVars
      pat = genPatternWithUsed usedVars clause.pattern
      body = genExpr' ctxWithPat (indent + 1) clause.body
  in case clause.guard of
    Nothing -> ind indent <> pat <> " -> " <> body
    Just g ->
      -- Check if guard contains pattern binds (<-)
      if containsPatternBind g
      then
        -- Use nested case statements for pattern guards
        let { patBinds, conds } = splitGuard g
            -- Update context with variables bound by pattern binds
            ctxWithBinds = foldr addBindVars ctxWithPat patBinds
            bodyWithBinds = genExpr' ctxWithBinds (indent + 1) clause.body
        in ind indent <> pat <> " ->\n" <>
           genPatternBindChain ctxWithPat (indent + 1) patBinds conds bodyWithBinds
      else if isGuardSafe g
      then ind indent <> pat <> " when " <> genExpr' ctxWithPat indent g <> " -> " <> body
      else
        -- Guard uses non-guard-safe functions, convert to if in body
        ind indent <> pat <> " ->\n" <>
        ind (indent + 1) <> "if " <> genExpr' ctxWithPat indent g <> " do\n" <>
        ind (indent + 2) <> body <> "\n" <>
        ind (indent + 1) <> "end"
  where
    -- Add variables from pattern bind expressions to context
    addBindVars :: { pat :: Expr, expr :: Expr } -> GenCtx -> GenCtx
    addBindVars { pat } c = addBindVarsFromExpr pat c

    addBindVarsFromExpr :: Expr -> GenCtx -> GenCtx
    addBindVarsFromExpr (ExprVar v) c = c { locals = Set.insert v c.locals }
    addBindVarsFromExpr (ExprApp f arg) c = addBindVarsFromExpr f (addBindVarsFromExpr arg c)
    addBindVarsFromExpr (ExprRecord fields) c = foldr (\(Tuple _ e) acc -> addBindVarsFromExpr e acc) c fields
    addBindVarsFromExpr (ExprParens e) c = addBindVarsFromExpr e c
    addBindVarsFromExpr _ c = c

-- | Generate nested case statements for pattern binds
-- | Each pattern bind becomes a case statement, with boolean conditions as guards
genPatternBindChain :: GenCtx -> Int -> Array { pat :: Expr, expr :: Expr } -> Array Expr -> String -> String
genPatternBindChain ctx indent patBinds conds body =
  case Array.uncons patBinds of
    Nothing ->
      -- No more pattern binds, check conditions
      if Array.null conds
      then body
      else
        -- Generate if statement for boolean conditions
        let condExpr = Array.foldl (\acc c -> ExprBinOp "&&" acc c) (fromMaybe (ExprLit (LitBool true)) (Array.head conds)) (fromMaybe [] (Array.tail conds))
        in ind indent <> "if " <> genExpr' ctx indent condExpr <> " do\n" <>
           ind (indent + 1) <> body <> "\n" <>
           ind indent <> "end"
    Just { head: pb, tail: restBinds } ->
      -- Generate case for this pattern bind
      let patStr = exprToPattern pb.pat
          exprStr = genExpr' ctx indent pb.expr
          -- Update context with bound variables
          ctxWithBind = addBindVarsFromExpr pb.pat ctx
          innerCode = genPatternBindChain ctxWithBind (indent + 1) restBinds conds body
      in ind indent <> "case " <> exprStr <> " do\n" <>
         ind (indent + 1) <> patStr <> " -> " <> innerCode <> "\n" <>
         ind (indent + 1) <> "_ -> nil\n" <>
         ind indent <> "end"
  where
    addBindVarsFromExpr :: Expr -> GenCtx -> GenCtx
    addBindVarsFromExpr (ExprVar v) c = c { locals = Set.insert v c.locals }
    addBindVarsFromExpr (ExprApp f arg) c = addBindVarsFromExpr f (addBindVarsFromExpr arg c)
    addBindVarsFromExpr (ExprRecord fields) c = foldr (\(Tuple _ e) acc -> addBindVarsFromExpr e acc) c fields
    addBindVarsFromExpr (ExprParens e) c = addBindVarsFromExpr e c
    addBindVarsFromExpr _ c = c

-- | Generate do statements
genDoStmts :: Int -> Array DoStatement -> String
genDoStmts = genDoStmtsCtx emptyCtx

genDoStmtsCtx :: GenCtx -> Int -> Array DoStatement -> String
genDoStmtsCtx ctx indent stmts = case Array.uncons stmts of
  Nothing -> "nil"
  Just { head: stmt, tail: rest } ->
    case stmt of
      DoExpr e ->
        let indStr = repeatStr indent " "
        in if Array.null rest
           then indStr <> genExpr' ctx 0 e
           else indStr <> genExpr' ctx 0 e <> "\n" <> genDoStmtsCtx ctx indent rest
      DoLet binds ->
        let ctxWithBinds = foldr (\b c -> addLocalsFromPattern b.pattern c) ctx binds
        in intercalate "\n" (Array.fromFoldable (map (genLetBindCtx ctx indent) binds)) <> "\n" <> genDoStmtsCtx ctxWithBinds indent rest
      DoBind pat e ->
        -- Monadic bind: detect monad type from expression
        -- Maybe monad: peek, peekAt, charAt, Array.head, Array.find, etc.
        -- Either monad: parse functions, etc.
        -- Generic: use bind function (for Effect, custom monads)
        let ctxWithPat = addLocalsFromPattern pat ctx
            monadType = detectMonadType e
            indStr = repeatStr indent " "
            -- Check if pattern is a Tuple destructuring - common for parser results
            patStr = case pat of
              PatCon "Tuple" (Cons p1 (Cons p2 Nil)) -> "{:tuple, " <> genPattern p1 <> ", " <> genPattern p2 <> "}"
              _ -> genPattern pat
        in if monadType == 0
           then
             -- Maybe monad: use case expression to handle :nothing
             indStr <> "case " <> genExpr' ctx 0 e <> " do\n" <>
             indStr <> "  :nothing -> :nothing\n" <>
             indStr <> "  {:just, " <> patStr <> "} ->\n" <>
             genDoStmtsCtx ctxWithPat (indent + 4) rest <> "\n" <>
             indStr <> "end"
           else if monadType == 1
           then
             -- Either monad: use case expression to propagate :left
             indStr <> "case " <> genExpr' ctx 0 e <> " do\n" <>
             indStr <> "  {:left, err} -> {:left, err}\n" <>
             indStr <> "  {:right, " <> patStr <> "} ->\n" <>
             genDoStmtsCtx ctxWithPat (indent + 4) rest <> "\n" <>
             indStr <> "end"
           else
             -- Generic monad: use bind function from Nova.Runtime
             -- Desugar: x <- e; rest  =>  Nova.Runtime.bind(e, fn x -> rest end)
             indStr <> "Nova.Runtime.bind(" <> genExpr' ctx 0 e <> ", fn " <> patStr <> " ->\n" <>
             genDoStmtsCtx ctxWithPat (indent + 2) rest <> "\n" <>
             indStr <> "end)"

-- | Detect which monad type an expression returns
-- | Returns: 0 = Maybe, 1 = Either, 2 = Generic
detectMonadType :: Expr -> Int
detectMonadType expr = go expr
  where
    go (ExprVar name) = classifyFunc name
    go (ExprQualified _ name) = classifyFunc name
    go (ExprApp f _) = go f  -- Check the function being applied
    go (ExprParens e) = go e
    go _ = 2  -- Generic

    classifyFunc name =
      if isMaybeFunc name then 0
      else if isEitherFunc name then 1
      else 2

    -- Known functions that return Maybe
    isMaybeFunc name =
      name == "peek" || name == "peekAt" || name == "charAt" ||
      name == "head" || name == "tail" || name == "last" || name == "init" ||
      name == "find" || name == "findIndex" || name == "elemIndex" ||
      name == "lookup" || name == "index" || name == "uncons" ||
      name == "fromString" || name == "stripPrefix" || name == "stripSuffix"

    -- Known functions that return Either (parser functions, unification, type inference, CST conversion)
    isEitherFunc name =
      String.contains (String.Pattern "parse") (String.toLower name) ||
      String.contains (String.Pattern "unify") (String.toLower name) ||
      String.contains (String.Pattern "expect") (String.toLower name) ||
      String.contains (String.Pattern "collect") (String.toLower name) ||
      String.contains (String.Pattern "infer") (String.toLower name) ||
      String.contains (String.Pattern "check") (String.toLower name) ||
      String.contains (String.Pattern "instantiate") (String.toLower name) ||
      String.contains (String.Pattern "generalize") (String.toLower name) ||
      String.contains (String.Pattern "lookup") (String.toLower name) ||
      String.contains (String.Pattern "convert") (String.toLower name) ||
      name == "traverse" ||
      name == "success" || name == "failure"

-- | Generate data type (as tagged tuples or structs)
genDataType :: DataType -> String
genDataType dt =
  "  # Data type: " <> dt.name <> "\n" <>
  intercalate "\n" (Array.fromFoldable (map genConstructor dt.constructors))
  where
    genConstructor :: DataConstructor -> String
    genConstructor con =
      if con.isRecord
      then genRecordConstructor con
      else genTupleConstructor con

    genTupleConstructor con =
      let arity = List.length con.fields
          params = Array.mapWithIndex (\i _ -> "arg" <> show i) (Array.fromFoldable con.fields)
          args = intercalate ", " params
          body = if arity == 0
                 then ":" <> snakeCase con.name
                 else "{:" <> snakeCase con.name <> ", " <> args <> "}"
      in "  def " <> snakeCase con.name <> "(" <> args <> "), do: " <> body

    genRecordConstructor con =
      let fieldsArr = Array.fromFoldable con.fields
          params = map (\f -> snakeCase f.label) fieldsArr
          args = intercalate ", " params
          fields = intercalate ", " (map (\f -> snakeCase f.label <> ": " <> snakeCase f.label) fieldsArr)
      in "  def " <> snakeCase con.name <> "(" <> args <> "), do: %{__type__: :" <> snakeCase con.name <> ", " <> fields <> "}"

-- | Generate newtype (single constructor wrapping a type)
genNewtype :: NewtypeDecl -> String
genNewtype nt =
  "  # Newtype: " <> nt.name <> "\n" <>
  "  def " <> snakeCase nt.constructor <> "(arg0), do: {:'" <> snakeCase nt.constructor <> "', arg0}"

-- | Generate type class declaration (just a comment in Elixir)
genTypeClass :: TypeClass -> String
genTypeClass tc =
  "  # Type class: " <> tc.name <> " " <> intercalate " " (Array.fromFoldable tc.typeVars)

-- | Generate type class instance
-- For derived instances, we just emit a comment because Elixir's
-- native operators (==, etc.) handle structural equality
genTypeClassInstance :: TypeClassInstance -> String
genTypeClassInstance inst =
  if inst.derived
  then "  # derive instance " <> inst.className <> " " <> genTypeExpr inst.ty
  else "  # instance " <> inst.className <> " " <> genTypeExpr inst.ty

-- | Generate infix declaration (just a comment - metadata only)
genInfix :: InfixDecl -> String
genInfix inf =
  let assocStr = case inf.associativity of
        AssocLeft -> "infixl"
        AssocRight -> "infixr"
        AssocNone -> "infix"
  in "  # " <> assocStr <> " " <> show inf.precedence <> " " <> inf.functionName <> " as " <> inf.operator

-- | Generate type alias (just a comment in Elixir)
genTypeAlias :: TypeAlias -> String
genTypeAlias ta =
  "  # @type " <> snakeCase ta.name <> " :: " <> genTypeExpr ta.ty

-- | Generate type expression as comment
genTypeExpr :: TypeExpr -> String
genTypeExpr (TyExprCon name) = snakeCase name <> "()"
genTypeExpr (TyExprVar name) = snakeCase name
genTypeExpr (TyExprApp f arg) = genTypeExpr f <> "(" <> genTypeExpr arg <> ")"
genTypeExpr (TyExprArrow a b) = "(" <> genTypeExpr a <> " -> " <> genTypeExpr b <> ")"
genTypeExpr (TyExprRecord fields _) =
  "%{" <> intercalate ", " (Array.fromFoldable (map (\(Tuple l t) -> snakeCase l <> ": " <> genTypeExpr t) fields)) <> "}"
genTypeExpr (TyExprForAll _ t) = genTypeExpr t
genTypeExpr (TyExprConstrained _ t) = genTypeExpr t
genTypeExpr (TyExprParens t) = "(" <> genTypeExpr t <> ")"
genTypeExpr (TyExprTuple ts) = "{" <> intercalate ", " (Array.fromFoldable (map genTypeExpr ts)) <> "}"

-- | Binary operator mapping
genBinOp :: String -> String
genBinOp "+" = "+"
genBinOp "-" = "-"
genBinOp "*" = "*"
genBinOp "/" = "/"
genBinOp "==" = "=="
genBinOp "/=" = "!="
genBinOp "<" = "<"
genBinOp ">" = ">"
genBinOp "<=" = "<="
genBinOp ">=" = ">="
genBinOp "&&" = "and"
genBinOp "||" = "or"
genBinOp "<>" = "<>"
genBinOp "++" = "++"
genBinOp ":" = "|"  -- cons
genBinOp ">>=" = "|> bind"  -- monadic bind
genBinOp op = op  -- pass through

-- | Unary operator mapping
genUnaryOp :: String -> String
genUnaryOp "-" = "-"
genUnaryOp "not" = "not "
genUnaryOp op = op

-- | Convert camelCase to snake_case and handle primed variables
snakeCase :: String -> String
snakeCase s =
  -- First handle primed variables: v' -> v_prime, foo'' -> foo_prime_prime
  let withoutPrimes = handlePrimes s
      result = go (String.toCodePointArray withoutPrimes) false ""
  in escapeReserved result
  where
    handlePrimes str =
      String.replaceAll (String.Pattern "'") (String.Replacement "_prime") str
    go [] _ acc = acc
    go cps prevLower acc = case Array.uncons cps of
      Nothing -> acc
      Just { head: cp, tail: rest } ->
        let cpStr = String.singleton cp
            isUpper = cpStr >= "A" && cpStr <= "Z"
            lower = String.toLower cpStr
            prefix = if isUpper && prevLower then "_" else ""
        in go rest (not isUpper) (acc <> prefix <> lower)

-- | Escape Elixir reserved words by appending underscore
escapeReserved :: String -> String
escapeReserved s = if isReserved s then s <> "_" else s
  where
    isReserved word = Array.elem word elixirReserved
    elixirReserved =
      [ "nil", "true", "false", "do", "end", "if", "else", "unless"
      , "case", "cond", "when", "and", "or", "not", "in", "fn"
      , "def", "defp", "defmodule", "defstruct", "defmacro", "defimpl"
      , "defprotocol", "defexception", "defdelegate", "defguard"
      , "import", "require", "use", "alias", "for", "with", "quote"
      , "unquote", "receive", "try", "catch", "rescue", "after", "raise"
      , "throw", "exit", "super", "spawn", "send", "self"
      -- Elixir special forms and kernel functions that conflict
      , "mod", "rem", "div", "abs", "max", "min"
      ]

-- | Indentation helper
ind :: Int -> String
ind n = String.joinWith "" (Array.replicate (n * 2) " ")

-- | Escape string for Elixir
escapeString :: String -> String
escapeString s = s
  # String.replaceAll (String.Pattern "\\") (String.Replacement "\\\\")
  # String.replaceAll (String.Pattern "\"") (String.Replacement "\\\"")
  # String.replaceAll (String.Pattern "\n") (String.Replacement "\\n")
  # String.replaceAll (String.Pattern "\t") (String.Replacement "\\t")
