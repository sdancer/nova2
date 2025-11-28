module Nova.Compiler.CodeGenWasm where

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
import Data.Foldable (foldr)
import Nova.Compiler.Ast (Module, Declaration(..), FunctionDeclaration, Expr(..), Pattern(..), Literal(..), LetBind, CaseClause, DoStatement(..))

-- WebAssembly Text Format (WAT) Code Generation
-- Simplified version for bootstrap testing

-- Code generation context
type WasmCtx =
  { moduleName :: String
  , moduleFuncs :: Set String
  , locals :: Set String
  , varCounter :: Int
  , dataConstructors :: Map String Int
  }

emptyCtx :: String -> WasmCtx
emptyCtx modName =
  { moduleName: modName
  , moduleFuncs: Set.empty
  , locals: Set.empty
  , varCounter: 0
  , dataConstructors: Map.empty
  }

-- | Mangle a name for WASM identifier
mangleName :: String -> String
mangleName name =
  let escaped = String.replaceAll (String.Pattern "'") (String.Replacement "_prime_") name
      escaped2 = String.replaceAll (String.Pattern ".") (String.Replacement "_") escaped
  in "$" <> escaped2

-- | Mangle module name
mangleModuleName :: String -> String
mangleModuleName name =
  String.toLower (String.replaceAll (String.Pattern ".") (String.Replacement "_") name)

-- | Add locals from pattern
addLocalsFromPattern :: Pattern -> WasmCtx -> WasmCtx
addLocalsFromPattern (PatVar name) ctx = ctx { locals = Set.insert name ctx.locals }
addLocalsFromPattern PatWildcard ctx = ctx
addLocalsFromPattern (PatLit _) ctx = ctx
addLocalsFromPattern (PatCon _ pats) ctx = foldr addLocalsFromPattern ctx pats
addLocalsFromPattern (PatRecord fields) ctx = foldr (\(Tuple _ p) c -> addLocalsFromPattern p c) ctx fields
addLocalsFromPattern (PatList pats) ctx = foldr addLocalsFromPattern ctx pats
addLocalsFromPattern (PatCons hd tl) ctx = addLocalsFromPattern tl (addLocalsFromPattern hd ctx)
addLocalsFromPattern (PatAs name pat) ctx = addLocalsFromPattern pat (ctx { locals = Set.insert name ctx.locals })
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

-- | Generate module
genModule :: Module -> String
genModule m =
  let modName = mangleModuleName m.name
      allFuncs = Array.mapMaybe getFunc m.declarations
      grouped = groupFunctions allFuncs
      uniqueFuncs = Array.nubByEq (\a b -> a.name == b.name && a.arity == b.arity)
                      (map (\g -> { name: g.name, arity: g.arity }) grouped)
      exports = intercalate "\n  " (map genExport uniqueFuncs)
      ctx = (emptyCtx modName) { moduleFuncs = Set.fromFoldable (map (\f -> f.name) uniqueFuncs) }
      funcDefs = intercalate "\n\n" (map (genFunctionGroup ctx) grouped)
  in "(module\n" <>
     "  ;; Module: " <> m.name <> "\n\n" <>
     "  (memory (export \"memory\") 1)\n" <>
     "  (global $heap_ptr (mut i32) (i32.const 1024))\n\n" <>
     "  ;; Runtime helpers\n" <>
     genRuntimeHelpers <> "\n\n" <>
     "  ;; Functions\n" <>
     funcDefs <> "\n\n" <>
     "  ;; Exports\n  " <>
     exports <> "\n" <>
     ")\n"
  where
    getFunc (DeclFunction f) = Just f
    getFunc _ = Nothing

-- | Generate runtime helper functions
genRuntimeHelpers :: String
genRuntimeHelpers =
  "  (func $make_int (param $val i32) (result i32)\n" <>
  "    (i32.or (i32.shl (local.get $val) (i32.const 4)) (i32.const 0))\n" <>
  "  )\n" <>
  "  (func $unbox_int (param $val i32) (result i32)\n" <>
  "    (i32.shr_s (local.get $val) (i32.const 4))\n" <>
  "  )\n" <>
  "  (func $make_bool (param $val i32) (result i32)\n" <>
  "    (i32.or (i32.shl (local.get $val) (i32.const 4)) (i32.const 1))\n" <>
  "  )\n" <>
  "  (func $make_unit (result i32)\n" <>
  "    (i32.const 7)\n" <>
  "  )"

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

-- | Generate simple function
genFunction :: WasmCtx -> FunctionDeclaration -> String
genFunction ctx func =
  let paramNames = Array.mapWithIndex getParamName func.parameters
      params = map (\n -> "(param " <> mangleName n <> " i32)") paramNames
      paramsStr = intercalate " " params
      ctxWithParams = ctx { locals = Set.fromFoldable paramNames }
      bodyCode = genExpr ctxWithParams func.body
  in "  (func " <> mangleName func.name <> " " <> paramsStr <> " (result i32)\n" <>
     "    " <> bodyCode <> "\n" <>
     "  )"

-- | Generate merged function with pattern matching
genMergedFunction :: WasmCtx -> FunctionGroup -> String
genMergedFunction ctx group =
  let paramNames = Array.range 0 (group.arity - 1) # map (\i -> "p" <> show i)
      params = map (\n -> "(param " <> mangleName n <> " i32)") paramNames
      paramsStr = intercalate " " params
      ctxWithParams = ctx { locals = Set.fromFoldable paramNames }
      matchCode = genPatternMatch ctxWithParams paramNames group.clauses
  in "  (func " <> mangleName group.name <> " " <> paramsStr <> " (result i32)\n" <>
     "    " <> matchCode <> "\n" <>
     "  )"

-- | Generate pattern matching code
genPatternMatch :: WasmCtx -> Array String -> Array FunctionDeclaration -> String
genPatternMatch ctx paramNames clauses =
  case Array.uncons clauses of
    Nothing -> "(unreachable)"
    Just { head: clause, tail: rest } ->
      let vars = Array.concatMap patternVars clause.parameters
          ctxWithBindings = ctx { locals = Set.union ctx.locals (Set.fromFoldable vars) }
          bodyCode = genExpr ctxWithBindings clause.body
          fallback = genPatternMatch ctx paramNames rest
      in if Array.null rest
         then bodyCode
         else "(if (result i32) (i32.const 1)\n" <>
              "        (then " <> bodyCode <> ")\n" <>
              "        (else " <> fallback <> "))"

-- | Generate expression
genExpr :: WasmCtx -> Expr -> String
genExpr ctx (ExprVar name) =
  if Set.member name ctx.locals
  then "(local.get " <> mangleName name <> ")"
  else if Set.member name ctx.moduleFuncs
  then "(ref.func " <> mangleName name <> ")"
  else "(local.get " <> mangleName name <> ")"

genExpr _ (ExprQualified modName name) =
  "(call " <> mangleName (modName <> "_" <> name) <> ")"

genExpr _ (ExprLit lit) = genLiteral lit

genExpr ctx (ExprApp func arg) =
  let argCode = genExpr ctx arg
  in case func of
    ExprVar name | Set.member name ctx.moduleFuncs ->
      "(call " <> mangleName name <> " " <> argCode <> ")"
    _ ->
      let funcCode = genExpr ctx func
      in "(call_indirect (type $fn1) " <> argCode <> " " <> funcCode <> ")"

genExpr ctx (ExprLambda pats body) =
  case Array.uncons pats of
    Nothing -> genExpr ctx body
    Just { head: pat, tail: _ } ->
      let vars = patternVars pat
          ctxWithParam = ctx { locals = Set.union ctx.locals (Set.fromFoldable vars) }
      in genExpr ctxWithParam body

genExpr ctx (ExprLet binds body) =
  genLetBinds ctx binds body

genExpr ctx (ExprIf cond thenE elseE) =
  "(if (result i32)\n" <>
  "      (call $unbox_int " <> genExpr ctx cond <> ")\n" <>
  "      (then " <> genExpr ctx thenE <> ")\n" <>
  "      (else " <> genExpr ctx elseE <> ")\n" <>
  "    )"

genExpr ctx (ExprCase scrutinee clauses) =
  genCaseExpr ctx scrutinee clauses

genExpr ctx (ExprBinOp op l r) =
  genBinOp ctx op l r

genExpr ctx (ExprUnaryOp "-" e) =
  "(call $make_int (i32.sub (i32.const 0) (call $unbox_int " <> genExpr ctx e <> ")))"

genExpr ctx (ExprUnaryOp _ e) = genExpr ctx e

genExpr ctx (ExprList elems) =
  case Array.head elems of
    Nothing -> "(call $make_unit)"
    Just h -> genExpr ctx h

genExpr ctx (ExprTuple elems) =
  case Array.head elems of
    Just e -> genExpr ctx e
    Nothing -> "(call $make_unit)"

genExpr _ (ExprRecord _) = "(call $make_unit)"

genExpr ctx (ExprRecordAccess expr _) = genExpr ctx expr

genExpr ctx (ExprRecordUpdate expr _) = genExpr ctx expr

genExpr ctx (ExprParens e) = genExpr ctx e

genExpr ctx (ExprDo stmts) = genDoStmts ctx stmts

genExpr ctx (ExprTyped e _) = genExpr ctx e

genExpr _ (ExprSection _) = "(call $make_unit)"

genExpr ctx (ExprSectionLeft e _) = genExpr ctx e

genExpr ctx (ExprSectionRight _ e) = genExpr ctx e

-- | Generate literal
genLiteral :: Literal -> String
genLiteral (LitInt n) = "(call $make_int (i32.const " <> show n <> "))"
genLiteral (LitNumber _) = "(call $make_int (i32.const 0))"
genLiteral (LitString _) = "(call $make_unit)"
genLiteral (LitChar _) = "(call $make_int (i32.const 0))"
genLiteral (LitBool true) = "(call $make_bool (i32.const 1))"
genLiteral (LitBool false) = "(call $make_bool (i32.const 0))"

-- | Generate let bindings
genLetBinds :: WasmCtx -> Array LetBind -> Expr -> String
genLetBinds ctx binds body =
  case Array.uncons binds of
    Nothing -> genExpr ctx body
    Just { head: bind, tail: rest } ->
      let vars = patternVars bind.pattern
          ctxWithVar = ctx { locals = Set.union ctx.locals (Set.fromFoldable vars) }
          valueCode = genExpr ctx bind.value
          restCode = genLetBinds ctxWithVar rest body
      in case bind.pattern of
           PatVar name ->
             "(block (result i32)\n" <>
             "        (local.set " <> mangleName name <> " " <> valueCode <> ")\n" <>
             "        " <> restCode <> "\n" <>
             "      )"
           _ -> restCode

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
          ctxWithVars = ctx { locals = Set.union ctx.locals (Set.fromFoldable vars) }
          bodyCode = genExpr ctxWithVars clause.body
          fallback = genCaseClauses ctx scrutCode rest
      in case clause.pattern of
           PatVar _ -> bodyCode
           PatWildcard -> bodyCode
           PatLit lit ->
             let check = "(i32.eq " <> scrutCode <> " " <> genLiteral lit <> ")"
             in "(if (result i32) " <> check <> "\n" <>
                "        (then " <> bodyCode <> ")\n" <>
                "        (else " <> fallback <> "))"
           _ ->
             if Array.null rest
             then bodyCode
             else "(if (result i32) (i32.const 1)\n" <>
                  "        (then " <> bodyCode <> ")\n" <>
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
    "==" -> "(call $make_bool (i32.eq " <> lCode <> " " <> rCode <> "))"
    "/=" -> "(call $make_bool (i32.ne " <> lCode <> " " <> rCode <> "))"
    "<" -> "(call $make_bool (i32.lt_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    ">" -> "(call $make_bool (i32.gt_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    "<=" -> "(call $make_bool (i32.le_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    ">=" -> "(call $make_bool (i32.ge_s (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    "&&" -> "(call $make_bool (i32.and (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    "||" -> "(call $make_bool (i32.or (call $unbox_int " <> lCode <> ") (call $unbox_int " <> rCode <> ")))"
    "<>" -> lCode
    ":" -> lCode
    "$" -> "(call_indirect (type $fn1) " <> rCode <> " " <> lCode <> ")"
    "<<<" -> lCode
    ">>>" -> rCode
    _ -> "(call $make_unit)"

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
              ctxWithVars = ctx { locals = Set.union ctx.locals (Set.fromFoldable vars) }
          in "(block (result i32)\n" <>
             "        (drop " <> genExpr ctx e <> ")\n" <>
             "        " <> genDoStmts ctxWithVars rest <> ")"
