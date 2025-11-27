module Test.TypeCheck.OccursCheckTrace2 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast (Expr(..), Pattern(..), LetBind, Declaration(..), Literal(..))
import Nova.Compiler.Types (emptyEnv, Env, Type(..), applySubst, mkScheme, freshVar, extendEnv, lookupEnv, tArrow, tMaybe, tInt, composeSubst)
import Nova.Compiler.TypeChecker (infer, TCError, inferBinds)

main :: Effect Unit
main = do
  log "=== Trace using actual infer function ==="

  -- Test 1: Failing case (two params, case + Right)
  log "\n-- Test 1: Two params, case + Right (should fail) --"
  traceInfer """
let helper sub k = case Just 1 of
      Just n -> Right n
      Nothing -> Right sub
in helper 0 "k"
"""

  -- Test 2: Working case (one param)
  log "\n-- Test 2: One param, case + Right (should work) --"
  traceInfer """
let helper sub = case Just 1 of
      Just n -> Right n
      Nothing -> Right sub
in helper 0
"""

  -- Test 3: Two params but no case
  log "\n-- Test 3: Two params, no case (should work) --"
  traceInfer """
let helper sub k = Right sub
in helper 0 "k"
"""

  -- Test 4: Two params, case but no Right
  log "\n-- Test 4: Two params, case, no Right (should work) --"
  traceInfer """
let helper sub k = case Just 1 of
      Just n -> n
      Nothing -> sub
in helper 0 "k"
"""

  -- Test 5: Two params, case + Just (not Right)
  log "\n-- Test 5: Two params, case + Just (should work) --"
  traceInfer """
let helper sub k = case Just 1 of
      Just n -> Just n
      Nothing -> Just sub
in helper 0 "k"
"""

  -- Test 6: Try tracing helper body alone
  log "\n-- Test 6: Just the lambda body (case expression) --"
  let env0 = emptyEnv
  let Tuple subTv env1 = freshVar env0 "sub"
  let env2 = extendEnv env1 "sub" (mkScheme [] (TyVar subTv))
  let Tuple kTv env3 = freshVar env2 "k"
  let env4 = extendEnv env3 "k" (mkScheme [] (TyVar kTv))

  -- Now infer just the case expression
  let caseExprSrc = """case Just 1 of
    Just n -> Right n
    Nothing -> Right sub"""
  let caseTokens = tokenize caseExprSrc
  case P.parseExpression caseTokens of
    Left err -> log $ "  Parse error: " <> err
    Right (Tuple caseExpr _) -> do
      log $ "  Parsed case expression"
      case infer env4 caseExpr of
        Left err -> log $ "  INFER ERROR: " <> show err
        Right res -> do
          log $ "  Case inferred OK"
          log $ "  Type: " <> showType res.ty
          log $ "  sub became: " <> showType (applySubst res.sub (TyVar subTv))
          log $ "  k became: " <> showType (applySubst res.sub (TyVar kTv))

  log "\n=== Done ==="

traceInfer :: String -> Effect Unit
traceInfer src = do
  let tokens = tokenize src
  case P.parseExpression tokens of
    Left err -> log $ "  Parse error: " <> err
    Right (Tuple expr _) -> do
      log $ "  Parsed expression"
      case infer emptyEnv expr of
        Left err -> log $ "  INFER ERROR: " <> show err
        Right res -> do
          log $ "  Inferred OK"
          log $ "  Type: " <> showType res.ty

showType :: Type -> String
showType (TyVar v) = v.name
showType (TyCon tc) =
  if Array.null tc.args
  then tc.name
  else tc.name <> "(" <> Array.intercalate ", " (map showType tc.args) <> ")"
showType (TyRecord _) = "{...}"
