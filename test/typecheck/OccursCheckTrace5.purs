module Test.TypeCheck.OccursCheckTrace5 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Set as Set
import Nova.Compiler.Ast (Expr(..), Pattern(..), LetBind, CaseClause, Literal(..))
import Nova.Compiler.Types (emptyEnv, Env, Type(..), Scheme, applySubst, mkScheme, freshVar, extendEnv, lookupEnv, tArrow, tInt, tString, freeTypeVars, freeTypeVarsEnv)
import Nova.Compiler.TypeChecker (infer, TCError, generalize)

main :: Effect Unit
main = do
  log "=== Checking generalization ==="

  let env0 = emptyEnv

  -- Build the AST manually
  let justClause :: CaseClause
      justClause =
        { pattern: PatCon "Just" [PatVar "n"]
        , body: ExprApp (ExprVar "Right") (ExprVar "n")
        , guard: Nothing
        }

  let nothingClause :: CaseClause
      nothingClause =
        { pattern: PatCon "Nothing" []
        , body: ExprApp (ExprVar "Right") (ExprVar "sub")
        , guard: Nothing
        }

  let caseExpr = ExprCase
        (ExprApp (ExprVar "Just") (ExprLit (LitInt 1)))
        [justClause, nothingClause]

  let lambdaExpr = ExprLambda [PatVar "sub", PatVar "k"] caseExpr

  -- Step 1: Create placeholder for helper
  let Tuple helperTv env1 = freshVar env0 "let_helper"
  let env2 = extendEnv env1 "helper" (mkScheme [] (TyVar helperTv))

  log $ "Placeholder type var id: " <> show helperTv.id

  -- Step 2: Infer the lambda
  case infer env2 lambdaExpr of
    Left err -> log $ "INFER ERROR: " <> show err
    Right lambdaRes -> do
      log $ "Lambda type: " <> showType lambdaRes.ty
      log $ "Lambda type free vars: " <> showFreeVars (freeTypeVars lambdaRes.ty)

      log $ "\nEnvironment free vars: " <> showFreeVars (freeTypeVarsEnv lambdaRes.env)

      -- Check what helper is bound to in the new env
      case lookupEnv lambdaRes.env "helper" of
        Nothing -> log "helper not found"
        Just helperScheme -> do
          log $ "helper scheme type: " <> showType helperScheme.ty
          log $ "helper scheme type free vars: " <> showFreeVars (freeTypeVars helperScheme.ty)

      -- Difference (what gets generalized)
      let tyFree = freeTypeVars lambdaRes.ty
      let envFree = freeTypeVarsEnv lambdaRes.env
      let diff = Set.difference tyFree envFree
      log $ "\nDifference (to generalize): " <> showFreeVars diff

      -- What the scheme would look like
      let scheme = generalize lambdaRes.env lambdaRes.ty
      log $ "\nGeneralized scheme:"
      log $ "  vars: " <> show (Array.length scheme.vars)
      log $ "  type: " <> showType scheme.ty

      -- Check what "sub" is bound to
      case lookupEnv lambdaRes.env "sub" of
        Nothing -> log "\nsub not found"
        Just subScheme -> do
          log $ "\nsub scheme type: " <> showType subScheme.ty
          log $ "sub scheme type free vars: " <> showFreeVars (freeTypeVars subScheme.ty)

      -- Check what "k" is bound to
      case lookupEnv lambdaRes.env "k" of
        Nothing -> log "\nk not found"
        Just kScheme -> do
          log $ "\nk scheme type: " <> showType kScheme.ty
          log $ "k scheme type free vars: " <> showFreeVars (freeTypeVars kScheme.ty)

  log "\n=== Done ==="

showType :: Type -> String
showType (TyVar v) = v.name <> "(id=" <> show v.id <> ")"
showType (TyCon tc) =
  if Array.null tc.args
  then tc.name
  else tc.name <> "(" <> Array.intercalate ", " (map showType tc.args) <> ")"
showType (TyRecord _) = "{...}"

showFreeVars :: Set.Set Int -> String
showFreeVars s = "{" <> Array.intercalate ", " (map show (Set.toUnfoldable s :: Array Int)) <> "}"
