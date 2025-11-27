module Test.TypeCheck.OccursCheckTrace4 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Nova.Compiler.Ast (Expr(..), Pattern(..), LetBind, CaseClause, Literal(..))
import Nova.Compiler.Types (emptyEnv, Env, Type(..), Scheme, applySubst, mkScheme, freshVar, extendEnv, lookupEnv, tArrow, tInt, tString, composeSubst)
import Nova.Compiler.TypeChecker (infer, TCError, instantiate, generalize)
import Nova.Compiler.Unify (unify)

main :: Effect Unit
main = do
  log "=== Trace using AST constructors ==="

  let env0 = emptyEnv

  -- Build the AST manually:
  -- let helper sub k = case Just 1 of
  --       Just n -> Right n
  --       Nothing -> Right sub
  -- in helper 0 "k"

  -- Case clauses
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

  -- case Just 1 of ...
  let caseExpr = ExprCase
        (ExprApp (ExprVar "Just") (ExprLit (LitInt 1)))
        [justClause, nothingClause]

  -- \sub k -> case ...
  let lambdaExpr = ExprLambda [PatVar "sub", PatVar "k"] caseExpr

  -- let helper = \sub k -> ... in helper 0 "k"
  let letBind :: LetBind
      letBind = { pattern: PatVar "helper", value: lambdaExpr, typeAnn: Nothing }

  let body = ExprApp
        (ExprApp (ExprVar "helper") (ExprLit (LitInt 0)))
        (ExprLit (LitString "k"))

  let fullExpr = ExprLet [letBind] body

  -- Now infer
  log "\n-- Full expression --"
  case infer env0 fullExpr of
    Left err -> log $ "  INFER ERROR: " <> show err
    Right res -> do
      log $ "  Type: " <> showType res.ty

  -- Now trace step by step

  -- Step 1: Create placeholder for helper
  log "\n-- Step 1: Create placeholder --"
  let Tuple helperTv env1 = freshVar env0 "let_helper"
  let env2 = extendEnv env1 "helper" (mkScheme [] (TyVar helperTv))
  log $ "  Placeholder: " <> showTVar helperTv

  -- Step 2: Infer the lambda
  log "\n-- Step 2: Infer lambda --"
  case infer env2 lambdaExpr of
    Left err -> log $ "  INFER ERROR: " <> show err
    Right lambdaRes -> do
      log $ "  Lambda type: " <> showType lambdaRes.ty
      log $ "  placeholder after: " <> showType (applySubst lambdaRes.sub (TyVar helperTv))

      -- Step 3: Generalize
      log "\n-- Step 3: Generalize --"
      let genType = applySubst lambdaRes.sub lambdaRes.ty
      log $ "  Type to generalize: " <> showType genType
      let scheme = generalize lambdaRes.env genType
      log $ "  Scheme vars: " <> show (Array.length scheme.vars)
      log $ "  Scheme type: " <> showType scheme.ty

      -- Step 4: Add to env
      log "\n-- Step 4: Add to env --"
      let env3 = extendEnv lambdaRes.env "helper" scheme

      -- Step 5: Infer body
      log "\n-- Step 5: Infer body (helper 0 \"k\") --"
      case infer env3 body of
        Left err -> log $ "  INFER ERROR: " <> show err
        Right bodyRes -> do
          log $ "  Body type: " <> showType bodyRes.ty

      -- Step 5b: Infer applications step by step
      log "\n-- Step 5b: Manual application trace --"
      case lookupEnv env3 "helper" of
        Nothing -> log "  helper not found"
        Just helperScheme -> do
          log $ "  helper scheme: " <> show (Array.length helperScheme.vars) <> " vars, type: " <> showType helperScheme.ty

          let inst = instantiate env3 helperScheme
          log $ "  Instantiated: " <> showType inst.ty

          -- Apply to 0
          let Tuple r1Tv env4 = freshVar inst.env "r"
          log $ "  Fresh r1: " <> showTVar r1Tv
          log $ "  Unifying: " <> showType inst.ty <> " with (Int -> " <> showTVar r1Tv <> ")"
          case unify inst.ty (tArrow tInt (TyVar r1Tv)) of
            Left ue -> log $ "  UNIFY ERROR: " <> show ue
            Right s1 -> do
              log $ "  Success!"
              let r1After = applySubst s1 (TyVar r1Tv)
              log $ "  r1 after: " <> showType r1After

              -- Apply to "k"
              let Tuple r2Tv env5 = freshVar env4 "r"
              log $ "  Fresh r2: " <> showTVar r2Tv
              log $ "  Unifying: " <> showType r1After <> " with (String -> " <> showTVar r2Tv <> ")"
              case unify r1After (tArrow tString (TyVar r2Tv)) of
                Left ue -> log $ "  UNIFY ERROR: " <> show ue
                Right s2 -> do
                  log $ "  Success!"
                  let finalTy = applySubst s2 (TyVar r2Tv)
                  log $ "  Final type: " <> showType finalTy

  log "\n=== Done ==="

showTVar :: { id :: Int, name :: String } -> String
showTVar tv = tv.name <> " (id=" <> show tv.id <> ")"

showType :: Type -> String
showType (TyVar v) = showTVar v
showType (TyCon tc) =
  if Array.null tc.args
  then tc.name
  else tc.name <> "(" <> Array.intercalate ", " (map showType tc.args) <> ")"
showType (TyRecord _) = "{...}"
