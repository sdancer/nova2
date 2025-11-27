module Test.TypeCheck.OccursCheckTrace3 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast (Expr(..), Pattern(..), LetBind, Literal(..))
import Nova.Compiler.Types (emptyEnv, Env, Type(..), Scheme, applySubst, mkScheme, freshVar, extendEnv, lookupEnv, tArrow, tInt, tString, composeSubst)
import Nova.Compiler.TypeChecker (infer, TCError, instantiate, generalize)
import Nova.Compiler.Unify (unify)
import Data.Set as Set

main :: Effect Unit
main = do
  log "=== Trace the let binding process ==="

  let env0 = emptyEnv

  -- Step 1: Create placeholder for helper
  log "\n-- Step 1: Create placeholder --"
  let Tuple helperTv env1 = freshVar env0 "let_helper"
  let env2 = extendEnv env1 "helper" (mkScheme [] (TyVar helperTv))
  log $ "  Placeholder: " <> showTVar helperTv

  -- Step 2: Infer the lambda \sub k -> case ...
  log "\n-- Step 2: Infer lambda expression --"
  let lambdaSrc = """\\sub k -> case Just 1 of
    Just n -> Right n
    Nothing -> Right sub"""
  let tokens = tokenize lambdaSrc
  case P.parseExpression tokens of
    Left err -> log $ "  Parse error: " <> err
    Right (Tuple lambdaExpr _) -> do
      log "  Parsed lambda"
      case infer env2 lambdaExpr of
        Left err -> log $ "  INFER ERROR: " <> show err
        Right lambdaRes -> do
          log $ "  Lambda type: " <> showType lambdaRes.ty
          log $ "  Substitution effects:"
          log $ "    helperTv after sub: " <> showType (applySubst lambdaRes.sub (TyVar helperTv))

          -- Step 3: Generalize the lambda type
          log "\n-- Step 3: Generalize --"
          let scheme = generalize lambdaRes.env lambdaRes.ty
          log $ "  Scheme vars: " <> show (Array.length scheme.vars)
          log $ "  Scheme type: " <> showType scheme.ty

          -- Step 4: Update helper binding
          log "\n-- Step 4: Update helper binding --"
          let env3 = extendEnv lambdaRes.env "helper" scheme

          -- Step 5: Infer helper 0 "k"
          log "\n-- Step 5: Infer application 'helper 0' --"
          case lookupEnv env3 "helper" of
            Nothing -> log "  ERROR: helper not in env"
            Just helperScheme -> do
              log $ "  helper scheme vars: " <> show (Array.length helperScheme.vars)
              log $ "  helper scheme type: " <> showType helperScheme.ty

              -- Instantiate helper
              let helperInst = instantiate env3 helperScheme
              log $ "  Instantiated: " <> showType helperInst.ty

              -- Apply to 0
              log "\n  Applying to 0:"
              case infer helperInst.env (ExprLit (LitInt 0)) of
                Left err -> log $ "    ERROR: " <> show err
                Right argRes -> do
                  log $ "    Arg type: " <> showType argRes.ty

                  -- Unify helper type with (Int -> result)
                  let Tuple r1Tv env4 = freshVar argRes.env "r"
                  log $ "    Result var: " <> showTVar r1Tv

                  log $ "    Unifying: " <> showType (applySubst argRes.sub helperInst.ty) <> " with (Int -> " <> showTVar r1Tv <> ")"
                  case unify (applySubst argRes.sub helperInst.ty) (tArrow tInt (TyVar r1Tv)) of
                    Left ue -> log $ "    UNIFY ERROR: " <> show ue
                    Right s1 -> do
                      log $ "    Unify OK"
                      let app1Ty = applySubst s1 (TyVar r1Tv)
                      log $ "    app1 type: " <> showType app1Ty

                      -- Now apply to "k"
                      log "\n  Applying result to \"k\":"
                      case infer env4 (ExprLit (LitString "k")) of
                        Left err -> log $ "    ERROR: " <> show err
                        Right arg2Res -> do
                          log $ "    Arg type: " <> showType arg2Res.ty

                          let Tuple r2Tv env5 = freshVar arg2Res.env "r"
                          log $ "    Result var: " <> showTVar r2Tv

                          let app1TyWithSubs = applySubst arg2Res.sub app1Ty
                          log $ "    Unifying: " <> showType app1TyWithSubs <> " with (String -> " <> showTVar r2Tv <> ")"
                          case unify app1TyWithSubs (tArrow tString (TyVar r2Tv)) of
                            Left ue -> log $ "    UNIFY ERROR: " <> show ue
                            Right s2 -> do
                              log $ "    Unify OK"
                              let finalTy = applySubst s2 (TyVar r2Tv)
                              log $ "    Final type: " <> showType finalTy

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
