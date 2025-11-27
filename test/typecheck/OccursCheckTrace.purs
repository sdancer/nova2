module Test.TypeCheck.OccursCheckTrace where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast (Expr(..), Pattern(..), LetBind, CaseClause, Literal(..))
import Nova.Compiler.Types (emptyEnv, Env, Type(..), Subst, Scheme, applySubst, freeTypeVars, mkScheme, freshVar, extendEnv, lookupEnv, tArrow, tMaybe, tEither, tInt, tString, emptySubst, composeSubst)
import Nova.Compiler.TypeChecker (infer, instantiate, inferPat, generalize, TCError)
import Nova.Compiler.Unify (unify)
import Data.Set as Set

-- Manual trace through the inference for:
--   let helper sub k = case Just 1 of
--         Just n -> Right n
--         Nothing -> Right sub
--   in helper 0 "k"

main :: Effect Unit
main = do
  log "=== Manual inference trace ==="

  let env0 = emptyEnv

  -- Step 1: inferBinds adds placeholder for helper
  log "\n-- Step 1: Add placeholder for helper --"
  let Tuple helperTv env1 = freshVar env0 "let_helper"
  log $ "  Created placeholder: " <> showTVar helperTv
  let env2 = extendEnv env1 "helper" (mkScheme [] (TyVar helperTv))

  -- Step 2: Infer the value (lambda \sub k -> ...)
  log "\n-- Step 2: Infer lambda --"

  -- Step 2a: First param 'sub'
  let Tuple subTv env3 = freshVar env2 "a"
  log $ "  param 'sub': " <> showTVar subTv
  let env4 = extendEnv env3 "sub" (mkScheme [] (TyVar subTv))

  -- Step 2b: Second param 'k'
  let Tuple kTv env5 = freshVar env4 "a"
  log $ "  param 'k': " <> showTVar kTv
  let env6 = extendEnv env5 "k" (mkScheme [] (TyVar kTv))

  -- Step 2c: Infer body (case expression)
  log "\n-- Step 3: Infer case expression --"

  -- 3a: Infer scrutinee (Just 1)
  log "  3a: Infer scrutinee (Just 1)"
  -- Just 1 has type Maybe Int
  let scrutTy = tMaybe tInt
  log $ "    scrutinee type: Maybe Int"

  -- 3b: Create result type variable
  let Tuple caseTv env7 = freshVar env6 "case"
  log $ "  3b: Case result var: " <> showTVar caseTv
  let caseTy = TyVar caseTv

  -- 3c: Infer clause 1: Just n -> Right n
  log "\n  3c: Clause 1: Just n -> Right n"

  -- Pattern Just n binds n : Int
  let Tuple nTv env8 = freshVar env7 "pcon"
  log $ "    Pattern binding n: Int (via Just pattern)"
  let env9 = extendEnv env8 "n" (mkScheme [] tInt)

  -- Infer Right n
  log "    Infer 'Right n':"
  -- Right : forall a b. b -> Either a b
  case lookupEnv env9 "Right" of
    Nothing -> log "    ERROR: Right not found"
    Just rightScheme -> do
      log $ "    Right scheme has " <> show (Array.length rightScheme.vars) <> " vars"
      let rightInst = instantiate env9 rightScheme
      log $ "    Instantiated Right type: " <> showType rightInst.ty
      -- Apply to n : Int
      case rightInst.ty of
        TyCon tc | tc.name == "Fun" -> do
          log $ "    Applying to n : Int"
          -- Result should be Either ?a Int
          case Array.last tc.args of
            Just resultTy -> log $ "    Result type: " <> showType resultTy
            Nothing -> log "    ERROR: malformed Fun type"
        _ -> log $ "    Not a function: " <> showType rightInst.ty

      -- Now we need to unify this with caseTy
      let Tuple r1Tv env10 = freshVar rightInst.env "r"
      log $ "    Fresh result var for app: " <> showTVar r1Tv

      -- Unify rightInst.ty with (Int -> r1)
      log $ "    Unify: " <> showType rightInst.ty <> " with (Int -> " <> showTVar r1Tv <> ")"
      case unify rightInst.ty (tArrow tInt (TyVar r1Tv)) of
        Left ue -> log $ "    UNIFY ERROR: " <> show ue
        Right s1 -> do
          log $ "    Unification succeeded"
          let rightNType = applySubst s1 (TyVar r1Tv)
          log $ "    Right n type: " <> showType rightNType

          -- Unify with case result type
          log $ "    Unify result with case type: " <> showType rightNType <> " with " <> showType (applySubst s1 caseTy)
          case unify (applySubst s1 caseTy) rightNType of
            Left ue -> log $ "    UNIFY ERROR: " <> show ue
            Right s2 -> do
              log $ "    Unification succeeded"
              let combinedSub = composeSubst s2 s1
              let caseTypeNow = applySubst combinedSub caseTy
              log $ "    Case type is now: " <> showType caseTypeNow

              -- 3d: Infer clause 2: Nothing -> Right sub
              log "\n  3d: Clause 2: Nothing -> Right sub"

              -- Pattern Nothing - no bindings
              log "    Pattern Nothing - no bindings"

              -- Infer Right sub
              log "    Infer 'Right sub':"
              let rightInst2 = instantiate env10 rightScheme
              log $ "    Instantiated Right type: " <> showType rightInst2.ty

              -- sub : TyVar subTv
              log $ "    Applying to sub : " <> showTVar subTv
              let Tuple r2Tv env11 = freshVar rightInst2.env "r"
              log $ "    Fresh result var for app: " <> showTVar r2Tv

              -- Unify rightInst2.ty with (subTv -> r2)
              log $ "    Unify: " <> showType rightInst2.ty <> " with (" <> showTVar subTv <> " -> " <> showTVar r2Tv <> ")"
              case unify rightInst2.ty (tArrow (TyVar subTv) (TyVar r2Tv)) of
                Left ue -> log $ "    UNIFY ERROR: " <> show ue
                Right s3 -> do
                  log $ "    Unification succeeded"
                  let rightSubType = applySubst s3 (TyVar r2Tv)
                  log $ "    Right sub type: " <> showType rightSubType

                  -- Unify with case result type (which is now caseTypeNow)
                  let caseTypeWithS3 = applySubst s3 caseTypeNow
                  log $ "    Unify result with case type: " <> showType rightSubType <> " with " <> showType caseTypeWithS3
                  case unify caseTypeWithS3 rightSubType of
                    Left ue -> log $ "    UNIFY ERROR: " <> show ue
                    Right s4 -> do
                      log $ "    Unification succeeded"
                      let finalSub = composeSubst s4 (composeSubst s3 combinedSub)
                      let finalCaseTy = applySubst finalSub caseTy
                      log $ "    Final case type: " <> showType finalCaseTy

                      -- Now back up to lambda
                      log "\n-- Step 4: Build lambda type --"
                      let subTyFinal = applySubst finalSub (TyVar subTv)
                      let kTyFinal = applySubst finalSub (TyVar kTv)
                      log $ "    sub type: " <> showType subTyFinal
                      log $ "    k type: " <> showType kTyFinal
                      log $ "    body type: " <> showType finalCaseTy

                      let innerLambdaTy = tArrow kTyFinal finalCaseTy
                      let fullLambdaTy = tArrow subTyFinal innerLambdaTy
                      log $ "    Full lambda type: " <> showType fullLambdaTy

                      -- Step 5: Unify with placeholder
                      log "\n-- Step 5: Unify with placeholder --"
                      let placeholderTy = applySubst finalSub (TyVar helperTv)
                      log $ "    Placeholder type: " <> showType placeholderTy
                      log $ "    Lambda type: " <> showType fullLambdaTy

                      case unify placeholderTy fullLambdaTy of
                        Left ue -> log $ "    UNIFY ERROR: " <> show ue
                        Right s5 -> do
                          log $ "    Unification succeeded"
                          log $ "    Helper type: " <> showType (applySubst s5 fullLambdaTy)

  log "\n=== Done ==="

showTVar :: { id :: Int, name :: String } -> String
showTVar tv = tv.name <> " (id=" <> show tv.id <> ")"

showType :: Type -> String
showType (TyVar v) = showTVar v
showType (TyCon tc) =
  if Array.null tc.args
  then tc.name
  else tc.name <> "(" <> showTypes tc.args <> ")"
showType (TyRecord r) = "{...}"

showTypes :: Array Type -> String
showTypes ts = Array.intercalate ", " (map showType ts)
