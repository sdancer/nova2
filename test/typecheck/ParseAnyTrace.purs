module Test.TypeCheck.ParseAnyTrace where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Nova.Compiler.Ast (Expr(..), Pattern(..), Literal(..), CaseClause)
import Nova.Compiler.Types (emptyEnv, Env, Type(..), applySubst, mkScheme, freshVar, extendEnv, lookupEnv, tArrow, tInt, tString, tMaybe, tEither, freeTypeVars)
import Nova.Compiler.TypeChecker (infer, TCError, instantiate)

main :: Effect Unit
main = do
  log "=== Tracing Q1 vs Q2 ==="

  -- Q2: case f n of Right r -> Right r; Left _ -> Left "fail"
  -- This is simpler, let's trace it first
  log "\n-- Q2: Simple nested case --"
  let env0 = emptyEnv
  -- Add f :: Int -> Either String Int
  let fScheme = mkScheme [] (tArrow tInt (tEither tString tInt))
  let env1 = extendEnv env0 "f" fScheme
  -- Add n :: Int
  let env2 = extendEnv env1 "n" (mkScheme [] tInt)

  -- Build: case f n of ...
  let fnApp = ExprApp (ExprVar "f") (ExprVar "n")
  let rightClause :: CaseClause
      rightClause = { pattern: PatCon "Right" [PatVar "r"], body: ExprApp (ExprVar "Right") (ExprVar "r"), guard: Nothing }
  let leftClause :: CaseClause
      leftClause = { pattern: PatCon "Left" [PatWildcard], body: ExprApp (ExprVar "Left") (ExprLit (LitString "fail")), guard: Nothing }
  let caseExpr = ExprCase fnApp [rightClause, leftClause]

  log "Inferring: case f n of Right r -> Right r; Left _ -> Left \"fail\""
  case infer env2 caseExpr of
    Left err -> log $ "  ERROR: " <> show err
    Right res -> do
      log $ "  OK! Type: " <> showType res.ty

  -- Q1: case mx of Nothing -> Left; Just n -> case f n of ...
  -- The difference: outer case introduces mx binding
  log "\n-- Q1: Nested case --"
  -- Add mx :: Maybe Int
  let env3 = extendEnv env1 "mx" (mkScheme [] (tMaybe tInt))

  -- Build outer case
  let nothingClause :: CaseClause
      nothingClause = { pattern: PatCon "Nothing" [], body: ExprApp (ExprVar "Left") (ExprLit (LitString "None")), guard: Nothing }
  let justClause :: CaseClause
      justClause = { pattern: PatCon "Just" [PatVar "n"], body: caseExpr, guard: Nothing }
  let outerCase = ExprCase (ExprVar "mx") [nothingClause, justClause]

  log "Inferring: case mx of Nothing -> Left \"None\"; Just n -> case f n of ..."
  case infer env3 outerCase of
    Left err -> log $ "  ERROR: " <> show err
    Right res -> do
      log $ "  OK! Type: " <> showType res.ty

  -- Q1b: Same but without f, using Just n directly
  log "\n-- Q1b: Without f, Just n as scrutinee --"
  let fnAppJust = ExprApp (ExprVar "Just") (ExprVar "n")
  let justClause2 :: CaseClause
      justClause2 = { pattern: PatCon "Just" [PatVar "r"], body: ExprApp (ExprVar "Right") (ExprVar "r"), guard: Nothing }
  let nothingClause2 :: CaseClause
      nothingClause2 = { pattern: PatCon "Nothing" [], body: ExprApp (ExprVar "Left") (ExprLit (LitString "fail")), guard: Nothing }
  let innerCase2 = ExprCase fnAppJust [justClause2, nothingClause2]
  let justClauseOuter :: CaseClause
      justClauseOuter = { pattern: PatCon "Just" [PatVar "n"], body: innerCase2, guard: Nothing }
  let outerCase2 = ExprCase (ExprVar "mx") [nothingClause, justClauseOuter]

  log "Inferring: case mx of Nothing -> ...; Just n -> case Just n of ..."
  case infer env3 outerCase2 of
    Left err -> log $ "  ERROR: " <> show err
    Right res -> do
      log $ "  OK! Type: " <> showType res.ty

  log "\n=== Done ==="

showType :: Type -> String
showType (TyVar v) = v.name <> "(id=" <> show v.id <> ")"
showType (TyCon tc) =
  if Array.null tc.args
  then tc.name
  else tc.name <> "(" <> Array.intercalate ", " (map showType tc.args) <> ")"
showType (TyRecord _) = "{...}"
