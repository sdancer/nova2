module Nova.Compiler.Unify where

import Prelude
import Data.Either (Either(..))
import Data.Map as Map
import Data.Set as Set
import Data.Array (zip, length)
import Data.Foldable (foldM)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Nova.Compiler.Types (Type(..), TVar, TCon, Subst, emptySubst, singleSubst, composeSubst, applySubst, freeTypeVars)

-- | Unification error type
data UnifyError
  = OccursCheck TVar Type
  | TypeMismatch Type Type
  | ArityMismatch String Int Int
  | RecordFieldMismatch String

instance showUnifyError :: Show UnifyError where
  show (OccursCheck v t) = "Occurs check: " <> v.name <> " in type"
  show (TypeMismatch t1 t2) = "Type mismatch: " <> showType t1 <> " vs " <> showType t2
  show (ArityMismatch name n1 n2) = "Arity mismatch for " <> name <> ": " <> show n1 <> " vs " <> show n2
  show (RecordFieldMismatch f) = "Record field mismatch: " <> f

showType :: Type -> String
showType (TyVar v) = v.name <> "[" <> show v.id <> "]"
showType (TyCon tc) = tc.name <> "(" <> show (length tc.args) <> " args)"
showType (TyRecord r) = "{record}"

-- | Occurs check: does variable v occur in type t?
occurs :: TVar -> Type -> Boolean
occurs v t = Set.member v.id (freeTypeVars t)

-- | Bind a type variable to a type, with occurs check
bindVar :: TVar -> Type -> Either UnifyError Subst
bindVar v t
  | TyVar v' <- t, v.id == v'.id = Right emptySubst
  | occurs v t = Left (OccursCheck v t)
  | otherwise = Right (singleSubst v t)

-- | Main unification algorithm
unify :: Type -> Type -> Either UnifyError Subst
unify (TyVar v) t = bindVar v t
unify t (TyVar v) = bindVar v t
unify (TyCon c1) (TyCon c2)
  | c1.name /= c2.name = Left (TypeMismatch (TyCon c1) (TyCon c2))
  | length c1.args /= length c2.args = Left (ArityMismatch c1.name (length c1.args) (length c2.args))
  | otherwise = unifyMany c1.args c2.args
unify (TyRecord r1) (TyRecord r2) = unifyRecords r1 r2
unify t1 t2 = Left (TypeMismatch t1 t2)

-- | Unify two lists of types pairwise
unifyMany :: Array Type -> Array Type -> Either UnifyError Subst
unifyMany ts1 ts2 = foldM step emptySubst (zip ts1 ts2)
  where
    step :: Subst -> Tuple Type Type -> Either UnifyError Subst
    step sub (Tuple t1 t2) = do
      s <- unify (applySubst sub t1) (applySubst sub t2)
      pure (composeSubst s sub)

-- | Unify record types
unifyRecords :: { fields :: Map.Map String Type, row :: _ }
             -> { fields :: Map.Map String Type, row :: _ }
             -> Either UnifyError Subst
unifyRecords r1 r2 =
  -- Get common keys and unify their types
  let keys1 = Map.keys r1.fields
  in foldM (unifyField r1.fields r2.fields) emptySubst keys1

-- | Helper to unify a single field
unifyField :: Map.Map String Type -> Map.Map String Type -> Subst -> String -> Either UnifyError Subst
unifyField fields1 fields2 sub k =
  case Tuple (Map.lookup k fields1) (Map.lookup k fields2) of
    Tuple (Just t1) (Just t2) ->
      case unify (applySubst sub t1) (applySubst sub t2) of
        Left err -> Left err
        Right s -> Right (composeSubst s sub)
    _ -> Right sub -- field not in both, handle with row polymorphism later
