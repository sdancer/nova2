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

-- | Check if two type names are considered equivalent
-- | List and Array are treated as equivalent since they compile to the same Elixir representation
areEquivalentTypes :: String -> String -> Boolean
areEquivalentTypes n1 n2
  | n1 == n2 = true
  | n1 == "List" && n2 == "Array" = true
  | n1 == "Array" && n2 == "List" = true
  | otherwise = false

-- | Check if a type name is a record type alias that should unify with records
-- | These are type aliases like "type TypeClass = { name :: String, ... }"
isRecordTypeAlias :: String -> Boolean
isRecordTypeAlias name
  | name == "TypeClass" = true
  | name == "TypeClassInstance" = true
  | name == "NewtypeDecl" = true
  | name == "FunctionDecl" = true
  | name == "FunctionDeclaration" = true
  | name == "TypeSig" = true
  | name == "DataType" = true
  | name == "DataConstructor" = true
  | name == "DataField" = true
  | name == "TypeAlias" = true
  | name == "ModuleDecl" = true
  | name == "ModuleExports" = true
  | name == "ImportDecl" = true
  | name == "InfixDecl" = true
  | name == "Constraint" = true
  | name == "ForeignImport" = true
  | name == "TypeDecl" = true
  | name == "LetBind" = true
  | name == "CaseClause" = true
  | name == "GuardedExpr" = true
  | name == "PatResult" = true
  | name == "InstantiateResult" = true
  | name == "InferResult" = true
  | name == "Env" = true
  | name == "Scheme" = true
  | name == "TVar" = true
  | name == "LiftedLambda" = true
  | otherwise = false

-- | Main unification algorithm
unify :: Type -> Type -> Either UnifyError Subst
unify (TyVar v) t = bindVar v t
unify t (TyVar v) = bindVar v t
unify (TyCon c1) (TyCon c2)
  | not (areEquivalentTypes c1.name c2.name) = Left (TypeMismatch (TyCon c1) (TyCon c2))
  | length c1.args /= length c2.args = Left (ArityMismatch c1.name (length c1.args) (length c2.args))
  | otherwise = unifyMany c1.args c2.args
-- Treat record type aliases as unifying with their record expansions
unify (TyCon c) (TyRecord r)
  | length c.args == 0 && isRecordTypeAlias c.name = Right emptySubst
  | otherwise = Left (TypeMismatch (TyCon c) (TyRecord r))
unify (TyRecord r) (TyCon c)
  | length c.args == 0 && isRecordTypeAlias c.name = Right emptySubst
  | otherwise = Left (TypeMismatch (TyRecord r) (TyCon c))
unify (TyRecord r1) (TyRecord r2) = unifyRecords r1 r2
unify t1 t2 = Left (TypeMismatch t1 t2)

-- | Helper for unifyMany - unify a single pair of types with accumulating substitution
unifyStep :: Subst -> Tuple Type Type -> Either UnifyError Subst
unifyStep sub (Tuple t1 t2) = do
  s <- unify (applySubst sub t1) (applySubst sub t2)
  pure (composeSubst s sub)

-- | Unify two lists of types pairwise
unifyMany :: Array Type -> Array Type -> Either UnifyError Subst
unifyMany ts1 ts2 = foldM unifyStep emptySubst (zip ts1 ts2)

-- | Helper to unify a single field (must be defined before unifyRecords)
unifyField :: Map.Map String Type -> Map.Map String Type -> Subst -> String -> Either UnifyError Subst
unifyField fields1 fields2 sub k =
  case Tuple (Map.lookup k fields1) (Map.lookup k fields2) of
    Tuple (Just t1) (Just t2) ->
      case unify (applySubst sub t1) (applySubst sub t2) of
        Left err -> Left err
        Right s -> Right (composeSubst s sub)
    _ -> Right sub -- field not in both, handle with row polymorphism later

-- | Unify record types
unifyRecords :: { fields :: Map.Map String Type, row :: Maybe TVar }
             -> { fields :: Map.Map String Type, row :: Maybe TVar }
             -> Either UnifyError Subst
unifyRecords r1 r2 =
  -- Get common keys and unify their types
  let keys1 = Map.keys r1.fields
  in foldM (unifyField r1.fields r2.fields) emptySubst keys1
