module Nova.Compiler.Unify where

import Prelude
import Data.Either (Either(..))
import Data.Map as Map
import Data.Set as Set
import Data.Array (zip, length, take, fromFoldable) as Array
import Data.Array (length, zip)
import Data.Foldable (foldM)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Nova.Compiler.Types (Type(..), TVar, TCon, Record, Subst, emptySubst, singleSubst, composeSubst, applySubst, freeTypeVars)

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
showType (TyRecord r) =
  let fieldNames = Array.fromFoldable (Map.keys r.fields)
      fieldStr = String.joinWith ", " (Array.take 5 fieldNames)
      suffix = if Array.length fieldNames > 5 then ", ..." else ""
  in "{" <> fieldStr <> suffix <> "}"

-- | Occurs check: does variable v occur in type t?
occurs :: TVar -> Type -> Boolean
occurs v t = Set.member v.id (freeTypeVars t)

-- | Bind a type variable to a type, with occurs check
bindVar :: TVar -> Type -> Either UnifyError Subst
bindVar v t =
  if isSameVar v t
  then Right emptySubst
  else if occurs v t
       then Left (OccursCheck v t)
       else Right (singleSubst v t)
  where
    isSameVar tv ty = case ty of
      TyVar tv2 -> tv.id == tv2.id
      _ -> false

-- | Check if two type names are considered equivalent
-- | List and Array are treated as equivalent since they compile to the same Elixir representation
areEquivalentTypes :: String -> String -> Boolean
areEquivalentTypes n1 n2
  | n1 == n2 = true
  | n1 == "List" && n2 == "Array" = true
  | n1 == "Array" && n2 == "List" = true
  | otherwise = false

-- | Check if a type name is a known record type alias using a map lookup
-- | The map should contain all record type aliases (name -> expanded type)
isRecordTypeAliasInMap :: Map.Map String Type -> String -> Boolean
isRecordTypeAliasInMap aliasMap name =
  case Map.lookup name aliasMap of
    Just (TyRecord _) -> true
    Just _ -> false  -- Alias exists but expands to non-record
    Nothing ->
      -- Also check unqualified name
      let unqualifiedName = case indexOf (String.Pattern ".") name of
            Just idx -> String.drop (idx + 1) name
            Nothing -> name
      in if unqualifiedName /= name
         then case Map.lookup unqualifiedName aliasMap of
                Just (TyRecord _) -> true
                _ -> false
         else false
  where
    indexOf p s = String.lastIndexOf p s

-- | Type alias map for record type aliases
type TypeAliasMap = Map.Map String Type

-- | Look up a record type alias and extract the Record (fields and row)
lookupRecordAlias :: Map.Map String Type -> String -> Maybe Record
lookupRecordAlias aliasMap name =
  case Map.lookup name aliasMap of
    Just (TyRecord r) -> Just r
    Just _ -> Nothing  -- Alias exists but expands to non-record
    Nothing ->
      -- Also check unqualified name
      let unqualifiedName = case indexOf (String.Pattern ".") name of
            Just idx -> String.drop (idx + 1) name
            Nothing -> name
      in if unqualifiedName /= name
         then case Map.lookup unqualifiedName aliasMap of
                Just (TyRecord r) -> Just r
                _ -> Nothing
         else Nothing
  where
    indexOf p s = String.lastIndexOf p s

-- | Main unification algorithm with type alias map
unifyWithAliases :: TypeAliasMap -> Type -> Type -> Either UnifyError Subst
unifyWithAliases aliases (TyVar v) t = bindVar v t
unifyWithAliases aliases t (TyVar v) = bindVar v t
unifyWithAliases aliases (TyCon c1) (TyCon c2)
  | not (areEquivalentTypes c1.name c2.name) = Left (TypeMismatch (TyCon c1) (TyCon c2))
  | length c1.args /= length c2.args = Left (ArityMismatch c1.name (length c1.args) (length c2.args))
  | otherwise = unifyManyWithAliases aliases c1.args c2.args
-- Treat record type aliases as unifying with their record expansions
unifyWithAliases aliases (TyCon c) (TyRecord r)
  | length c.args == 0 = case lookupRecordAlias aliases c.name of
      Just aliasRecord -> unifyRecordsWithAliases aliases aliasRecord r
      Nothing -> Left (TypeMismatch (TyCon c) (TyRecord r))
  | otherwise = Left (TypeMismatch (TyCon c) (TyRecord r))
unifyWithAliases aliases (TyRecord r) (TyCon c)
  | length c.args == 0 = case lookupRecordAlias aliases c.name of
      Just aliasRecord -> unifyRecordsWithAliases aliases aliasRecord r
      Nothing -> Left (TypeMismatch (TyRecord r) (TyCon c))
  | otherwise = Left (TypeMismatch (TyRecord r) (TyCon c))
unifyWithAliases aliases (TyRecord r1) (TyRecord r2) = unifyRecordsWithAliases aliases r1 r2
unifyWithAliases aliases t1 t2 = Left (TypeMismatch t1 t2)

-- | Main unification algorithm (backward compatible - uses empty alias map)
unify :: Type -> Type -> Either UnifyError Subst
unify = unifyWithAliases Map.empty

-- | Helper for unifyManyWithAliases - unify a single pair of types with accumulating substitution
unifyStepWithAliases :: TypeAliasMap -> Subst -> Tuple Type Type -> Either UnifyError Subst
unifyStepWithAliases aliases sub (Tuple t1 t2) = do
  s <- unifyWithAliases aliases (applySubst sub t1) (applySubst sub t2)
  pure (composeSubst s sub)

-- | Unify two lists of types pairwise (with alias map)
unifyManyWithAliases :: TypeAliasMap -> Array Type -> Array Type -> Either UnifyError Subst
unifyManyWithAliases aliases ts1 ts2 = foldM (unifyStepWithAliases aliases) emptySubst (zip ts1 ts2)

-- | Helper to unify a single field (with alias map)
unifyFieldWithAliases :: TypeAliasMap -> Map.Map String Type -> Map.Map String Type -> Subst -> String -> Either UnifyError Subst
unifyFieldWithAliases aliases fields1 fields2 sub k =
  case Tuple (Map.lookup k fields1) (Map.lookup k fields2) of
    Tuple (Just t1) (Just t2) ->
      case unifyWithAliases aliases (applySubst sub t1) (applySubst sub t2) of
        Left err -> Left err
        Right s -> Right (composeSubst s sub)
    _ -> Right sub -- field not in both, handle with row polymorphism later

-- | Unify record types (with alias map)
unifyRecordsWithAliases :: TypeAliasMap -> { fields :: Map.Map String Type, row :: Maybe TVar }
             -> { fields :: Map.Map String Type, row :: Maybe TVar }
             -> Either UnifyError Subst
unifyRecordsWithAliases aliases r1 r2 =
  -- Get common keys and unify their types
  let keys1 = Map.keys r1.fields
  in foldM (unifyFieldWithAliases aliases r1.fields r2.fields) emptySubst keys1

-- | Helper for unifyMany - unify a single pair of types with accumulating substitution (backward compat)
unifyStep :: Subst -> Tuple Type Type -> Either UnifyError Subst
unifyStep = unifyStepWithAliases Map.empty

-- | Unify two lists of types pairwise (backward compat)
unifyMany :: Array Type -> Array Type -> Either UnifyError Subst
unifyMany = unifyManyWithAliases Map.empty

-- | Helper to unify a single field (backward compat)
unifyField :: Map.Map String Type -> Map.Map String Type -> Subst -> String -> Either UnifyError Subst
unifyField = unifyFieldWithAliases Map.empty

-- | Unify record types (backward compat)
unifyRecords :: { fields :: Map.Map String Type, row :: Maybe TVar }
             -> { fields :: Map.Map String Type, row :: Maybe TVar }
             -> Either UnifyError Subst
unifyRecords = unifyRecordsWithAliases Map.empty
