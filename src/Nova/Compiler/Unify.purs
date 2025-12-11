module Nova.Compiler.Unify where

import Prelude
import Data.Either (Either(..))
import Data.Map as Map
import Data.Set as Set
import Data.Array as Array
import Data.Array (length, zip)
import Data.Foldable (foldM)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String (Pattern(..))
import Nova.Compiler.Types (Type(..), TVar, TCon, Record, Subst, emptySubst, singleSubst, composeSubst, applySubst, freeTypeVars, mkTyApp)

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
showType (TyCon tc) =
  if Array.length tc.args == 0
  then tc.name
  else tc.name <> " " <> String.joinWith " " (map showTypeParens tc.args)
showType (TyRecord r) =
  let fieldEntries = Array.fromFoldable (Map.toUnfoldable r.fields :: Array (Tuple String Type))
      showField (Tuple name ty) = name <> " :: " <> showType ty
      fieldStrs = map showField (Array.take 8 fieldEntries)
      suffix = if Array.length fieldEntries > 8 then ", ..." else ""
  in "{ " <> String.joinWith ", " fieldStrs <> suffix <> " }"
showType (TyApp f arg) = "(" <> showType f <> " " <> showType arg <> ")"

-- | Show type with parens for complex types
showTypeParens :: Type -> String
showTypeParens t@(TyVar _) = showType t
showTypeParens t@(TyCon tc) | Array.length tc.args == 0 = showType t
showTypeParens t@(TyRecord _) = showType t
showTypeParens t = "(" <> showType t <> ")"

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

-- | Strip module prefix from a type name (e.g., "Set.Set" -> "Set", "Data.Map.Map" -> "Map")
stripModulePrefix :: String -> String
stripModulePrefix name = case String.lastIndexOf ((Pattern ".")) name of
  Just idx -> String.drop (idx + 1) name
  Nothing -> name

-- | Check if two type names are considered equivalent
-- | List and Array are treated as equivalent since they compile to the same Elixir representation
-- | Number and Int are treated as equivalent for Nova's Elixir backend
areEquivalentTypes :: String -> String -> Boolean
areEquivalentTypes n1 n2
  | n1 == n2 = true
  -- Handle qualified vs unqualified type names (e.g., "Set.Set" == "Set")
  | stripModulePrefix n1 == stripModulePrefix n2 = true
  | n1 == "List" && n2 == "Array" = true
  | n1 == "Array" && n2 == "List" = true
  | n1 == "Number" && n2 == "Int" = true
  | n1 == "Int" && n2 == "Number" = true
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
      let unqualifiedName = case indexOf ((Pattern ".")) name of
            Just idx -> String.drop (idx + 1) name
            Nothing -> name
      in if not (unqualifiedName == name)
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
      let unqualifiedName = case indexOf ((Pattern ".")) name of
            Just idx -> String.drop (idx + 1) name
            Nothing -> name
      in if not (unqualifiedName == name)
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
  | not (length c1.args == length c2.args) = Left (ArityMismatch c1.name (length c1.args) (length c2.args))
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
-- HKT: unify type applications (m a) with (n b) by unifying m~n and a~b
unifyWithAliases aliases (TyApp f1 a1) (TyApp f2 a2) = do
  s1 <- unifyWithAliases aliases f1 f2
  s2 <- unifyWithAliases aliases (applySubst s1 a1) (applySubst s1 a2)
  pure (composeSubst s2 s1)
-- HKT: unify TyApp with TyCon - (m a) unified with (Array b)
-- Unify m with the head type constructor, then unify a with b
unifyWithAliases aliases (TyApp f a) (TyCon c)
  | length c.args > 0 =
      let headTyCon = TyCon { name: c.name, args: Array.take (length c.args - 1) c.args }
          lastArg = case Array.last c.args of
            Just arg -> arg
            Nothing -> TyCon { name: "Unit", args: [] }  -- shouldn't happen
      in do
        s1 <- unifyWithAliases aliases f headTyCon
        s2 <- unifyWithAliases aliases (applySubst s1 a) (applySubst s1 lastArg)
        pure (composeSubst s2 s1)
  | otherwise = Left (TypeMismatch (mkTyApp f a) (TyCon c))
unifyWithAliases aliases (TyCon c) (TyApp f a)
  | length c.args > 0 =
      let headTyCon = TyCon { name: c.name, args: Array.take (length c.args - 1) c.args }
          lastArg = case Array.last c.args of
            Just arg -> arg
            Nothing -> TyCon { name: "Unit", args: [] }  -- shouldn't happen
      in do
        s1 <- unifyWithAliases aliases f headTyCon
        s2 <- unifyWithAliases aliases (applySubst s1 a) (applySubst s1 lastArg)
        pure (composeSubst s2 s1)
  | otherwise = Left (TypeMismatch (TyCon c) (mkTyApp f a))
-- Allow TyApp to unify with TyVar
unifyWithAliases aliases (TyApp f a) (TyVar v) = bindVar v (mkTyApp f a)
unifyWithAliases aliases (TyVar v) (TyApp f a) = bindVar v (mkTyApp f a)
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
