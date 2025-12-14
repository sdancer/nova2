module Nova.Compiler.Types where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List (List)
import Data.Foldable (foldl)
import Data.Array as Array
import Data.String as String
import Nova.Compiler.Ast (TypeExpr, Module)

-- | Type variable identified by integer id for fast comparisons
type TVar = { id :: Int, name :: String }

mkTVar :: Int -> String -> TVar
mkTVar id name = { id, name }

-- | Type constructor with zero or more parameters
type TCon = { name :: String, args :: Array Type }

-- | Normalize a type constructor name by stripping module qualifiers
-- | e.g., "Map.Map" -> "Map", "Data.Maybe.Maybe" -> "Maybe"
normalizeTypeName :: String -> String
normalizeTypeName name =
  case String.lastIndexOf (String.Pattern ".") name of
    Nothing -> name
    Just idx -> String.drop (idx + 1) name

mkTCon :: String -> Array Type -> TCon
mkTCon name args = { name: normalizeTypeName name, args }

mkTCon0 :: String -> TCon
mkTCon0 name = { name: normalizeTypeName name, args: [] }

-- | Record type with labeled fields and optional row variable
type Record = { fields :: Map String Type, row :: Maybe TVar }

-- | Core type representation
data Type
  = TyVar TVar
  | TyCon TCon
  | TyRecord Record
  | TyApp Type Type  -- ^ Type application for HKT: (TyApp m a) represents (m a)

derive instance eqType :: Eq Type

-- Convenience helpers for common built-ins
tInt :: Type
tInt = TyCon (mkTCon0 "Int")

tString :: Type
tString = TyCon (mkTCon0 "String")

tChar :: Type
tChar = TyCon (mkTCon0 "Char")

tBool :: Type
tBool = TyCon (mkTCon0 "Bool")

tList :: Type -> Type
tList el = TyCon (mkTCon "List" [el])

tArray :: Type -> Type
tArray el = TyCon (mkTCon "Array" [el])

tArrow :: Type -> Type -> Type
tArrow a b = TyCon (mkTCon "Fun" [a, b])

tMaybe :: Type -> Type
tMaybe t = TyCon (mkTCon "Maybe" [t])

tEither :: Type -> Type -> Type
tEither l r = TyCon (mkTCon "Either" [l, r])

tMap :: Type -> Type -> Type
tMap k v = TyCon (mkTCon "Map" [k, v])  -- Use simple name to match parser output

tSet :: Type -> Type
tSet t = TyCon (mkTCon "Set" [t])

tUnit :: Type
tUnit = TyCon (mkTCon0 "Unit")

tOrdering :: Type
tOrdering = TyCon (mkTCon0 "Ordering")

tNumber :: Type
tNumber = TyCon (mkTCon0 "Number")

tTokenType :: Type
tTokenType = TyCon (mkTCon0 "TokenType")

-- | Helper to construct TyApp (needed for code generation)
mkTyApp :: Type -> Type -> Type
mkTyApp f a = TyApp f a

tTuple :: Array Type -> Type
tTuple ts =
  let n = Array.length ts
      -- PureScript uses "Tuple" for 2-tuples, but we use Tuple2, Tuple3 internally
      -- Normalize to just "Tuple" for 2-tuples to match what parser generates
      name = if n == 2 then "Tuple" else "Tuple" <> show n
  in TyCon { name, args: ts }

-- | Substitution: Map from TVar.id to Type
type Subst = Map Int Type

emptySubst :: Subst
emptySubst = Map.empty

lookupSubst :: Subst -> TVar -> Type
lookupSubst sub v = case Map.lookup v.id sub of
  Just t -> t
  Nothing -> TyVar v

singleSubst :: TVar -> Type -> Subst
singleSubst v t = Map.singleton v.id t

-- | Apply substitution to a type
applySubst :: Subst -> Type -> Type
applySubst sub (TyVar v) = lookupSubst sub v
applySubst sub (TyCon c) = TyCon (c { args = map (applySubst sub) c.args })
applySubst sub (TyRecord r) =
  let fieldsList :: List (Tuple String Type)
      fieldsList = Map.toUnfoldable r.fields
      mappedFields = map (\(Tuple k v) -> Tuple k (applySubst sub v)) fieldsList
  in TyRecord (r { fields = Map.fromFoldable mappedFields })
applySubst sub (TyApp f arg) = mkTyApp (applySubst sub f) (applySubst sub arg)

-- | Compose two substitutions: s1 `compose` s2 applies s2 then s1
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 =
  let s2List :: List (Tuple Int Type)
      s2List = Map.toUnfoldable s2
      mapped = map (\(Tuple k v) -> Tuple k (applySubst s1 v)) s2List
  in Map.union s1 (Map.fromFoldable mapped)

-- | Type scheme: quantified polymorphic type (forall vars . type)
type Scheme = { vars :: Array TVar, ty :: Type }

mkScheme :: Array TVar -> Type -> Scheme
mkScheme vars ty = { vars, ty }

-- | Free type variables in a type
freeTypeVars :: Type -> Set Int
freeTypeVars (TyVar v) = Set.singleton v.id
freeTypeVars (TyCon c) = foldl (\acc t -> Set.union acc (freeTypeVars t)) Set.empty c.args
freeTypeVars (TyRecord r) = foldl (\acc t -> Set.union acc (freeTypeVars t)) Set.empty (Map.values r.fields)
freeTypeVars (TyApp f arg) = Set.union (freeTypeVars f) (freeTypeVars arg)

-- | Free type variables in a scheme
freeTypeVarsScheme :: Scheme -> Set Int
freeTypeVarsScheme s =
  let boundIds = Set.fromFoldable (map _.id s.vars)
  in Set.difference (freeTypeVars s.ty) boundIds

-- | Typing environment: maps identifiers to schemes
type Env =
  { bindings :: Map String Scheme
  , counter :: Int
  , registryLayer :: Maybe Int  -- placeholder for registry reference
  , namespace :: Maybe String
  , typeAliases :: Map String Type  -- type alias name -> expanded type (for record aliases)
  }

-- | A module that has been type-checked/inferred
-- | This wrapper ensures at the type level that only validated modules
-- | can be passed to code generation. The env contains type information
-- | collected during type checking (bindings, type aliases, etc.)
type TypedModule =
  { mod :: Module
  , env :: Env
  }

-- | Create a TypedModule from a module and its type-checked environment
mkTypedModule :: Module -> Env -> TypedModule
mkTypedModule mod env = { mod, env }

-- | Extract the module from a TypedModule
typedModuleToModule :: TypedModule -> Module
typedModuleToModule tm = tm.mod

-- | Extract the environment from a TypedModule
typedModuleEnv :: TypedModule -> Env
typedModuleEnv tm = tm.env

emptyEnv :: Env
emptyEnv =
  { bindings: builtinPrelude
  , counter: 0
  , registryLayer: Nothing
  , namespace: Nothing
  , typeAliases: Map.empty
  }

-- | Extend environment with a new binding
extendEnv :: Env -> String -> Scheme -> Env
extendEnv env name scheme = env { bindings = Map.insert name scheme env.bindings }

-- | Extend environment with a type alias (name -> expanded Type)
extendTypeAlias :: Env -> String -> Type -> Env
extendTypeAlias env name ty = env { typeAliases = Map.insert name ty env.typeAliases }

-- | Lookup a type alias by name
lookupTypeAlias :: Env -> String -> Maybe Type
lookupTypeAlias env name = Map.lookup name env.typeAliases

-- | Lookup a scheme by name
lookupEnv :: Env -> String -> Maybe Scheme
lookupEnv env name = Map.lookup name env.bindings

-- | Apply a substitution to all type schemes in an environment
applySubstToEnv :: Subst -> Env -> Env
applySubstToEnv sub env =
  env { bindings = mapBindings applyToScheme env.bindings }
  where
    applyToScheme :: Scheme -> Scheme
    applyToScheme s = s { ty = applySubst sub s.ty }
    -- Helper to map over bindings using Array conversion
    mapBindings :: (Scheme -> Scheme) -> Map String Scheme -> Map String Scheme
    mapBindings f m =
      let arr :: Array (Tuple String Scheme)
          arr = Map.toUnfoldable m
      in Array.foldl (\acc (Tuple k v) -> Map.insert k (f v) acc) Map.empty arr

-- | Generate a fresh type variable
freshVar :: Env -> String -> Tuple TVar Env
freshVar env hint =
  let v = mkTVar env.counter (hint <> show env.counter)
      env' = env { counter = env.counter + 1 }
  in Tuple v env'

-- | Free type variables in environment
freeTypeVarsEnv :: Env -> Set Int
freeTypeVarsEnv env =
  foldl (\acc s -> Set.union acc (freeTypeVarsScheme s)) Set.empty (Map.values env.bindings)

-- | Builtin prelude types and operators
builtinPrelude :: Map String Scheme
builtinPrelude = Map.fromFoldable
  -- Types
  [ Tuple "Int" (mkScheme [] tInt)
  , Tuple "String" (mkScheme [] tString)
  , Tuple "Char" (mkScheme [] tChar)
  , Tuple "Bool" (mkScheme [] tBool)
  , Tuple "Boolean" (mkScheme [] tBool)  -- PureScript uses Boolean, alias to Bool
  , Tuple "True" (mkScheme [] tBool)
  , Tuple "False" (mkScheme [] tBool)
  , Tuple "true" (mkScheme [] tBool)   -- lowercase for Elixir/JS style
  , Tuple "false" (mkScheme [] tBool)  -- lowercase for Elixir/JS style
  , Tuple "Array" (mkScheme [a] (tArray (TyVar a)))
  , Tuple "List" (mkScheme [a] (tList (TyVar a)))
  , Tuple "Cons" (mkScheme [a] (tArrow (TyVar a) (tArrow (tList (TyVar a)) (tList (TyVar a)))))
  , Tuple "Nil" (mkScheme [a] (tList (TyVar a)))
  , Tuple "Maybe" (mkScheme [a] (TyCon (mkTCon "Maybe" [TyVar a])))
  , Tuple "Either" (mkScheme [a, b] (TyCon (mkTCon "Either" [TyVar a, TyVar b])))
  -- Arithmetic operators (Int -> Int -> Int)
  , Tuple "+" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "-" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "*" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "/" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "mod" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "negate" (mkScheme [a] (tArrow (TyVar a) (TyVar a)))
  -- Bounded type class (top/bottom for Int)
  , Tuple "top" (mkScheme [] tInt)
  , Tuple "bottom" (mkScheme [] tInt)
  -- Comparison operators (polymorphic - works for Int, Char, String, etc.)
  , Tuple "<" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  , Tuple ">" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  , Tuple "<=" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  , Tuple ">=" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  -- Polymorphic equality (a -> a -> Bool)
  , Tuple "==" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  , Tuple "/=" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  -- Boolean operators
  , Tuple "&&" (mkScheme [] (tArrow tBool (tArrow tBool tBool)))
  , Tuple "||" (mkScheme [] (tArrow tBool (tArrow tBool tBool)))
  , Tuple "not" (mkScheme [] (tArrow tBool tBool))
  , Tuple "otherwise" (mkScheme [] tBool)  -- otherwise = true
  -- Semigroup append (works on String, Array, etc.)
  , Tuple "<>" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) (TyVar a))))
  -- List/Array operators
  , Tuple ":" (mkScheme [a] (tArrow (TyVar a) (tArrow (tArray (TyVar a)) (tArray (TyVar a)))))
  , Tuple "++" (mkScheme [a] (tArrow (tArray (TyVar a)) (tArrow (tArray (TyVar a)) (tArray (TyVar a)))))
  -- Function operators
  , Tuple "$" (mkScheme [a, b] (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (TyVar a) (TyVar b))))
  , Tuple "." (mkScheme [a, b, c] (tArrow (tArrow (TyVar b) (TyVar c)) (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (TyVar a) (TyVar c)))))
  , Tuple "<<<" (mkScheme [a, b, c] (tArrow (tArrow (TyVar b) (TyVar c)) (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (TyVar a) (TyVar c)))))
  , Tuple ">>>" (mkScheme [a, b, c] (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (tArrow (TyVar b) (TyVar c)) (tArrow (TyVar a) (TyVar c)))))
  -- Identity and const
  , Tuple "identity" (mkScheme [a] (tArrow (TyVar a) (TyVar a)))
  , Tuple "const" (mkScheme [a, b] (tArrow (TyVar a) (tArrow (TyVar b) (TyVar a))))
  -- Reverse function application
  , Tuple "#" (mkScheme [a, b] (tArrow (TyVar a) (tArrow (tArrow (TyVar a) (TyVar b)) (TyVar b))))


  -- Maybe functions
  , Tuple "Just" (mkScheme [a] (tArrow (TyVar a) (tMaybe (TyVar a))))
  , Tuple "Nothing" (mkScheme [a] (tMaybe (TyVar a)))
  , Tuple "maybe" (mkScheme [a, b] (tArrow (TyVar b) (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (tMaybe (TyVar a)) (TyVar b)))))
  , Tuple "fromMaybe" (mkScheme [a] (tArrow (TyVar a) (tArrow (tMaybe (TyVar a)) (TyVar a))))
  , Tuple "isJust" (mkScheme [a] (tArrow (tMaybe (TyVar a)) tBool))
  , Tuple "isNothing" (mkScheme [a] (tArrow (tMaybe (TyVar a)) tBool))

  -- Either functions
  , Tuple "Left" (mkScheme [a, b] (tArrow (TyVar a) (tEither (TyVar a) (TyVar b))))
  , Tuple "Right" (mkScheme [a, b] (tArrow (TyVar b) (tEither (TyVar a) (TyVar b))))
  , Tuple "either" (mkScheme [a, b, c] (tArrow (tArrow (TyVar a) (TyVar c)) (tArrow (tArrow (TyVar b) (TyVar c)) (tArrow (tEither (TyVar a) (TyVar b)) (TyVar c)))))
  -- Applicative/Monad pure (simplified - works for all m a)
  , Tuple "pure" (mkScheme [a, b] (tArrow (TyVar a) (TyVar b)))


  -- Tuple functions and constructors
  , Tuple "Tuple" (mkScheme [a, b] (tArrow (TyVar a) (tArrow (TyVar b) (tTuple [TyVar a, TyVar b]))))
  , Tuple "Tuple2" (mkScheme [a, b] (tArrow (TyVar a) (tArrow (TyVar b) (tTuple [TyVar a, TyVar b]))))
  , Tuple "Tuple3" (mkScheme [a, b, c] (tArrow (TyVar a) (tArrow (TyVar b) (tArrow (TyVar c) (tTuple [TyVar a, TyVar b, TyVar c])))))
  , Tuple "Tuple4" (mkScheme [a, b, c, d] (tArrow (TyVar a) (tArrow (TyVar b) (tArrow (TyVar c) (tArrow (TyVar d) (tTuple [TyVar a, TyVar b, TyVar c, TyVar d]))))))
  , Tuple "Tuple5" (mkScheme [a, b, c, d, e] (tArrow (TyVar a) (tArrow (TyVar b) (tArrow (TyVar c) (tArrow (TyVar d) (tArrow (TyVar e) (tTuple [TyVar a, TyVar b, TyVar c, TyVar d, TyVar e])))))))
  , Tuple "fst" (mkScheme [a, b] (tArrow (tTuple [TyVar a, TyVar b]) (TyVar a)))
  , Tuple "snd" (mkScheme [a, b] (tArrow (tTuple [TyVar a, TyVar b]) (TyVar b)))

  -- Show
  , Tuple "show" (mkScheme [a] (tArrow (TyVar a) tString))
  -- Effect.Console
  , Tuple "log" (mkScheme [] (tArrow tString tUnit))

  -- Sentinel for guarded functions
  , Tuple "__guarded__" (mkScheme [a] (TyVar a))
  -- Underscore accessor pattern (_.field is really a record with that field)
  , Tuple "_" (mkScheme [a] (TyVar a))

  -- NOTE: AST constructors and library functions are now derived dynamically
  -- from parsing source files via extractExports. This eliminates hundreds of
  -- lines of hardcoded, duplicated type information.
  --
  -- Functions from lib/Data/List.purs, lib/Data/Map.purs, etc. are loaded
  -- dynamically when those modules are typechecked and added to the registry.
  ]
  where
    a = mkTVar (-1) "a"
    b = mkTVar (-2) "b"
    c = mkTVar (-3) "c"
    d = mkTVar (-6) "d"
    e = mkTVar (-7) "e"

-- Internal type helpers for prelude
tSubst :: Type
tSubst = tMap tInt tType  -- Subst = Map Int Type

tEnv :: Type
tEnv = TyRecord { fields: Map.fromFoldable [Tuple "bindings" (tMap tString tScheme), Tuple "counter" tInt, Tuple "registryLayer" (tMaybe tInt), Tuple "namespace" (tMaybe tString)], row: Nothing }

tScheme :: Type
tScheme = TyRecord { fields: Map.fromFoldable [Tuple "vars" (tArray tTVar), Tuple "ty" tType], row: Nothing }

tType :: Type
tType = TyCon (mkTCon0 "Type")

tTVar :: Type
tTVar = TyRecord { fields: Map.fromFoldable [Tuple "id" tInt, Tuple "name" tString], row: Nothing }

-- TCon is actually a type alias for {name :: String, args :: Array Type}
-- We use tTCon for general use and tTConRecord for the expanded form
tTCon :: Type
tTCon = tTConRecord

tTConRecord :: Type
tTConRecord = TyRecord { fields: Map.fromFoldable [Tuple "name" tString, Tuple "args" (tArray tType)], row: Nothing }

tRecord :: Type
tRecord = TyRecord { fields: Map.fromFoldable [Tuple "fields" (tMap tString tType), Tuple "row" (tMaybe tTVar)], row: Nothing }

-- Module system types
tTypeInfo :: Type
tTypeInfo = TyRecord { fields: Map.fromFoldable [Tuple "arity" tInt, Tuple "constructors" (tArray tString)], row: Nothing }

tTypeAliasInfo :: Type
tTypeAliasInfo = TyRecord { fields: Map.fromFoldable [Tuple "params" (tArray tString), Tuple "body" tTypeExpr], row: Nothing }

tModuleExports :: Type
tModuleExports = TyRecord { fields: Map.fromFoldable
  [ Tuple "types" (tMap tString tTypeInfo)
  , Tuple "constructors" (tMap tString tScheme)
  , Tuple "values" (tMap tString tScheme)
  , Tuple "typeAliases" (tMap tString tTypeAliasInfo)
  ], row: Nothing }

tModuleRegistry :: Type
tModuleRegistry = tMap tString tModuleExports

tModule :: Type
tModule = TyCon (mkTCon0 "Module")

tTypedModule :: Type
tTypedModule = TyRecord { fields: Map.fromFoldable [Tuple "mod" tModule, Tuple "env" tEnv], row: Nothing }

tPattern :: Type
tPattern = TyCon (mkTCon0 "Pattern")

tExpr :: Type
tExpr = TyCon (mkTCon0 "Expr")

tLiteral :: Type
tLiteral = TyCon (mkTCon0 "Literal")

tTypeExpr :: Type
tTypeExpr = TyCon (mkTCon0 "TypeExpr")

tLetBind :: Type
tLetBind = TyRecord { fields: Map.fromFoldable
  [ Tuple "pattern" tPattern
  , Tuple "value" tExpr
  , Tuple "typeAnn" (tMaybe tTypeExpr)
  ], row: Nothing }

tCaseClause :: Type
tCaseClause = TyRecord { fields: Map.fromFoldable
  [ Tuple "pattern" tPattern
  , Tuple "guard" (tMaybe tExpr)
  , Tuple "body" tExpr
  ], row: Nothing }

tDeclaration :: Type
tDeclaration = TyCon (mkTCon0 "Declaration")

tFunctionDecl :: Type
tFunctionDecl = TyRecord { fields: Map.fromFoldable
  [ Tuple "name" tString
  , Tuple "parameters" (tList tPattern)
  , Tuple "body" tExpr
  , Tuple "guards" (tList tGuardedExprRec)
  , Tuple "typeSignature" (tMaybe tTypeSigRec)
  ], row: Nothing }

-- Record type versions for nested use (avoiding circular definition issues)
tGuardedExprRec :: Type
tGuardedExprRec = TyRecord { fields: Map.fromFoldable
  [ Tuple "guards" (tList tGuardClause)
  , Tuple "body" tExpr
  ], row: Nothing }

tTypeSigRec :: Type
tTypeSigRec = TyRecord { fields: Map.fromFoldable
  [ Tuple "name" tString
  , Tuple "typeVars" (tArray tString)
  , Tuple "constraints" (tArray tConstraint)
  , Tuple "ty" tTypeExpr
  ], row: Nothing }

tTypeSig :: Type
tTypeSig = tTypeSigRec

tDataType :: Type
tDataType = TyRecord { fields: Map.fromFoldable
  [ Tuple "name" tString
  , Tuple "typeVars" (tList tString)
  , Tuple "constructors" (tList tDataConstructor)
  ], row: Nothing }

tDataConstructor :: Type
tDataConstructor = TyRecord { fields: Map.fromFoldable
  [ Tuple "name" tString
  , Tuple "fields" (tList tDataField)
  , Tuple "isRecord" tBool
  ], row: Nothing }

tDataField :: Type
tDataField = TyRecord { fields: Map.fromFoldable
  [ Tuple "label" tString
  , Tuple "ty" tTypeExpr
  ], row: Nothing }

tTypeAlias :: Type
tTypeAlias = TyRecord { fields: Map.fromFoldable
  [ Tuple "name" tString
  , Tuple "typeVars" (tArray tString)
  , Tuple "ty" tTypeExpr
  ], row: Nothing }

tModuleDecl :: Type
tModuleDecl = TyRecord { fields: Map.fromFoldable
  [ Tuple "name" tString
  , Tuple "declarations" (tList tDeclaration)
  ], row: Nothing }

tImportDecl :: Type
tImportDecl = TyRecord { fields: Map.fromFoldable
  [ Tuple "moduleName" tString
  , Tuple "alias" (tMaybe tString)
  , Tuple "items" (tArray tImportItem)
  , Tuple "hiding" tBool
  ], row: Nothing }

tDoStatement :: Type
tDoStatement = TyCon (mkTCon0 "DoStatement")

tGuardedExpr :: Type
tGuardedExpr = tGuardedExprRec

tGuardClause :: Type
tGuardClause = TyCon (mkTCon0 "GuardClause")

tTypeClass :: Type
tTypeClass = TyRecord { fields: Map.fromFoldable
  [ Tuple "name" tString
  , Tuple "typeVars" (tArray tString)
  , Tuple "methods" (tArray tTypeSig)
  , Tuple "kind" (tMaybe tString)
  ], row: Nothing }

tTypeClassInstance :: Type
tTypeClassInstance = TyRecord { fields: Map.fromFoldable
  [ Tuple "className" tString
  , Tuple "ty" tTypeExpr
  , Tuple "methods" (tArray tFunctionDecl)
  , Tuple "derived" tBool
  ], row: Nothing }

tInfixDecl :: Type
tInfixDecl = TyRecord { fields: Map.fromFoldable
  [ Tuple "associativity" tString
  , Tuple "precedence" tInt
  , Tuple "operator" tString
  ], row: Nothing }

tConstraint :: Type
tConstraint = TyRecord { fields: Map.fromFoldable
  [ Tuple "className" tString
  , Tuple "types" (tArray tTypeExpr)
  ], row: Nothing }

tImportItem :: Type
tImportItem = TyCon (mkTCon0 "ImportItem")

tImportSpec :: Type
tImportSpec = TyCon (mkTCon0 "ImportSpec")

tAssociativity :: Type
tAssociativity = TyCon (mkTCon0 "Associativity")

tForeignImport :: Type
tForeignImport = TyRecord { fields: Map.fromFoldable
  [ Tuple "moduleName" tString
  , Tuple "functionName" tString
  , Tuple "ty" tTypeExpr
  ], row: Nothing }

tNewtypeDecl :: Type
tNewtypeDecl = TyRecord { fields: Map.fromFoldable
  [ Tuple "name" tString
  , Tuple "typeVars" (tArray tString)
  , Tuple "constructor" tString
  , Tuple "wrappedType" tTypeExpr
  ], row: Nothing }

tTypeDecl :: Type
tTypeDecl = TyRecord { fields: Map.fromFoldable
  [ Tuple "name" tString
  , Tuple "ty" tTypeExpr
  ], row: Nothing }

tUnifyError :: Type
tUnifyError = TyCon (mkTCon0 "UnifyError")

tTCError :: Type
tTCError = TyCon (mkTCon0 "TCError")

tPatResult :: Type
tPatResult = TyRecord { fields: Map.fromFoldable
  [ Tuple "env" tEnv
  , Tuple "sub" tSubst
  ], row: Nothing }

tInstantiateResult :: Type
tInstantiateResult = TyRecord { fields: Map.fromFoldable
  [ Tuple "ty" tType
  , Tuple "env" tEnv
  ], row: Nothing }

tInferResult :: Type
tInferResult = TyRecord { fields: Map.fromFoldable
  [ Tuple "ty" tType
  , Tuple "env" tEnv
  , Tuple "sub" tSubst
  ], row: Nothing }

-- | Type alias info for parameterized type aliases
-- | e.g., type ParseResult a = Either String (Tuple a (Array Token))
-- | has params = ["a"], body = TyExprApp ...
type TypeAliasInfo = { params :: Array String, body :: TypeExpr }

-- | Helper to get params from TypeAliasInfo (aids self-hosted compiler type inference)
-- Uses field access instead of pattern matching to avoid record inference issues
getAliasInfoParams :: TypeAliasInfo -> Array String
getAliasInfoParams info = info.params

-- | Helper to get body from TypeAliasInfo (aids self-hosted compiler type inference)
-- Uses field access instead of pattern matching to avoid record inference issues
getAliasInfoBody :: TypeAliasInfo -> TypeExpr
getAliasInfoBody info = info.body

-- | Module exports: what a module makes available to importers
-- | This is the core data structure for the module system
type ModuleExports =
  { types :: Map String TypeInfo           -- Type constructors (e.g., Maybe, Either)
  , constructors :: Map String Scheme      -- Data constructors (e.g., Just, Nothing, Left, Right)
  , values :: Map String Scheme            -- Functions and values
  , typeAliases :: Map String TypeAliasInfo  -- Type aliases (surface syntax)
  , expandedTypeAliases :: Map String Type   -- Expanded type aliases for unification (internal Type)
  }

-- | Information about an exported type
type TypeInfo =
  { arity :: Int                           -- Number of type parameters
  , constructors :: Array String           -- Names of data constructors
  }

-- | Empty module exports
emptyExports :: ModuleExports
emptyExports =
  { types: Map.empty
  , constructors: Map.empty
  , values: Map.empty
  , typeAliases: Map.empty
  , expandedTypeAliases: Map.empty
  }

-- | Prelude module exports
-- | Contains ONLY primitive types and operators that cannot be derived from source.
-- | All stdlib functions (map, foldl, fromMaybe, etc.) are loaded dynamically
-- | from lib/*.purs files via extractExports.
preludeExports :: ModuleExports
preludeExports =
  { types: Map.fromFoldable
      [ Tuple "Boolean" { arity: 0, constructors: [] }
      , Tuple "Int" { arity: 0, constructors: [] }
      , Tuple "String" { arity: 0, constructors: [] }
      , Tuple "Char" { arity: 0, constructors: [] }
      , Tuple "Number" { arity: 0, constructors: [] }
      , Tuple "Array" { arity: 1, constructors: [] }
      , Tuple "Unit" { arity: 0, constructors: ["Unit"] }
      , Tuple "Ordering" { arity: 0, constructors: ["LT", "EQ", "GT"] }
      ]
  , constructors: Map.fromFoldable
      [ Tuple "Unit" (mkScheme [] tUnit)
      , Tuple "LT" (mkScheme [] tOrdering)
      , Tuple "EQ" (mkScheme [] tOrdering)
      , Tuple "GT" (mkScheme [] tOrdering)
      ]
  , values: Map.fromFoldable
      -- Primitive operators (these are built into the runtime, not defined in source)
      -- Eq operators
      [ Tuple "==" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
      , Tuple "/=" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
      -- Ord operators
      , Tuple "<" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
      , Tuple ">" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
      , Tuple "<=" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
      , Tuple ">=" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
      -- Arithmetic operators
      , Tuple "+" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
      , Tuple "-" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
      , Tuple "*" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
      , Tuple "/" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
      , Tuple "mod" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
      -- Boolean operators
      , Tuple "&&" (mkScheme [] (tArrow tBool (tArrow tBool tBool)))
      , Tuple "||" (mkScheme [] (tArrow tBool (tArrow tBool tBool)))
      -- Semigroup operator
      , Tuple "<>" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) (TyVar a))))
      -- Application operators
      , Tuple "$" (mkScheme [a, b] (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (TyVar a) (TyVar b))))
      , Tuple "#" (mkScheme [a, b] (tArrow (TyVar a) (tArrow (tArrow (TyVar a) (TyVar b)) (TyVar b))))
      -- Composition operators
      , Tuple "<<<" (mkScheme [a, b, c] (tArrow (tArrow (TyVar b) (TyVar c)) (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (TyVar a) (TyVar c)))))
      , Tuple ">>>" (mkScheme [a, b, c] (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (tArrow (TyVar b) (TyVar c)) (tArrow (TyVar a) (TyVar c)))))
      -- Applicative/Functor operators (primitive - implementation varies by type)
      , Tuple "<$>" (mkScheme [a, b, c, d] (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (TyVar c) (TyVar d))))
      , Tuple "*>" (mkScheme [a, b] (tArrow (TyVar a) (tArrow (TyVar b) (TyVar b))))
      , Tuple "<*" (mkScheme [a, b] (tArrow (TyVar a) (tArrow (TyVar b) (TyVar a))))
      -- Bind operator (primitive - monad bind)
      , Tuple ">>=" (mkScheme [a, b, c] (tArrow (TyVar a) (tArrow (tArrow (TyVar b) (TyVar c)) (TyVar c))))
      -- Guard sentinel
      , Tuple "otherwise" (mkScheme [] tBool)
      ]
      -- NOTE: All stdlib functions (identity, const, map, foldl, fromMaybe, etc.)
      -- are now loaded dynamically from lib/Nova/Prelude.purs, lib/Data/Array.purs,
      -- and other lib/*.purs files via extractExports.
  , typeAliases: Map.empty
  , expandedTypeAliases: Map.empty
  }
  where
    a = mkTVar (-1) "a"
    b = mkTVar (-2) "b"
    c = mkTVar (-3) "c"
    d = mkTVar (-6) "d"

-- | Effect.Console module exports
-- | NOTE: All functions (log, logShow, error, etc.) are now loaded dynamically
-- | from lib/Effect/Console.purs via extractExports.
effectConsoleExports :: ModuleExports
effectConsoleExports = emptyExports

-- | Default module registry with standard library modules
-- | Nova.Compiler.* modules are NOT included here - they are dynamically
-- | registered by Regenerate.purs during compilation
defaultRegistry :: ModuleRegistry
defaultRegistry = Map.fromFoldable
  [ Tuple "Prelude" preludeExports
  , Tuple "Effect.Console" effectConsoleExports
  ]

-- | Module registry: maps module names to their exports
type ModuleRegistry = Map String ModuleExports

-- | Empty module registry
emptyRegistry :: ModuleRegistry
emptyRegistry = Map.empty

-- | Look up a module's exports
lookupModule :: ModuleRegistry -> String -> Maybe ModuleExports
lookupModule reg name = Map.lookup name reg

-- | Register a module's exports
registerModule :: ModuleRegistry -> String -> ModuleExports -> ModuleRegistry
registerModule reg name exports = Map.insert name exports reg

-- | Merge exports into environment bindings
-- | Used when processing imports
mergeExportsToEnv :: Env -> ModuleExports -> Env
mergeExportsToEnv env exports =
  let -- Add constructors
      ctorList = Map.toUnfoldable exports.constructors
      env1 = Array.foldl (\e (Tuple name scheme) -> extendEnv e name scheme) env ctorList
      -- Add values
      valList = Map.toUnfoldable exports.values
      env2 = Array.foldl (\e (Tuple name scheme) -> extendEnv e name scheme) env1 valList
      -- Add expanded type aliases (for unification to expand record type aliases from imports)
      env3 = env2 { typeAliases = Map.union exports.expandedTypeAliases env2.typeAliases }
  in env3

-- | Merge exports into environment with a module prefix
-- | Used when importing with "as" alias: import Data.List as List
-- | Adds bindings like "List.reverse" for qualified access
mergeExportsToEnvWithPrefix :: Env -> ModuleExports -> String -> Env
mergeExportsToEnvWithPrefix env exports prefix =
  let -- Add constructors with prefix
      ctorList = Map.toUnfoldable exports.constructors
      env1 = Array.foldl (\e (Tuple name scheme) -> extendEnv e (prefix <> "." <> name) scheme) env ctorList
      -- Add values with prefix
      valList = Map.toUnfoldable exports.values
      env2 = Array.foldl (\e (Tuple name scheme) -> extendEnv e (prefix <> "." <> name) scheme) env1 valList
      -- Add expanded type aliases with prefix
      aliasList = Map.toUnfoldable exports.expandedTypeAliases :: Array (Tuple String Type)
      newAliases = Array.foldl (\m (Tuple name ty) -> Map.insert (prefix <> "." <> name) ty m) env2.typeAliases aliasList
      env3 = env2 { typeAliases = newAliases }
  in env3

-- | Merge specific items from exports into environment
-- | items: list of names to import
mergeSelectedExports :: Env -> ModuleExports -> Array String -> Env
mergeSelectedExports env exports items =
  Array.foldl addItem env items
  where
    addItem e name =
      -- First check expanded type aliases (e.g., Constraint, DataField)
      let e' = case Map.lookup name exports.expandedTypeAliases of
            Just ty -> e { typeAliases = Map.insert name ty e.typeAliases }
            Nothing -> e
      in case Map.lookup name exports.constructors of
        Just scheme -> extendEnv e' name scheme
        Nothing -> case Map.lookup name exports.values of
          Just scheme -> extendEnv e' name scheme
          Nothing -> e'  -- Item not found, skip (might be just a type alias)

-- | Merge a type and its constructors into environment
mergeTypeExport :: Env -> ModuleExports -> String -> Array String -> Env
mergeTypeExport env exports typeName ctorNames =
  -- Add the type constructors
  Array.foldl addCtor env ctorNames
  where
    addCtor e ctorName =
      case Map.lookup ctorName exports.constructors of
        Just scheme -> extendEnv e ctorName scheme
        Nothing -> e

-- ============================================================================
-- Pretty Printing Types (PureScript syntax)
-- ============================================================================

-- | Pretty-print a type in PureScript syntax
showType :: Type -> String
showType (TyVar v) = v.name
showType (TyCon c) = showTyCon c
showType (TyRecord r) = showRecord r
showType (TyApp f arg) = "(" <> showType f <> " " <> showType arg <> ")"

-- | Pretty-print a type constructor
showTyCon :: TCon -> String
showTyCon c = case c.name of
  -- Function type: a -> b
  "Fun" -> case c.args of
    [arg, ret] -> showTypeArg arg <> " -> " <> showType ret
    _ -> c.name <> " " <> String.joinWith " " (map showTypeArg c.args)
  -- Array type: Array a
  "Array" -> case c.args of
    [elem] -> "Array " <> showTypeArg elem
    _ -> "Array"
  -- List type: List a
  "List" -> case c.args of
    [elem] -> "List " <> showTypeArg elem
    _ -> "List"
  -- Maybe type: Maybe a
  "Maybe" -> case c.args of
    [elem] -> "Maybe " <> showTypeArg elem
    _ -> "Maybe"
  -- Either type: Either a b
  "Either" -> case c.args of
    [l, r] -> "Either " <> showTypeArg l <> " " <> showTypeArg r
    _ -> "Either"
  -- Map type: Map k v
  "Map" -> case c.args of
    [k, v] -> "Map " <> showTypeArg k <> " " <> showTypeArg v
    _ -> "Map"
  -- Set type: Set a
  "Set" -> case c.args of
    [elem] -> "Set " <> showTypeArg elem
    _ -> "Set"
  -- Tuple types: Tuple a b or (a, b, c)
  "Tuple" -> case c.args of
    [] -> "Unit"
    [_] -> "Tuple " <> String.joinWith " " (map showTypeArg c.args)
    _ -> "(" <> String.joinWith ", " (map showType c.args) <> ")"
  -- Multi-element tuples
  name | String.take 5 name == "Tuple" ->
    "(" <> String.joinWith ", " (map showType c.args) <> ")"
  -- No-arg type constructor
  _ | Array.null c.args -> c.name
  -- Type constructor with args
  _ -> c.name <> " " <> String.joinWith " " (map showTypeArg c.args)

-- | Pretty-print a type as an argument (wrap complex types in parens)
showTypeArg :: Type -> String
showTypeArg ty = case ty of
  TyVar v -> v.name
  TyCon c -> case c.name of
    -- Function types need parens when used as arguments
    "Fun" -> "(" <> showType ty <> ")"
    -- Type constructors with arguments need parens
    _ | not (Array.null c.args) -> "(" <> showType ty <> ")"
    -- Simple type constructors don't need parens
    _ -> c.name
  TyRecord r -> showRecord r
  TyApp _ _ -> "(" <> showType ty <> ")"  -- Type applications need parens

-- | Pretty-print a record type
showRecord :: Record -> String
showRecord r =
  let fields = Map.toUnfoldable r.fields :: Array (Tuple String Type)
      fieldStrs = map (\(Tuple name ty) -> name <> " :: " <> showType ty) fields
      inner = String.joinWith ", " fieldStrs
  in case r.row of
    Nothing -> "{ " <> inner <> " }"
    Just rv -> "{ " <> inner <> " | " <> rv.name <> " }"

-- | Pretty-print a type scheme
showScheme :: Scheme -> String
showScheme s =
  let tyStr = showType s.ty
  in if Array.null s.vars
     then tyStr
     else "forall " <> String.joinWith " " (map _.name s.vars) <> ". " <> tyStr
