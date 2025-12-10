-- | Import processing module for Nova compiler
-- |
-- | This module handles all aspects of import processing:
-- | 1. Semantic resolution of imports against a module registry
-- | 2. Building the environment with imported bindings
-- | 3. Handling qualified vs unqualified imports
-- | 4. Type alias resolution for imported types
module Nova.Compiler.ImportProcessor
  ( processImports
  , processImportDecl
  , importItem
  , mergeExportsWithTypeAliases
  , expandModuleAliases
  , ResolvedImports
  , resolveImports
  , isRecordType
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))

import Nova.Compiler.Ast (Declaration(..), ImportDeclaration, ImportItem(..), ImportSpec(..), TypeExpr(..))
import Nova.Compiler.Types (Type(..), Env, Scheme(..), ModuleExports, ModuleRegistry, TypeAliasInfo, TypeInfo, tArrow)
import Nova.Compiler.Types as Types

-- | Resolved import mapping: maps imported names to their source module
-- | This is used by CodeGen to generate qualified calls
type ResolvedImports = Map.Map String String

-- | Local array map wrapper (uses Prelude map in real PureScript, resolved to Array.map by self-hosted compiler)
arrayMap :: forall a b. (a -> b) -> Array a -> Array b
arrayMap = map

-- | Local list map wrapper (uses Prelude map in real PureScript, resolved to List.map by self-hosted compiler)
listMap :: forall a b. (a -> b) -> List a -> List b
listMap = map

-- | Map over a Map, keeping only Just values (local impl since Map.mapMaybe not exported)
-- | Specialized for String keys since that's what we use in this module
mapMapMaybe :: forall v w. (v -> Maybe w) -> Map.Map String v -> Map.Map String w
mapMapMaybe f m =
  let pairs :: Array (Tuple String v)
      pairs = Map.toUnfoldable m
      filtered = Array.mapMaybe (\(Tuple k v) -> case f v of
        Just w -> Just (Tuple k w)
        Nothing -> Nothing) pairs
  in Map.fromFoldable filtered

-- | Check if a Type is a record type (TyRecord)
isRecordType :: Type -> Boolean
isRecordType (TyRecord _) = true
isRecordType _ = false

-- | Helper functions to extract fields from ModuleExports
-- | These are top-level to help the self-hosted compiler with type inference
getExportsExpandedTypeAliases :: ModuleExports -> Map.Map String Type
getExportsExpandedTypeAliases exports = exports.expandedTypeAliases

getExportsTypeAliases :: ModuleExports -> Map.Map String TypeAliasInfo
getExportsTypeAliases exports = exports.typeAliases

getExportsValues :: ModuleExports -> Map.Map String Scheme
getExportsValues exports = exports.values

getExportsConstructors :: ModuleExports -> Map.Map String Scheme
getExportsConstructors exports = exports.constructors

getExportsTypes :: ModuleExports -> Map.Map String TypeInfo
getExportsTypes exports = exports.types

-- | Process all import declarations, building up the environment
processImports :: ModuleRegistry -> Env -> Array Declaration -> Env
processImports registry env decls = Array.foldl processDecl env decls
  where
  processDecl e (DeclImport imp) = processImportDecl registry e imp
  processDecl e _ = e

-- | Process a single import declaration
-- | Handles both qualified and unqualified imports
processImportDecl :: ModuleRegistry -> Env -> ImportDeclaration -> Env
processImportDecl registry env imp =
  case Types.lookupModule registry imp.moduleName of
    Nothing -> env  -- Module not found, skip (could add warning)
    Just exports ->
      -- Use helper functions to extract fields (helps self-hosted compiler with type inference)
      let exportsExpandedTypeAliases :: Map.Map String Type
          exportsExpandedTypeAliases = getExportsExpandedTypeAliases exports
          exportsTypeAliases :: Map.Map String TypeAliasInfo
          exportsTypeAliases = getExportsTypeAliases exports
          -- Apply alias prefix if present, or use module name as prefix
          envWithQualified :: Env
          envWithQualified = case imp.alias of
            Just alias -> Types.mergeExportsToEnvWithPrefix env exports alias
            Nothing ->
              -- Extract last component of module name for default qualified access
              -- e.g., Data.List -> List
              let lastPart = case String.lastIndexOf (String.Pattern ".") imp.moduleName of
                    Nothing -> imp.moduleName
                    Just idx -> String.drop (idx + 1) imp.moduleName
              in Types.mergeExportsToEnvWithPrefix env exports lastPart
      in
        if imp.hiding then
          -- Import everything EXCEPT the listed items (qualified access already added)
          mergeExportsWithTypeAliases envWithQualified exports
        else if List.null imp.items then
          -- Empty import list
          case imp.alias of
            Just _ ->
              -- With alias and no items: qualified access + type aliases for unification
              -- Only add record type aliases, not values (which would conflict)
              -- Use pre-expanded type aliases from exports if available
              let moduleAliases :: Map.Map String Type
                  moduleAliases = if Map.isEmpty exportsExpandedTypeAliases
                                  then expandModuleAliases exportsTypeAliases
                                  else exportsExpandedTypeAliases
                  addIfRecordAlias :: Env -> Tuple String Type -> Env
                  addIfRecordAlias e (Tuple name ty) =
                    if isRecordType ty then extendTypeAlias e name ty else e
              in Array.foldl addIfRecordAlias envWithQualified (Map.toUnfoldable moduleAliases)
            Nothing ->
              -- No alias and no items: qualified + unqualified (e.g., import X)
              mergeExportsWithTypeAliases envWithQualified exports
        else
          -- Import only specified items unqualified (qualified access already added)
          List.foldl (importItem exports) envWithQualified imp.items

-- | Merge all exports from a module into the environment
-- | Also adds type aliases for record types to enable unification
mergeExportsWithTypeAliases :: Env -> ModuleExports -> Env
mergeExportsWithTypeAliases env exports =
  -- Use helper functions to extract fields (helps self-hosted compiler with type inference)
  let exportsExpandedTypeAliases :: Map.Map String Type
      exportsExpandedTypeAliases = getExportsExpandedTypeAliases exports
      exportsTypeAliases :: Map.Map String TypeAliasInfo
      exportsTypeAliases = getExportsTypeAliases exports
      env1 :: Env
      env1 = Types.mergeExportsToEnv env exports
      -- Use pre-expanded type aliases from exports if available
      moduleAliases :: Map.Map String Type
      moduleAliases = if Map.isEmpty exportsExpandedTypeAliases
                      then expandModuleAliases exportsTypeAliases
                      else exportsExpandedTypeAliases
      addIfRecordAlias :: Env -> Tuple String Type -> Env
      addIfRecordAlias e (Tuple name ty) =
        if isRecordType ty then extendTypeAlias e name ty else e
      env2 :: Env
      env2 = Array.foldl addIfRecordAlias env1 (Map.toUnfoldable moduleAliases)
  in env2

-- | Convert module's TypeAliasInfo map to simple alias map (Map String Type)
-- | Uses module's own aliases to resolve nested references
expandModuleAliases :: Map.Map String TypeAliasInfo -> Map.Map String Type
expandModuleAliases aliasInfos =
  let -- First pass: convert without resolving nested aliases
      initial = mapMapMaybe (\info ->
        if Array.null info.params
        then Just (typeExprToType Map.empty info.body)
        else Nothing) aliasInfos
      -- Second pass: expand any nested references within each alias
      pass2 = mapMapMaybe (\info ->
        if Array.null info.params
        then Just (typeExprToTypeWithAllAliases initial aliasInfos Map.empty info.body)
        else Nothing) aliasInfos
      -- Third pass: expand again using pass2 results for deeper nesting
      pass3 = mapMapMaybe (\info ->
        if Array.null info.params
        then Just (typeExprToTypeWithAllAliases pass2 aliasInfos Map.empty info.body)
        else Nothing) aliasInfos
  in pass3

-- | Import a single item from module exports
importItem :: ModuleExports -> Env -> ImportItem -> Env
importItem exports env item =
  -- Use helper functions to extract fields (helps self-hosted compiler with type inference)
  let exportsExpandedTypeAliases :: Map.Map String Type
      exportsExpandedTypeAliases = getExportsExpandedTypeAliases exports
      exportsTypeAliases :: Map.Map String TypeAliasInfo
      exportsTypeAliases = getExportsTypeAliases exports
      exportsValues :: Map.Map String Scheme
      exportsValues = getExportsValues exports
      exportsConstructors :: Map.Map String Scheme
      exportsConstructors = getExportsConstructors exports
      exportsTypes :: Map.Map String TypeInfo
      exportsTypes = getExportsTypes exports
      -- Use pre-expanded type aliases from exports (computed by addValuesToExports)
      -- Fall back to expanding them ourselves for backwards compatibility
      moduleAliases :: Map.Map String Type
      moduleAliases = if Map.isEmpty exportsExpandedTypeAliases
                      then expandModuleAliases exportsTypeAliases
                      else exportsExpandedTypeAliases
      -- Helper to import all RECORD type aliases from a module
      -- This is needed because record type aliases may be referenced by values
      addAllRecordAliases :: Env -> Env
      addAllRecordAliases e =
        let addIfRecord :: Env -> Tuple String Type -> Env
            addIfRecord e2 (Tuple name ty) =
              if isRecordType ty then extendTypeAlias e2 name ty else e2
        in Array.foldl addIfRecord e (Map.toUnfoldable moduleAliases)
  in case item of
    ImportValue name ->
      -- Import a value or constructor by name
      -- Also check if it's a type alias (type aliases are imported as values in PureScript)
      -- ALSO import all record type aliases from the module (needed for unification)
      case Map.lookup name exportsValues of
        Just scheme -> addAllRecordAliases (extendEnv env name scheme)
        Nothing -> case Map.lookup name exportsConstructors of
          Just scheme -> addAllRecordAliases (extendEnv env name scheme)
          Nothing ->
            -- Check if it's a type alias
            case Map.lookup name moduleAliases of
              Just ty -> addAllRecordAliases (extendTypeAlias env name ty)
              Nothing -> addAllRecordAliases env  -- Still import record aliases even if value not found
    ImportType typeName spec ->
      -- Import a type and optionally its constructors
      -- First check if it's a type alias and add it to env.typeAliases
      -- Also add any RECORD type aliases that this type references (from the unexpanded TypeExpr)
      let env' = case Map.lookup typeName exportsTypeAliases of
            Just aliasInfo ->
              -- Add the expanded type alias itself
              let expandedTy :: Type
                  expandedTy = case Map.lookup typeName moduleAliases of
                    Just ty -> ty
                    Nothing -> typeExprToType Map.empty aliasInfo.body
                  e1 :: Env
                  e1 = extendTypeAlias env typeName expandedTy
                  -- Collect type names from the UNEXPANDED body (TypeExpr) to find referenced aliases
                  referencedNames :: Set.Set String
                  referencedNames = collectTypeExprNames aliasInfo.body
                  addIfRecordAlias :: Env -> String -> Env
                  addIfRecordAlias e name = case Map.lookup name moduleAliases of
                    Just aliasTy | isRecordType aliasTy -> extendTypeAlias e name aliasTy
                    _ -> e
              in Array.foldl addIfRecordAlias e1 (Set.toUnfoldable referencedNames)
            Nothing ->
              -- Check if it's in moduleAliases (simple type alias from expansion)
              case Map.lookup typeName moduleAliases of
                Just ty -> extendTypeAlias env typeName ty
                Nothing -> env
      in case Map.lookup typeName exportsTypes of
        Nothing ->
          -- Not a data type, but still import record type aliases from module
          -- (needed for types that reference record aliases internally)
          addAllRecordAliases env'
        Just typeInfo ->
          case spec of
            ImportAll ->
              -- Import all constructors and their record type alias dependencies
              addAllRecordAliases (mergeTypeExport env' exports typeName typeInfo.constructors)
            ImportSome ctorNames ->
              -- Import specific constructors
              addAllRecordAliases (mergeTypeExport env' exports typeName (List.toUnfoldable ctorNames))
            ImportNone ->
              -- Import just the type, no constructors, but still need record aliases
              addAllRecordAliases env'

-- | Resolve imports to track which modules provide which names
-- | Returns a map from imported name -> source module name
resolveImports :: ModuleRegistry -> Array Declaration -> ResolvedImports
resolveImports registry decls = Array.foldl processDecl Map.empty decls
  where
  processDecl resolved (DeclImport imp) = resolveImportDecl registry resolved imp
  processDecl resolved _ = resolved

-- | Resolve a single import declaration
resolveImportDecl :: ModuleRegistry -> ResolvedImports -> ImportDeclaration -> ResolvedImports
resolveImportDecl registry resolved imp =
  case Types.lookupModule registry imp.moduleName of
    Nothing -> resolved
    Just exports ->
      if imp.hiding then
        -- Import everything except listed items
        let allNames = getAllExportedNames exports
            itemsAsArray :: Array ImportItem
            itemsAsArray = List.toUnfoldable imp.items
            hiddenNames = Set.fromFoldable (arrayMap getItemName itemsAsArray)
            importedNames = Set.difference allNames hiddenNames
            importedArray :: Array String
            importedArray = Set.toUnfoldable importedNames
        in Array.foldl (\m name -> Map.insert name imp.moduleName m) resolved importedArray
      else if List.null imp.items then
        -- Import everything unqualified
        let allNames = getAllExportedNames exports
            allNamesArray :: Array String
            allNamesArray = Set.toUnfoldable allNames
        in Array.foldl (\m name -> Map.insert name imp.moduleName m) resolved allNamesArray
      else
        -- Import only specified items
        let itemsArray :: Array ImportItem
            itemsArray = List.toUnfoldable imp.items
        in Array.foldl (\m item -> Map.insert (getItemName item) imp.moduleName m) resolved itemsArray

-- Helper functions

-- | Get the name of an import item
getItemName :: ImportItem -> String
getItemName (ImportValue name) = name
getItemName (ImportType name _) = name

-- | Get all exported names from a module
getAllExportedNames :: ModuleExports -> Set.Set String
getAllExportedNames exports =
  let valueNames = Set.fromFoldable (Map.keys exports.values)
      ctorNames = Set.fromFoldable (Map.keys exports.constructors)
      typeNames = Set.fromFoldable (Map.keys exports.types)
      aliasNames = Set.fromFoldable (Map.keys exports.typeAliases)
  in Set.unions [valueNames, ctorNames, typeNames, aliasNames]

-- | Extend the environment with a new binding
extendEnv :: Env -> String -> Scheme -> Env
extendEnv env name scheme =
  env { bindings = Map.insert name scheme env.bindings }

-- | Extend the environment with a type alias
extendTypeAlias :: Env -> String -> Type -> Env
extendTypeAlias env name ty =
  env { typeAliases = Map.insert name ty env.typeAliases }

-- | Merge type export (constructors) into environment
mergeTypeExport :: Env -> ModuleExports -> String -> Array String -> Env
mergeTypeExport env exports _typeName ctorNames =
  let addCtor e ctorName =
        case Map.lookup ctorName exports.constructors of
          Just scheme -> extendEnv e ctorName scheme
          Nothing -> e
  in Array.foldl addCtor env ctorNames

-- | Convert a TypeExpr to Type (simple conversion, no alias expansion)
typeExprToType :: Map.Map String Type -> TypeExpr -> Type
typeExprToType aliases expr = case expr of
  TyExprVar name ->
    case Map.lookup name aliases of
      Just ty -> ty
      Nothing -> TyVar { id: 0, name: name }
  TyExprCon name ->
    TyCon { name: name, args: [] }
  TyExprArrow from to ->
    tArrow (typeExprToType aliases from) (typeExprToType aliases to)
  TyExprRecord fields maybeRow ->
    let fieldsAsArray :: Array (Tuple String TypeExpr)
        fieldsAsArray = List.toUnfoldable fields
        fieldMap = Map.fromFoldable (arrayMap (\(Tuple label ty) -> Tuple label (typeExprToType aliases ty)) fieldsAsArray)
        rowVar = case maybeRow of
          Just rowName -> Just { id: 0, name: rowName }
          Nothing -> Nothing
    in TyRecord { fields: fieldMap, row: rowVar }
  TyExprForAll _vars inner ->
    typeExprToType aliases inner
  TyExprParens inner ->
    typeExprToType aliases inner
  TyExprApp base arg ->
    case typeExprToType aliases base of
      TyCon tc -> TyCon { name: tc.name, args: tc.args <> [typeExprToType aliases arg] }
      other -> other
  TyExprConstrained _constraints inner ->
    typeExprToType aliases inner
  TyExprTuple items ->
    let itemsAsArray :: Array TypeExpr
        itemsAsArray = List.toUnfoldable items
        argTypes = arrayMap (typeExprToType aliases) itemsAsArray
    in TyCon { name: "Tuple", args: argTypes }

-- | Convert a TypeExpr to Type with full alias expansion
typeExprToTypeWithAllAliases :: Map.Map String Type -> Map.Map String TypeAliasInfo -> Map.Map String Type -> TypeExpr -> Type
typeExprToTypeWithAllAliases simpleAliases fullAliases paramAliases expr = case expr of
  TyExprVar name ->
    case Map.lookup name paramAliases of
      Just ty -> ty
      Nothing -> case Map.lookup name simpleAliases of
        Just ty -> ty
        Nothing -> TyVar { id: 0, name: name }
  TyExprCon name ->
    -- Check if this is a type alias that should be expanded (no params version)
    case Map.lookup name fullAliases of
      Just aliasInfo | Array.null aliasInfo.params ->
        typeExprToTypeWithAllAliases simpleAliases fullAliases paramAliases aliasInfo.body
      _ ->
        TyCon { name: name, args: [] }
  TyExprArrow from to ->
    tArrow
      (typeExprToTypeWithAllAliases simpleAliases fullAliases paramAliases from)
      (typeExprToTypeWithAllAliases simpleAliases fullAliases paramAliases to)
  TyExprRecord fields maybeRow ->
    let fieldsAsArray :: Array (Tuple String TypeExpr)
        fieldsAsArray = List.toUnfoldable fields
        fieldMap = Map.fromFoldable (arrayMap (\(Tuple label ty) -> Tuple label (typeExprToTypeWithAllAliases simpleAliases fullAliases paramAliases ty)) fieldsAsArray)
        rowVar = case maybeRow of
          Just rowName -> case Map.lookup rowName paramAliases of
            Just (TyVar v) -> Just v
            Just _ -> Nothing
            Nothing -> Just { id: 0, name: rowName }
          Nothing -> Nothing
    in TyRecord { fields: fieldMap, row: rowVar }
  TyExprForAll _vars inner ->
    typeExprToTypeWithAllAliases simpleAliases fullAliases paramAliases inner
  TyExprParens inner ->
    typeExprToTypeWithAllAliases simpleAliases fullAliases paramAliases inner
  TyExprApp _base _arg ->
    -- Collect all args from nested TyExprApp into one TCon
    let Tuple conName args = collectTypeApp expr
        argTypes = arrayMap (typeExprToTypeWithAllAliases simpleAliases fullAliases paramAliases) args
    in case Map.lookup conName fullAliases of
      Just aliasInfo | Array.length argTypes == Array.length aliasInfo.params ->
        let paramMap = Map.fromFoldable (Array.zip aliasInfo.params argTypes)
        in typeExprToTypeWithAllAliases simpleAliases fullAliases paramMap aliasInfo.body
      _ -> TyCon { name: conName, args: argTypes }
  TyExprConstrained _constraints inner ->
    typeExprToTypeWithAllAliases simpleAliases fullAliases paramAliases inner
  TyExprTuple items ->
    let itemsAsArray :: Array TypeExpr
        itemsAsArray = List.toUnfoldable items
        argTypes = arrayMap (typeExprToTypeWithAllAliases simpleAliases fullAliases paramAliases) itemsAsArray
    in TyCon { name: "Tuple", args: argTypes }

-- | Collect type constructor name and args from nested TyExprApp
collectTypeApp :: TypeExpr -> Tuple String (Array TypeExpr)
collectTypeApp (TyExprCon name) = Tuple name []
collectTypeApp (TyExprApp base arg) =
  let Tuple name args = collectTypeApp base
  in Tuple name (args <> [arg])
collectTypeApp _ = Tuple "Unknown" []

-- | Collect all type names referenced in a TypeExpr
collectTypeExprNames :: TypeExpr -> Set.Set String
collectTypeExprNames expr = case expr of
  TyExprVar _ -> Set.empty
  TyExprCon name -> Set.singleton name
  TyExprArrow from to ->
    Set.union (collectTypeExprNames from) (collectTypeExprNames to)
  TyExprRecord fields _ ->
    let fieldsAsArray :: Array (Tuple String TypeExpr)
        fieldsAsArray = List.toUnfoldable fields
        fieldsSets :: Array (Set.Set String)
        fieldsSets = arrayMap (\(Tuple _ ty) -> collectTypeExprNames ty) fieldsAsArray
    in Set.unions fieldsSets
  TyExprForAll _ inner ->
    collectTypeExprNames inner
  TyExprParens inner ->
    collectTypeExprNames inner
  TyExprApp base arg ->
    Set.union (collectTypeExprNames base) (collectTypeExprNames arg)
  TyExprConstrained _ inner ->
    collectTypeExprNames inner
  TyExprTuple items ->
    let itemsAsArray :: Array TypeExpr
        itemsAsArray = List.toUnfoldable items
        itemsSets :: Array (Set.Set String)
        itemsSets = arrayMap collectTypeExprNames itemsAsArray
    in Set.unions itemsSets
