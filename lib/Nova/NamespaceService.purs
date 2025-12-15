-- | Namespace Service for Nova Compiler
-- |
-- | Manages namespaces with declarations, providing:
-- | - Create/delete namespaces
-- | - Add/update/remove declarations
-- | - Lookup by name or ID
-- | - List all declarations
module Nova.NamespaceService where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array as Array
import OTP.Ets as Ets

-- | Declaration status
data DeclStatus = Fresh | Valid | Invalid | Stale

-- | Declaration kind
data DeclKind = FunctionDecl | DatatypeDecl | TypeAliasDecl | ForeignDecl

-- | A managed declaration
type ManagedDecl =
  { declId :: String
  , namespace :: String
  , name :: String
  , kind :: DeclKind
  , sourceText :: String
  , status :: DeclStatus
  , inferredType :: Maybe String
  , errors :: Array String
  }

-- | Reference type between declarations
data RefType = CallRef | TypeRef | ImportRef | PatternRef | ConstructorRef

instance eqRefType :: Eq RefType where
  eq CallRef CallRef = true
  eq TypeRef TypeRef = true
  eq ImportRef ImportRef = true
  eq PatternRef PatternRef = true
  eq ConstructorRef ConstructorRef = true
  eq _ _ = false

-- | Convert RefType to string for display
showRefType :: RefType -> String
showRefType CallRef = "call"
showRefType TypeRef = "type"
showRefType ImportRef = "import"
showRefType PatternRef = "pattern"
showRefType ConstructorRef = "constructor"

-- | A reference between declarations
type DeclRef =
  { sourceId :: String   -- Declaration making the reference
  , destId :: String     -- Declaration being referenced
  , refType :: RefType   -- Kind of reference
  }

-- | Namespace service state
type ServiceState =
  { namespaces :: Ets.Table
  , declarations :: Ets.Table
  , nameIndex :: Ets.Table
  , counter :: Ets.Table
  , refFrom :: Ets.Table  -- sourceId -> [DeclRef] (what does this decl reference)
  , refTo :: Ets.Table    -- destId -> [DeclRef] (what references this decl)
  }

-- | Initialize the namespace service
init :: Unit -> Either String ServiceState
init _u =
  case Ets.new "nova_namespaces" Ets.publicSetOptions of
    Left err -> Left err
    Right nsTab ->
      case Ets.new "nova_declarations" Ets.publicSetOptions of
        Left err -> Left err
        Right declTab ->
          case Ets.new "nova_name_index" Ets.publicSetOptions of
            Left err -> Left err
            Right idxTab ->
              case Ets.new "nova_counter" Ets.publicSetOptions of
                Left err -> Left err
                Right cntTab ->
                  case Ets.new "nova_ref_from" Ets.publicSetOptions of
                    Left err -> Left err
                    Right refFromTab ->
                      case Ets.new "nova_ref_to" Ets.publicSetOptions of
                        Left err -> Left err
                        Right refToTab ->
                          let _w = Ets.insert cntTab "counter" 0
                          in Right { namespaces: nsTab
                                   , declarations: declTab
                                   , nameIndex: idxTab
                                   , counter: cntTab
                                   , refFrom: refFromTab
                                   , refTo: refToTab
                                   }

-- | Create a new namespace
createNamespace :: ServiceState -> String -> Either String Unit
createNamespace st name =
  if Ets.member st.namespaces name
  then Left "Namespace already exists"
  else
    let nsInfo = { name: name, imports: emptyArray }
        _w = Ets.insert st.namespaces name nsInfo
    in Right unit

-- | Delete a namespace and all its declarations
deleteNamespace :: ServiceState -> String -> Either String Unit
deleteNamespace st name =
  if not (Ets.member st.namespaces name)
  then Left "Namespace not found"
  else
    let decls = getNamespaceDecls st name
        _w = Array.foldl (\acc d -> let _x = removeDecl st d.declId in acc) unit decls
        _w2 = Ets.delete st.namespaces name
    in Right unit

-- | Generate a unique declaration ID
genDeclId :: ServiceState -> String
genDeclId st =
  case Ets.lookup st.counter "counter" of
    Nothing -> "decl_0"
    Just n ->
      let next = n + 1
          _w = Ets.update st.counter "counter" next
      in "decl_" <> intToString next

foreign import intToString :: Int -> String
  = "call 'erlang':'integer_to_list'($0)"

-- | Add a declaration to a namespace
addDecl :: ServiceState -> String -> String -> String -> DeclKind -> Either String String
addDecl st namespace name sourceText kind =
  addDeclWithType st namespace name sourceText kind Nothing

-- | Add a declaration with an optional type signature
addDeclWithType :: ServiceState -> String -> String -> String -> DeclKind -> Maybe String -> Either String String
addDeclWithType st namespace name sourceText kind typeStr =
  if not (Ets.member st.namespaces namespace)
  then Left "Namespace not found"
  else
    let declId = genDeclId st
        decl = { declId: declId
               , namespace: namespace
               , name: name
               , kind: kind
               , sourceText: sourceText
               , status: Fresh
               , inferredType: typeStr
               , errors: emptyArray
               }
        _w1 = Ets.insert st.declarations declId decl
        idxKey = namespace <> ":" <> name
        _w2 = Ets.insert st.nameIndex idxKey declId
    in Right declId

-- | Update a declaration
updateDecl :: ServiceState -> String -> String -> Either String Unit
updateDecl st declId newSourceText =
  case Ets.lookup st.declarations declId of
    Nothing -> Left "Declaration not found"
    Just decl ->
      let updated = decl { sourceText = newSourceText, status = Stale }
          _w = Ets.update st.declarations declId updated
      in Right unit

-- | Remove a declaration
removeDecl :: ServiceState -> String -> Either String Unit
removeDecl st declId =
  case Ets.lookup st.declarations declId of
    Nothing -> Left "Declaration not found"
    Just decl ->
      let idxKey = decl.namespace <> ":" <> decl.name
          _w1 = Ets.delete st.nameIndex idxKey
          _w2 = Ets.delete st.declarations declId
      in Right unit

-- | Get a declaration by ID
getDecl :: ServiceState -> String -> Maybe ManagedDecl
getDecl st declId = Ets.lookup st.declarations declId

-- | Get a declaration by namespace and name
getDeclByName :: ServiceState -> String -> String -> Maybe ManagedDecl
getDeclByName st namespace name =
  let idxKey = namespace <> ":" <> name
  in case Ets.lookup st.nameIndex idxKey of
    Nothing -> Nothing
    Just declId -> Ets.lookup st.declarations declId

-- | Get all declarations in a namespace
getNamespaceDecls :: ServiceState -> String -> Array ManagedDecl
getNamespaceDecls st namespace =
  let allDecls = Ets.allValues st.declarations
  in Array.filter (\d -> d.namespace == namespace) allDecls

-- | Get all namespaces
listNamespaces :: ServiceState -> Array String
listNamespaces st = Ets.allKeys st.namespaces

-- | Check if namespace exists
namespaceExists :: ServiceState -> String -> Boolean
namespaceExists st name = Ets.member st.namespaces name

-- | Set declaration status
setDeclStatus :: ServiceState -> String -> DeclStatus -> Either String Unit
setDeclStatus st declId status =
  case Ets.lookup st.declarations declId of
    Nothing -> Left "Declaration not found"
    Just decl ->
      let updated = decl { status = status }
          _w = Ets.update st.declarations declId updated
      in Right unit

-- | Set declaration errors
setDeclErrors :: ServiceState -> String -> Array String -> Either String Unit
setDeclErrors st declId errors =
  case Ets.lookup st.declarations declId of
    Nothing -> Left "Declaration not found"
    Just decl ->
      let updated = decl { errors = errors, status = Invalid }
          _w = Ets.update st.declarations declId updated
      in Right unit

-- | Set declaration type
setDeclType :: ServiceState -> String -> String -> Either String Unit
setDeclType st declId typeStr =
  case Ets.lookup st.declarations declId of
    Nothing -> Left "Declaration not found"
    Just decl ->
      let updated = decl { inferredType = Just typeStr, status = Valid }
          _w = Ets.update st.declarations declId updated
      in Right unit

-- | Get declarations by status
getDeclsByStatus :: ServiceState -> DeclStatus -> Array ManagedDecl
getDeclsByStatus st status =
  let allDecls = Ets.allValues st.declarations
  in Array.filter (\d -> statusEq d.status status) allDecls

-- | Status equality (pattern matching workaround)
statusEq :: DeclStatus -> DeclStatus -> Boolean
statusEq Fresh Fresh = true
statusEq Valid Valid = true
statusEq Invalid Invalid = true
statusEq Stale Stale = true
statusEq _ _ = false

-- | Empty array helper
emptyArray :: forall a. Array a
emptyArray = emptyArrayImpl unit

foreign import emptyArrayImpl :: forall a. Unit -> Array a
  = "[]"

-- | Add a reference from sourceId to destId
addRef :: ServiceState -> String -> String -> RefType -> Unit
addRef st sourceId destId refType =
  let ref = { sourceId: sourceId, destId: destId, refType: refType }
      -- Add to refFrom (what sourceId references)
      fromRefs = case Ets.lookup st.refFrom sourceId of
        Nothing -> [ref]
        Just existing -> Array.snoc existing ref
      _w1 = Ets.insert st.refFrom sourceId fromRefs
      -- Add to refTo (what references destId)
      toRefs = case Ets.lookup st.refTo destId of
        Nothing -> [ref]
        Just existing -> Array.snoc existing ref
      _w2 = Ets.insert st.refTo destId toRefs
  in unit

-- | Get all references FROM a declaration (what does it reference)
getRefsFrom :: ServiceState -> String -> Array DeclRef
getRefsFrom st sourceId =
  case Ets.lookup st.refFrom sourceId of
    Nothing -> emptyArray
    Just refs -> refs

-- | Get all references TO a declaration (what references it)
getRefsTo :: ServiceState -> String -> Array DeclRef
getRefsTo st destId =
  case Ets.lookup st.refTo destId of
    Nothing -> emptyArray
    Just refs -> refs

-- | Clear all references from a declaration (when recompiling)
clearRefsFrom :: ServiceState -> String -> Unit
clearRefsFrom st sourceId =
  case Ets.lookup st.refFrom sourceId of
    Nothing -> unit
    Just refs ->
      -- Remove this source from each dest's refTo list
      let _w1 = Array.foldl (\acc ref ->
            let destRefs = case Ets.lookup st.refTo ref.destId of
                  Nothing -> emptyArray
                  Just existing -> Array.filter (\r -> r.sourceId /= sourceId) existing
                _w = Ets.insert st.refTo ref.destId destRefs
            in acc) unit refs
          _w2 = Ets.delete st.refFrom sourceId
      in unit

-- | Clear all references to a declaration (when deleting)
clearRefsTo :: ServiceState -> String -> Unit
clearRefsTo st destId =
  case Ets.lookup st.refTo destId of
    Nothing -> unit
    Just refs ->
      -- Remove this dest from each source's refFrom list
      let _w1 = Array.foldl (\acc ref ->
            let srcRefs = case Ets.lookup st.refFrom ref.sourceId of
                  Nothing -> emptyArray
                  Just existing -> Array.filter (\r -> r.destId /= destId) existing
                _w = Ets.insert st.refFrom ref.sourceId srcRefs
            in acc) unit refs
          _w2 = Ets.delete st.refTo destId
      in unit

-- | Get all declarations that reference a given qualified name (legacy compat)
getReferencedBy :: ServiceState -> String -> Array String
getReferencedBy st destId =
  Array.map (\ref -> ref.sourceId) (getRefsTo st destId)

-- | Add multiple references from a source at once
addRefs :: ServiceState -> String -> Array { destId :: String, refType :: RefType } -> Unit
addRefs st sourceId refs =
  Array.foldl (\acc r -> let _w = addRef st sourceId r.destId r.refType in acc) unit refs

-- | Get count of references from a declaration
getRefFromCount :: ServiceState -> String -> Int
getRefFromCount st sourceId = Array.length (getRefsFrom st sourceId)

-- | Get count of references to a declaration
getRefToCount :: ServiceState -> String -> Int
getRefToCount st destId = Array.length (getRefsTo st destId)

-- | Get all reference types used by a declaration
getRefTypes :: ServiceState -> String -> Array RefType
getRefTypes st sourceId =
  let refs = getRefsFrom st sourceId
  in Array.nub (Array.map (\r -> r.refType) refs)

-- | Shutdown the service (delete tables)
shutdown :: ServiceState -> Unit
shutdown st =
  let _w1 = Ets.deleteTable st.namespaces
      _w2 = Ets.deleteTable st.declarations
      _w3 = Ets.deleteTable st.nameIndex
      _w4 = Ets.deleteTable st.counter
      _w5 = Ets.deleteTable st.refFrom
      _w6 = Ets.deleteTable st.refTo
  in unit

-- | Save all tables to a directory
save :: ServiceState -> String -> Either String Unit
save st dir =
  case Ets.tab2file st.namespaces (dir <> "/namespaces.ets") of
    Left err -> Left ("Failed to save namespaces: " <> err)
    Right _ ->
      case Ets.tab2file st.declarations (dir <> "/declarations.ets") of
        Left err -> Left ("Failed to save declarations: " <> err)
        Right _ ->
          case Ets.tab2file st.nameIndex (dir <> "/name_index.ets") of
            Left err -> Left ("Failed to save name index: " <> err)
            Right _ ->
              case Ets.tab2file st.counter (dir <> "/counter.ets") of
                Left err -> Left ("Failed to save counter: " <> err)
                Right _ ->
                  case Ets.tab2file st.refFrom (dir <> "/ref_from.ets") of
                    Left err -> Left ("Failed to save ref_from: " <> err)
                    Right _ ->
                      case Ets.tab2file st.refTo (dir <> "/ref_to.ets") of
                        Left err -> Left ("Failed to save ref_to: " <> err)
                        Right _ -> Right unit

-- | Load all tables from a directory
load :: String -> Either String ServiceState
load dir =
  case Ets.file2tab (dir <> "/namespaces.ets") of
    Left err -> Left ("Failed to load namespaces: " <> err)
    Right nsTab ->
      case Ets.file2tab (dir <> "/declarations.ets") of
        Left err -> Left ("Failed to load declarations: " <> err)
        Right declTab ->
          case Ets.file2tab (dir <> "/name_index.ets") of
            Left err -> Left ("Failed to load name index: " <> err)
            Right idxTab ->
              case Ets.file2tab (dir <> "/counter.ets") of
                Left err -> Left ("Failed to load counter: " <> err)
                Right cntTab ->
                  case Ets.file2tab (dir <> "/ref_from.ets") of
                    Left err -> Left ("Failed to load ref_from: " <> err)
                    Right refFromTab ->
                      case Ets.file2tab (dir <> "/ref_to.ets") of
                        Left err -> Left ("Failed to load ref_to: " <> err)
                        Right refToTab ->
                          Right { namespaces: nsTab
                                , declarations: declTab
                                , nameIndex: idxTab
                                , counter: cntTab
                                , refFrom: refFromTab
                                , refTo: refToTab
                                }
