-- | Nova Web UI - Simple backend-rendered HTML interface
module Nova.Web where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.Char as Char
import Data.List (List(..))
import Data.List as List
import Data.String as String
import Data.Json as Json
import Data.Tuple (Tuple(..))
import Data.Map as Map
import OTP.PersistentTerm as PT
import Nova.Eval as Eval
import Nova.NamespaceService as NS
import Nova.Compiler.CstPipeline as Pipeline
import Nova.Compiler.Ast as Ast

-- | Global state key
stateKey :: String
stateKey = "nova_web_state"

-- | Initialize the service if not already initialized
-- | Also loads all lib/ and src/ .purs files into namespaces
initServiceIfNeeded :: Unit -> Unit
initServiceIfNeeded _ =
  case PT.get stateKey of
    Nothing ->
      case NS.init unit of
        Right st ->
          let _s = PT.put stateKey st
              -- Load from parent directory (we run from nova_lang/)
              _lib = importAllFromDir "../lib"
              _src = importAllFromDir "../src"
          in unit
        Left _ -> unit
    Just _ -> unit

-- | Import all .purs files from a directory recursively
importAllFromDir :: String -> Array (Either String { moduleName :: String, declCount :: Int })
importAllFromDir dir =
  let files = listPursFilesRecursive dir
  in Array.map importFile files

-- | List all .purs files recursively in a directory
-- | Returns binary strings (converts from charlists)
foreign import listPursFilesRecursive :: String -> Array String
  = "let <Pattern> = call 'erlang':'binary_to_list'(call 'erlang':'iolist_to_binary'([$0, [47,42,42,47,42,46,112,117,114,115]])) in let <Files> = call 'filelib':'wildcard'(Pattern) in call 'lists':'map'(fun (F) -> call 'erlang':'list_to_binary'(F), Files)"

-- | Get the current service state (unsafe - assumes initialized)
foreign import getState :: Unit -> NS.ServiceState
  = "call 'persistent_term':'get'('nova_web_state')"

-- | List all namespace names (uses FFI to avoid type checker issues with ServiceState)
foreign import listNamespaces :: Unit -> Array String
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'listNamespaces'(St)"

-- | Check if namespace exists
foreign import namespaceExists :: String -> Boolean
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'namespaceExists'(St, $0)"

-- | Create a namespace
foreign import createNamespace :: String -> Either String Unit
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'createNamespace'(St, $0)"

-- | Delete a namespace
foreign import deleteNamespace :: String -> Either String Unit
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'deleteNamespace'(St, $0)"

-- | Get declarations in a namespace
foreign import getNamespaceDecls :: String -> Array NS.ManagedDecl
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'getNamespaceDecls'(St, $0)"

-- | Add a declaration
foreign import addDecl :: String -> String -> String -> NS.DeclKind -> Either String String
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'addDecl'(St, $0, $1, $2, $3)"

-- | Add a declaration with type signature
foreign import addDeclWithType :: String -> String -> String -> NS.DeclKind -> Maybe String -> Either String String
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'addDeclWithType'(St, $0, $1, $2, $3, $4)"

-- ============================================================================
-- Module Import Functions
-- ============================================================================

-- | Import a PureScript module source into the namespace service
-- | Parses the source, creates namespace, and adds each declaration
importModule :: String -> Either String { moduleName :: String, declCount :: Int }
importModule source =
  case Pipeline.parseModuleCst source of
    Left err -> Left ("Parse error: " <> err)
    Right mod ->
      let modName = mod.name
          _ns = createNamespace modName
          results = importDeclarations modName mod.declarations
          successCount = countSuccesses results
      in Right { moduleName: modName, declCount: successCount }

-- | Import a list of declarations into a namespace
-- | Groups multi-clause functions together to preserve pattern match order
importDeclarations :: String -> List Ast.Declaration -> Array (Either String String)
importDeclarations namespace decls =
  let -- Extract type signatures into a map by name
      typeSigs = buildTypeSigMap decls
      -- Separate functions from other declarations
      funcs = List.mapMaybe getFuncDecl decls
      others = List.filter (not <<< isFuncDecl) decls
      -- Group functions by name, preserving order
      groupedFuncs = groupFunctionsByName typeSigs funcs
      -- Import grouped functions
      funcResults = Array.map (importGroupedFunction namespace) groupedFuncs
      -- Import other declarations
      otherResults = Array.fromFoldable (List.mapMaybe (importOtherDecl namespace) others)
  in funcResults <> otherResults

-- | Build a map of function name -> type signature string
buildTypeSigMap :: List Ast.Declaration -> Map.Map String String
buildTypeSigMap decls =
  List.foldl addTypeSig Map.empty decls
  where
    addTypeSig :: Map.Map String String -> Ast.Declaration -> Map.Map String String
    addTypeSig acc (Ast.DeclTypeSig sig) = Map.insert sig.name (renderTypeSig sig) acc
    addTypeSig acc _ = acc

-- | Render a type signature to string
renderTypeSig :: Ast.TypeSignature -> String
renderTypeSig sig =
  let constraintPart = case sig.constraints of
        Nil -> ""
        cs -> String.joinWith ", " (Array.fromFoldable (List.map renderConstraint cs)) <> " => "
  in sig.name <> " :: " <> constraintPart <> renderTypeExpr sig.ty

-- | Render a constraint
renderConstraint :: Ast.Constraint -> String
renderConstraint c = c.className <> " " <> String.joinWith " " (Array.fromFoldable (List.map renderTypeExpr c.types))

-- | Extract function declaration if present
getFuncDecl :: Ast.Declaration -> Maybe Ast.FunctionDeclaration
getFuncDecl (Ast.DeclFunction f) = Just f
getFuncDecl _ = Nothing

-- | Check if declaration is a function
isFuncDecl :: Ast.Declaration -> Boolean
isFuncDecl (Ast.DeclFunction _) = true
isFuncDecl _ = false

-- | Group function clauses by name, preserving order within each group
-- | Uses type signature map to look up signatures for each function
-- | Returns array of {name, clauses, typeSig} records
groupFunctionsByName :: Map.Map String String -> List Ast.FunctionDeclaration -> Array { name :: String, clauses :: Array Ast.FunctionDeclaration, typeSig :: Maybe String }
groupFunctionsByName typeSigs funcs =
  let -- Fold to group by name, building list of groups in reverse order
      grouped = List.foldl (\groups func -> addFuncToGroups groups func) Nil funcs
      -- Reverse to restore original order, and finalize each group with type sigs
  in Array.map (finalizeGroup typeSigs) (Array.reverse (Array.fromFoldable grouped))

-- | Add a function to the appropriate group
addFuncToGroups :: List { name :: String, clauses :: List Ast.FunctionDeclaration } -> Ast.FunctionDeclaration -> List { name :: String, clauses :: List Ast.FunctionDeclaration }
addFuncToGroups groups func =
  case findFuncGroup func.name groups of
    Nothing ->
      -- New group
      Cons { name: func.name, clauses: Cons func Nil } groups
    Just { before, group, after } ->
      -- Add to existing group (append to end to preserve order)
      let newGroup = { name: group.name, clauses: List.snoc group.clauses func }
      in before <> Cons newGroup after

-- | Find a group by name, returning the group and surrounding elements
findFuncGroup :: String -> List { name :: String, clauses :: List Ast.FunctionDeclaration } -> Maybe { before :: List { name :: String, clauses :: List Ast.FunctionDeclaration }, group :: { name :: String, clauses :: List Ast.FunctionDeclaration }, after :: List { name :: String, clauses :: List Ast.FunctionDeclaration } }
findFuncGroup name groups = findFuncGroupHelper name Nil groups

findFuncGroupHelper :: String -> List { name :: String, clauses :: List Ast.FunctionDeclaration } -> List { name :: String, clauses :: List Ast.FunctionDeclaration } -> Maybe { before :: List { name :: String, clauses :: List Ast.FunctionDeclaration }, group :: { name :: String, clauses :: List Ast.FunctionDeclaration }, after :: List { name :: String, clauses :: List Ast.FunctionDeclaration } }
findFuncGroupHelper _ _ Nil = Nothing
findFuncGroupHelper name before (Cons g rest) =
  if g.name == name
  then Just { before: List.reverse before, group: g, after: rest }
  else findFuncGroupHelper name (Cons g before) rest

-- | Convert internal group format to output format, looking up type signature
finalizeGroup :: Map.Map String String -> { name :: String, clauses :: List Ast.FunctionDeclaration } -> { name :: String, clauses :: Array Ast.FunctionDeclaration, typeSig :: Maybe String }
finalizeGroup typeSigs g =
  { name: g.name
  , clauses: Array.fromFoldable g.clauses
  , typeSig: Map.lookup g.name typeSigs
  }

-- | Import a grouped function (all clauses combined)
importGroupedFunction :: String -> { name :: String, clauses :: Array Ast.FunctionDeclaration, typeSig :: Maybe String } -> Either String String
importGroupedFunction namespace group =
  let -- Render all clauses in order
      clauseTexts = Array.map renderFunctionBody group.clauses
      src = String.joinWith "\n" clauseTexts
  in addDeclWithType namespace group.name src NS.FunctionDecl group.typeSig

-- | Import non-function declarations
importOtherDecl :: String -> Ast.Declaration -> Maybe (Either String String)
importOtherDecl namespace decl =
  case decl of
    Ast.DeclDataType d ->
      let src = renderDataType d
          typeSig = Just (renderDataTypeSig d)
      in Just (addDeclWithType namespace d.name src NS.DatatypeDecl typeSig)
    Ast.DeclNewtype n ->
      let src = renderNewtype n
          typeSig = Just (renderNewtypeSig n)
      in Just (addDeclWithType namespace n.name src NS.DatatypeDecl typeSig)
    Ast.DeclTypeAlias a ->
      let src = renderTypeAlias a
          typeSig = Just (renderTypeExpr a.ty)
      in Just (addDeclWithType namespace a.name src NS.TypeAliasDecl typeSig)
    Ast.DeclForeignImport f ->
      let src = renderForeignBody f
          typeSig = Just (renderTypeExpr f.typeSignature)
      in Just (addDeclWithType namespace f.functionName src NS.ForeignDecl typeSig)
    Ast.DeclTypeClass c ->
      let src = renderClass c
          typeSig = Just ("class " <> c.name)
      in Just (addDeclWithType namespace c.name src NS.DatatypeDecl typeSig)
    -- Skip imports, type signatures (merged with functions), instances, infix, functions
    _ -> Nothing

-- | Count successful imports
countSuccesses :: Array (Either String String) -> Int
countSuccesses arr = Array.length (Array.filter isRight arr)
  where
    isRight (Right _) = true
    isRight (Left _) = false

-- ============================================================================
-- Declaration Renderers (using actual Ast types)
-- ============================================================================

-- | Render a function declaration to source (with signature)
renderFunction :: Ast.FunctionDeclaration -> String
renderFunction f =
  let sigPart = case f.typeSignature of
        Just sig -> f.name <> " :: " <> renderTypeExpr sig.ty <> "\n"
        Nothing -> ""
      params = String.joinWith " " (Array.fromFoldable (List.map renderPattern f.parameters))
      paramPart = if params == "" then "" else " " <> params
  in sigPart <> f.name <> paramPart <> " = " <> renderExpr f.body

-- | Render function body only (without signature, for display)
renderFunctionBody :: Ast.FunctionDeclaration -> String
renderFunctionBody f =
  let params = String.joinWith " " (Array.fromFoldable (List.map renderPattern f.parameters))
      paramPart = if params == "" then "" else " " <> params
  in f.name <> paramPart <> " = " <> renderExpr f.body

-- | Render a data type to source
renderDataType :: Ast.DataType -> String
renderDataType d =
  let vars = String.joinWith " " (Array.fromFoldable d.typeVars)
      varPart = if vars == "" then "" else " " <> vars
      ctors = String.joinWith " | " (Array.fromFoldable (List.map renderDataCtor d.constructors))
  in "data " <> d.name <> varPart <> " = " <> ctors

-- | Render data type signature (for display)
renderDataTypeSig :: Ast.DataType -> String
renderDataTypeSig d =
  let vars = String.joinWith " " (Array.fromFoldable d.typeVars)
      varPart = if vars == "" then "" else " " <> vars
  in "data " <> d.name <> varPart

-- | Render a data constructor (DataField has label and ty)
renderDataCtor :: Ast.DataConstructor -> String
renderDataCtor ctor =
  let fields = String.joinWith " " (Array.fromFoldable (List.map (\df -> renderTypeExpr df.ty) ctor.fields))
  in if fields == "" then ctor.name else ctor.name <> " " <> fields

-- | Render a newtype to source
renderNewtype :: Ast.NewtypeDecl -> String
renderNewtype n =
  let vars = String.joinWith " " (Array.fromFoldable n.typeVars)
      varPart = if vars == "" then "" else " " <> vars
  in "newtype " <> n.name <> varPart <> " = " <> n.constructor <> " " <> renderTypeExpr n.wrappedType

-- | Render newtype signature (for display)
renderNewtypeSig :: Ast.NewtypeDecl -> String
renderNewtypeSig n =
  let vars = String.joinWith " " (Array.fromFoldable n.typeVars)
      varPart = if vars == "" then "" else " " <> vars
  in "newtype " <> n.name <> varPart

-- | Render a type alias to source
renderTypeAlias :: Ast.TypeAlias -> String
renderTypeAlias a =
  let vars = String.joinWith " " (Array.fromFoldable a.typeVars)
      varPart = if vars == "" then "" else " " <> vars
  in "type " <> a.name <> varPart <> " = " <> renderTypeExpr a.ty

-- | Render a foreign import to source (full)
renderForeign :: Ast.ForeignImport -> String
renderForeign f =
  "foreign import " <> f.functionName <> " :: " <> renderTypeExpr f.typeSignature

-- | Render a foreign import body (without type, for display)
renderForeignBody :: Ast.ForeignImport -> String
renderForeignBody f =
  "foreign import " <> f.functionName

-- | Render a type class to source
renderClass :: Ast.TypeClass -> String
renderClass c =
  let vars = String.joinWith " " (Array.fromFoldable c.typeVars)
      methods = String.joinWith "\n  " (Array.fromFoldable (List.map renderMethodSig c.methods))
  in "class " <> c.name <> " " <> vars <> " where\n  " <> methods

-- | Render a method signature
renderMethodSig :: Ast.TypeSignature -> String
renderMethodSig sig = sig.name <> " :: " <> renderTypeExpr sig.ty

-- | Render a type expression to source (using actual Ast.TypeExpr)
renderTypeExpr :: Ast.TypeExpr -> String
renderTypeExpr ty = case ty of
  Ast.TyExprCon name -> name
  Ast.TyExprVar name -> name
  Ast.TyExprApp t1 t2 -> renderTypeExpr t1 <> " " <> wrapTypeExpr t2
  Ast.TyExprArrow t1 t2 -> wrapTypeExpr t1 <> " -> " <> renderTypeExpr t2
  Ast.TyExprRecord fields maybeRow ->
    let fieldStr = renderRecordTypeFields fields
        rowStr = case maybeRow of
          Nothing -> ""
          Just r -> " | " <> r
    in "{ " <> fieldStr <> rowStr <> " }"
  Ast.TyExprForAll vars ty' ->
    "forall " <> String.joinWith " " (Array.fromFoldable vars) <> ". " <> renderTypeExpr ty'
  Ast.TyExprConstrained constraints ty' ->
    renderConstraints constraints <> " => " <> renderTypeExpr ty'
  Ast.TyExprParens t -> "(" <> renderTypeExpr t <> ")"
  Ast.TyExprTuple tys ->
    "(" <> String.joinWith ", " (Array.fromFoldable (List.map renderTypeExpr tys)) <> ")"

-- | Wrap type expression in parens if needed
wrapTypeExpr :: Ast.TypeExpr -> String
wrapTypeExpr ty = case ty of
  Ast.TyExprApp _ _ -> "(" <> renderTypeExpr ty <> ")"
  Ast.TyExprArrow _ _ -> "(" <> renderTypeExpr ty <> ")"
  _ -> renderTypeExpr ty

-- | Render record type fields (List (Tuple String TypeExpr))
renderRecordTypeFields :: List (Tuple String Ast.TypeExpr) -> String
renderRecordTypeFields fields =
  String.joinWith ", " (Array.fromFoldable (List.map renderTupleField fields))
  where
    renderTupleField (Tuple name t) = name <> " :: " <> renderTypeExpr t

-- | Render constraints
renderConstraints :: List Ast.Constraint -> String
renderConstraints cs =
  let rendered = Array.fromFoldable (List.map renderConstraint cs)
  in if Array.length rendered == 1
     then fromMaybe "" (Array.head rendered)
     else "(" <> String.joinWith ", " rendered <> ")"

-- | Render a single constraint (types not args)
renderConstraint :: Ast.Constraint -> String
renderConstraint c = c.className <> " " <> String.joinWith " " (Array.fromFoldable (List.map wrapTypeExpr c.types))

-- | Render a pattern (using actual Ast.Pattern)
renderPattern :: Ast.Pattern -> String
renderPattern pat = case pat of
  Ast.PatVar name -> name
  Ast.PatWildcard -> "_"
  Ast.PatLit lit -> renderLiteral lit
  Ast.PatCon name pats ->
    let args = String.joinWith " " (Array.fromFoldable (List.map wrapPattern pats))
    in if args == "" then name else "(" <> name <> " " <> args <> ")"
  Ast.PatRecord fields ->
    "{ " <> String.joinWith ", " (Array.fromFoldable (List.map renderPatTupleField fields)) <> " }"
  Ast.PatList pats ->
    "[" <> String.joinWith ", " (Array.fromFoldable (List.map renderPattern pats)) <> "]"
  Ast.PatCons h t -> renderPattern h <> " : " <> renderPattern t
  Ast.PatAs name pat' -> name <> "@" <> wrapPattern pat'
  Ast.PatParens p -> "(" <> renderPattern p <> ")"

-- | Wrap pattern in parens if needed
wrapPattern :: Ast.Pattern -> String
wrapPattern pat = case pat of
  Ast.PatCon _ ps -> if List.null ps then renderPattern pat else "(" <> renderPattern pat <> ")"
  _ -> renderPattern pat

-- | Render pattern record field (Tuple String Pattern)
renderPatTupleField :: Tuple String Ast.Pattern -> String
renderPatTupleField (Tuple name p) = name <> ": " <> renderPattern p

-- | Render an expression (using actual Ast.Expr)
renderExpr :: Ast.Expr -> String
renderExpr expr = case expr of
  Ast.ExprVar name -> name
  Ast.ExprQualified ns name -> ns <> "." <> name
  Ast.ExprLit lit -> renderLiteral lit
  Ast.ExprApp e1 e2 -> renderExpr e1 <> " " <> wrapExpr e2
  Ast.ExprLambda pats body ->
    "\\" <> String.joinWith " " (Array.fromFoldable (List.map renderPattern pats)) <> " -> " <> renderExpr body
  Ast.ExprLet binds body ->
    "let " <> renderBindings binds <> " in " <> renderExpr body
  Ast.ExprIf cond t f ->
    "if " <> renderExpr cond <> " then " <> renderExpr t <> " else " <> renderExpr f
  Ast.ExprCase e clauses ->
    "case " <> renderExpr e <> " of " <> renderClauses clauses
  Ast.ExprDo stmts -> "do " <> renderDoStmts stmts
  Ast.ExprBinOp op e1 e2 -> renderExpr e1 <> " " <> op <> " " <> renderExpr e2
  Ast.ExprUnaryOp op e -> op <> renderExpr e
  Ast.ExprList items ->
    "[" <> String.joinWith ", " (Array.fromFoldable (List.map renderExpr items)) <> "]"
  Ast.ExprTuple items ->
    "(" <> String.joinWith ", " (Array.fromFoldable (List.map renderExpr items)) <> ")"
  Ast.ExprRecord fields ->
    "{ " <> String.joinWith ", " (Array.fromFoldable (List.map renderExprTupleField fields)) <> " }"
  Ast.ExprRecordAccess e field -> renderExpr e <> "." <> field
  Ast.ExprRecordUpdate e updates ->
    renderExpr e <> " { " <> String.joinWith ", " (Array.fromFoldable (List.map renderUpdateTuple updates)) <> " }"
  Ast.ExprTyped e ty -> renderExpr e <> " :: " <> renderTypeExpr ty
  Ast.ExprParens e -> "(" <> renderExpr e <> ")"
  Ast.ExprSection s -> "(" <> s <> ")"
  Ast.ExprSectionLeft e op -> "(" <> renderExpr e <> " " <> op <> ")"
  Ast.ExprSectionRight op e -> "(" <> op <> " " <> renderExpr e <> ")"

-- | Wrap expression in parens if needed
wrapExpr :: Ast.Expr -> String
wrapExpr e = case e of
  Ast.ExprApp _ _ -> "(" <> renderExpr e <> ")"
  Ast.ExprLambda _ _ -> "(" <> renderExpr e <> ")"
  Ast.ExprLet _ _ -> "(" <> renderExpr e <> ")"
  Ast.ExprCase _ _ -> "(" <> renderExpr e <> ")"
  Ast.ExprIf _ _ _ -> "(" <> renderExpr e <> ")"
  Ast.ExprBinOp _ _ _ -> "(" <> renderExpr e <> ")"
  _ -> renderExpr e

-- | Render let bindings (List LetBind)
renderBindings :: List Ast.LetBind -> String
renderBindings binds =
  String.joinWith "; " (Array.fromFoldable (List.map renderBinding binds))

-- | Render a single let binding (LetBind is a record)
renderBinding :: Ast.LetBind -> String
renderBinding bind =
  renderPattern bind.pattern <> " = " <> renderExpr bind.value

-- | Render case clauses (List CaseClause)
renderClauses :: List Ast.CaseClause -> String
renderClauses clauses =
  String.joinWith "; " (Array.fromFoldable (List.map renderClause clauses))

-- | Render a single case clause
renderClause :: Ast.CaseClause -> String
renderClause clause =
  let guardPart = case clause.guard of
        Nothing -> ""
        Just g -> " | " <> renderExpr g
  in renderPattern clause.pattern <> guardPart <> " -> " <> renderExpr clause.body

-- | Render do statements
renderDoStmts :: List Ast.DoStatement -> String
renderDoStmts stmts =
  String.joinWith "; " (Array.fromFoldable (List.map renderDoStmt stmts))

-- | Render a single do statement
renderDoStmt :: Ast.DoStatement -> String
renderDoStmt stmt = case stmt of
  Ast.DoLet binds -> "let " <> renderBindings binds
  Ast.DoBind pat e -> renderPattern pat <> " <- " <> renderExpr e
  Ast.DoExpr e -> renderExpr e

-- | Render expression record field (Tuple String Expr)
renderExprTupleField :: Tuple String Ast.Expr -> String
renderExprTupleField (Tuple name v) = name <> ": " <> renderExpr v

-- | Render record update (Tuple String Expr)
renderUpdateTuple :: Tuple String Ast.Expr -> String
renderUpdateTuple (Tuple name v) = name <> " = " <> renderExpr v

-- | Render a literal
renderLiteral :: Ast.Literal -> String
renderLiteral lit = case lit of
  Ast.LitInt n -> showInt n
  Ast.LitNumber n -> showNum n
  Ast.LitString s -> "\"" <> escapeString s <> "\""
  Ast.LitChar c -> "'" <> String.singleton (Char.toCharCode c) <> "'"
  Ast.LitBool b -> if b then "true" else "false"

-- | Show Int as binary string
foreign import showInt :: Int -> String
  = "call 'erlang':'integer_to_binary'($0)"

-- | Show Number as binary string
foreign import showNum :: Number -> String
  = "call 'erlang':'iolist_to_binary'(call 'io_lib':'format'([126,112], [$0]))"

-- | Escape string for rendering
escapeString :: String -> String
escapeString s = s -- TODO: proper escaping

-- | Read a file from the filesystem
foreign import readFile :: String -> Maybe String
  = "case call 'file':'read_file'($0) of <{'ok', Content}> when 'true' -> {'Just', Content} <_> when 'true' -> 'Nothing' end"

-- | Import a module from a file path
importFile :: String -> Either String { moduleName :: String, declCount :: Int }
importFile path =
  case readFile path of
    Nothing -> Left ("Cannot read file: " <> path)
    Just source -> importModule source

-- | Import all .purs files from a directory (non-recursive)
-- | Returns array of results
foreign import listPursFiles :: String -> Array String
  = "let <Pattern> = call 'erlang':'iolist_to_binary'([$0, #{}#, [42,46,112,117,114,115]]) in call 'filelib':'wildcard'(Pattern)"

-- | Import all library modules from a directory
importDirectory :: String -> Array (Either String { moduleName :: String, declCount :: Int })
importDirectory dir =
  let files = listPursFiles dir
  in Array.map importFile files

-- | Start the web server on a given port
-- Uses Nova.HTTPServer Elixir wrapper
startServer :: Int -> Unit
startServer port =
  let _w = initServiceIfNeeded unit
  in startServerImpl port

foreign import startServerImpl :: Int -> Unit
  = "call 'Elixir.Nova.HTTPServer':'start'($0, 'Nova.Web')"

-- | HTTP handler - called by Nova.HTTPServer
handle :: String -> String -> String -> { status :: Int, contentType :: String, body :: String }
handle method path body =
  if method == "GET"
  then handleGet path
  else if method == "POST"
  then handlePost path body
  else { status: 404, contentType: "text/plain", body: "Not Found" }

-- | Handle GET requests
handleGet :: String -> { status :: Int, contentType :: String, body :: String }
handleGet path =
  if path == "/" || path == "/index.html"
  then { status: 200, contentType: "text/html", body: indexPage }
  else if path == "/eval"
  then { status: 200, contentType: "text/html", body: evalPage }
  else if path == "/namespaces"
  then { status: 200, contentType: "text/html", body: namespacesPage unit }
  else if String.take 11 path == "/namespace/"
  then
    let nsName = String.drop 11 path
    in { status: 200, contentType: "text/html", body: namespaceDetailPage nsName }
  else { status: 404, contentType: "text/html", body: notFoundPage }

-- | Handle POST requests
handlePost :: String -> String -> { status :: Int, contentType :: String, body :: String }
handlePost path body =
  if path == "/api/eval"
  then handleEvalPost body
  else if path == "/api/namespace/create"
  then handleCreateNamespace body
  else if path == "/api/namespace/delete"
  then handleDeleteNamespace body
  else if path == "/api/decl/add"
  then handleAddDecl body
  else { status: 404, contentType: "application/json", body: "{\"error\":\"Not Found\"}" }

-- | Extract field from JSON using Json module
extractJsonField :: String -> String -> String
extractJsonField jsonStr field =
  case Json.decode jsonStr of
    Nothing -> ""
    Just obj -> fromMaybe "" (Json.getString obj field)

-- | Handle POST /api/eval
handleEvalPost :: String -> { status :: Int, contentType :: String, body :: String }
handleEvalPost body =
  let action = extractJsonField body "action"
      code = extractJsonField body "code"
      result = if action == "eval_expr"
               then case Eval.eval code of
                 Right r -> "{\"result\":\"" <> escapeJson r <> "\"}"
                 Left e -> "{\"error\":\"" <> escapeJson e <> "\"}"
               else if action == "compile_to_core"
               then case Eval.compileToCore code of
                 Right r -> "{\"result\":\"" <> escapeJson r <> "\"}"
                 Left e -> "{\"error\":\"" <> escapeJson e <> "\"}"
               else "{\"error\":\"Unknown action\"}"
  in { status: 200, contentType: "application/json", body: result }

-- | Handle POST /api/namespace/create
handleCreateNamespace :: String -> { status :: Int, contentType :: String, body :: String }
handleCreateNamespace body =
  let name = extractJsonField body "name"
  in if name == ""
     then { status: 400, contentType: "application/json", body: "{\"error\":\"Name required\"}" }
     else case createNamespace name of
       Right _ -> { status: 200, contentType: "application/json", body: "{\"success\":true}" }
       Left err -> { status: 400, contentType: "application/json", body: "{\"error\":\"" <> escapeJson err <> "\"}" }

-- | Handle POST /api/namespace/delete
handleDeleteNamespace :: String -> { status :: Int, contentType :: String, body :: String }
handleDeleteNamespace body =
  let name = extractJsonField body "name"
  in case deleteNamespace name of
       Right _ -> { status: 200, contentType: "application/json", body: "{\"success\":true}" }
       Left err -> { status: 400, contentType: "application/json", body: "{\"error\":\"" <> escapeJson err <> "\"}" }

-- | Handle POST /api/decl/add
handleAddDecl :: String -> { status :: Int, contentType :: String, body :: String }
handleAddDecl body =
  let namespace = extractJsonField body "namespace"
      name = extractJsonField body "name"
      source = extractJsonField body "source"
      kindStr = extractJsonField body "kind"
      kind = if kindStr == "datatype" then NS.DatatypeDecl
             else if kindStr == "typealias" then NS.TypeAliasDecl
             else if kindStr == "foreign" then NS.ForeignDecl
             else NS.FunctionDecl
  in case addDecl namespace name source kind of
       Right declId -> { status: 200, contentType: "application/json", body: "{\"declId\":\"" <> escapeJson declId <> "\"}" }
       Left err -> { status: 400, contentType: "application/json", body: "{\"error\":\"" <> escapeJson err <> "\"}" }

-- | Escape JSON string
foreign import escapeJson :: String -> String
  = "call 'erlang':'iolist_to_binary'(call 'lists':'map'(fun (C) -> case C of <34> when 'true' -> [92,34] <92> when 'true' -> [92,92] <10> when 'true' -> [92,110] <13> when 'true' -> [92,114] <9> when 'true' -> [92,116] <_> when 'true' -> [C] end, call 'unicode':'characters_to_list'($0)))"

-- | CSS styles
styles :: String
styles = "<style>*{box-sizing:border-box;margin:0;padding:0}body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif;background:#1a1a2e;color:#eee;min-height:100vh;padding:2rem}.hero{text-align:center;padding:3rem 0}.hero h1{font-size:3rem;color:#00d4ff;margin-bottom:.5rem}.hero p{font-size:1.2rem;color:#888}.nav-cards{display:flex;gap:1rem;justify-content:center;flex-wrap:wrap;margin-top:2rem}.card{background:#252540;padding:1.5rem 2rem;border-radius:8px;text-decoration:none;color:#eee;transition:transform .2s,background .2s;border:1px solid #333}.card:hover{transform:translateY(-2px);background:#303050;border-color:#00d4ff}.card h2{color:#00d4ff;margin-bottom:.5rem}.card p{color:#888;font-size:.9rem}h1{color:#00d4ff;margin-bottom:1rem}h2{color:#00d4ff;margin:1.5rem 0 1rem}h3.section-title{color:#888;font-size:.9rem;text-transform:uppercase;letter-spacing:1px;margin:1.5rem 0 .75rem;padding-bottom:.5rem;border-bottom:1px solid #333}.repl-container{display:grid;grid-template-columns:1fr 1fr;gap:1rem;margin-top:1rem}.input-section,.output-section{background:#252540;padding:1rem;border-radius:8px}label{display:block;margin-bottom:.5rem;color:#888}textarea,input[type=text]{width:100%;background:#1a1a2e;color:#eee;border:1px solid #444;border-radius:4px;padding:.75rem;font-family:Monaco,Menlo,monospace;font-size:14px}textarea{resize:vertical}textarea:focus,input:focus{outline:none;border-color:#00d4ff}.buttons{margin-top:1rem;display:flex;gap:.5rem;flex-wrap:wrap}button{background:#00d4ff;color:#1a1a2e;border:none;padding:.5rem 1rem;border-radius:4px;cursor:pointer;font-weight:600;transition:background .2s}button:hover{background:#00b8e6}button.danger{background:#ff4757}button.danger:hover{background:#ff3344}pre{background:#1a1a2e;padding:1rem;border-radius:4px;overflow-x:auto;font-family:Monaco,Menlo,monospace;font-size:14px;white-space:pre-wrap;min-height:100px}.ns-list{display:grid;gap:1rem;margin-top:1rem}.ns-item{background:#252540;padding:1rem;border-radius:8px;display:flex;justify-content:space-between;align-items:center;border:1px solid #333}.ns-item:hover{border-color:#444}.ns-item a{color:#00d4ff;text-decoration:none;font-size:1.1rem}.ns-item a:hover{text-decoration:underline}.ns-item .actions{display:flex;gap:.5rem}.decl-list{margin-top:1rem}.decl-item{background:#252540;padding:.75rem 1rem;border-radius:6px;margin-bottom:.5rem;border-left:3px solid #00d4ff}.decl-item.kind-data{border-left-color:#9b59b6}.decl-item.kind-type{border-left-color:#3498db}.decl-item.kind-function{border-left-color:#2ecc71}.decl-item.kind-foreign{border-left-color:#e67e22}.decl-name{font-weight:600;color:#fff;font-size:1rem;margin-bottom:.25rem}.decl-sig{margin-bottom:.5rem}.decl-sig code{color:#00d4ff;font-family:Monaco,Menlo,monospace;font-size:.85rem;background:#1a1a2e;padding:.2rem .4rem;border-radius:3px}.decl-source{margin:0;font-size:.8rem;max-height:120px;overflow:auto;min-height:auto;padding:.5rem;background:#1a1a2e;border-radius:4px}.form-group{margin-bottom:1rem}.back-link{color:#888;text-decoration:none;display:inline-block;margin-bottom:1rem}.back-link:hover{color:#00d4ff}.empty{color:#888;text-align:center;padding:2rem}</style>"

-- | HTML wrapper
htmlPage :: String -> String -> String
htmlPage title content =
  "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>" <> title <> "</title>" <> styles <> "</head><body>" <> content <> "</body></html>"

-- | Index page
indexPage :: String
indexPage = htmlPage "Nova Lang" "<div class=\"hero\"><h1>Nova Lang</h1><p>A PureScript dialect that compiles to Core Erlang</p></div><div class=\"nav-cards\"><a href=\"/namespaces\" class=\"card\"><h2>Namespaces</h2><p>Browse and manage code namespaces</p></a><a href=\"/eval\" class=\"card\"><h2>REPL</h2><p>Evaluate Nova expressions interactively</p></a></div>"

-- | Namespaces page
namespacesPage :: Unit -> String
namespacesPage _u =
  let namespaces = listNamespaces unit
      nsListHtml = if Array.null namespaces
                   then "<div class=\"empty\">No namespaces yet. Create one to get started.</div>"
                   else String.joinWith "" (Array.map renderNsItem namespaces)
  in htmlPage "Namespaces - Nova" ("<a href=\"/\" class=\"back-link\">&larr; Home</a><h1>Namespaces</h1><div class=\"form-group\"><input type=\"text\" id=\"nsName\" placeholder=\"Namespace name (e.g. MyModule)\"><button onclick=\"createNs()\" style=\"margin-left:.5rem\">Create Namespace</button></div><div class=\"ns-list\">" <> nsListHtml <> "</div><script>async function createNs(){const n=document.getElementById('nsName').value;if(!n)return alert('Enter a name');const r=await fetch('/api/namespace/create',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({name:n})});const d=await r.json();if(d.error)alert(d.error);else location.reload()}async function deleteNs(n){if(!confirm('Delete namespace '+n+'?'))return;const r=await fetch('/api/namespace/delete',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({name:n})});const d=await r.json();if(d.error)alert(d.error);else location.reload()}</script>")

renderNsItem :: String -> String
renderNsItem name =
  "<div class=\"ns-item\"><a href=\"/namespace/" <> name <> "\">" <> name <> "</a><div class=\"actions\"><button class=\"danger\" onclick=\"deleteNs('" <> name <> "')\">Delete</button></div></div>"

-- | Namespace detail page
namespaceDetailPage :: String -> String
namespaceDetailPage nsName =
  if not (namespaceExists nsName)
  then htmlPage "Not Found - Nova" "<a href=\"/namespaces\" class=\"back-link\">&larr; Namespaces</a><h1>Namespace Not Found</h1><p>The namespace \"" <> nsName <> "\" does not exist.</p>"
  else
    let decls = getNamespaceDecls nsName
        -- Sort by kind then name: data types, type aliases, functions, foreign
        sortedDecls = sortDeclarations decls
        -- Group by kind for display
        dataDecls = Array.filter (\d -> kindEq d.kind NS.DatatypeDecl) sortedDecls
        typeDecls = Array.filter (\d -> kindEq d.kind NS.TypeAliasDecl) sortedDecls
        funcDecls = Array.filter (\d -> kindEq d.kind NS.FunctionDecl) sortedDecls
        foreignDecls = Array.filter (\d -> kindEq d.kind NS.ForeignDecl) sortedDecls
        -- Render each section
        dataSection = if Array.null dataDecls then ""
                      else "<h3 class=\"section-title\">Data Types</h3>" <> String.joinWith "" (Array.map renderDeclItem dataDecls)
        typeSection = if Array.null typeDecls then ""
                      else "<h3 class=\"section-title\">Type Aliases</h3>" <> String.joinWith "" (Array.map renderDeclItem typeDecls)
        funcSection = if Array.null funcDecls then ""
                      else "<h3 class=\"section-title\">Functions</h3>" <> String.joinWith "" (Array.map renderDeclItem funcDecls)
        foreignSection = if Array.null foreignDecls then ""
                         else "<h3 class=\"section-title\">Foreign Imports</h3>" <> String.joinWith "" (Array.map renderDeclItem foreignDecls)
        declsHtml = if Array.null decls
                    then "<div class=\"empty\">No declarations in this namespace.</div>"
                    else dataSection <> typeSection <> funcSection <> foreignSection
    in htmlPage (nsName <> " - Nova") ("<a href=\"/namespaces\" class=\"back-link\">&larr; Namespaces</a><h1>" <> nsName <> "</h1><div class=\"decl-list\">" <> declsHtml <> "</div><h2>Add Declaration</h2><div class=\"form-group\"><input type=\"text\" id=\"declName\" placeholder=\"Declaration name\" style=\"margin-bottom:.5rem\"><br><select id=\"declKind\" style=\"background:#1a1a2e;color:#eee;border:1px solid #444;padding:.5rem;border-radius:4px\"><option value=\"function\">Function</option><option value=\"datatype\">Data Type</option><option value=\"typealias\">Type Alias</option><option value=\"foreign\">Foreign</option></select></div><div class=\"form-group\"><textarea id=\"declSource\" rows=\"4\" placeholder=\"add :: Int -> Int -> Int\\nadd x y = x + y\"></textarea></div><button onclick=\"addDecl()\">Add Declaration</button><script>function toggleSource(id){var e=document.getElementById('src-'+id);e.style.display=e.style.display==='none'?'block':'none'}async function addDecl(){const n=document.getElementById('declName').value;const s=document.getElementById('declSource').value;const k=document.getElementById('declKind').value;if(!n||!s)return alert('Name and source required');const r=await fetch('/api/decl/add',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({namespace:'" <> nsName <> "',name:n,source:s,kind:k})});const d=await r.json();if(d.error)alert(d.error);else location.reload()}</script>")

-- | Sort declarations by kind then name
sortDeclarations :: Array NS.ManagedDecl -> Array NS.ManagedDecl
sortDeclarations decls = sortByNameImpl decls

-- | Kind equality helper
kindEq :: NS.DeclKind -> NS.DeclKind -> Boolean
kindEq NS.FunctionDecl NS.FunctionDecl = true
kindEq NS.DatatypeDecl NS.DatatypeDecl = true
kindEq NS.TypeAliasDecl NS.TypeAliasDecl = true
kindEq NS.ForeignDecl NS.ForeignDecl = true
kindEq _ _ = false

-- | Sort declarations by name (FFI for simplicity)
-- Note: Nova records are Erlang maps, not tuples, so use maps:get
foreign import sortByNameImpl :: Array NS.ManagedDecl -> Array NS.ManagedDecl
  = "call 'lists':'sort'(fun (A, B) -> call 'erlang':'<'(call 'maps':'get'('name', A), call 'maps':'get'('name', B)), $0)"

renderDeclItem :: NS.ManagedDecl -> String
renderDeclItem decl =
  let kindClass = case decl.kind of
        NS.FunctionDecl -> "kind-function"
        NS.DatatypeDecl -> "kind-data"
        NS.TypeAliasDecl -> "kind-type"
        NS.ForeignDecl -> "kind-foreign"
      -- Show signature
      sigHtml = case decl.inferredType of
        Nothing -> ""
        Just t -> "<div class=\"decl-sig\"><code>" <> escapeHtml t <> "</code></div>"
      -- For functions/foreign: hidden source with click to expand
      -- For others: always show source
      sourceHtml = case decl.kind of
        NS.FunctionDecl ->
          "<pre class=\"decl-source\" style=\"display:none\" id=\"src-" <> decl.declId <> "\">" <> escapeHtml decl.sourceText <> "</pre>"
        NS.ForeignDecl ->
          "<pre class=\"decl-source\" style=\"display:none\" id=\"src-" <> decl.declId <> "\">" <> escapeHtml decl.sourceText <> "</pre>"
        _ -> "<pre class=\"decl-source\">" <> escapeHtml decl.sourceText <> "</pre>"
      -- Add click handler for functions/foreign
      clickAttr = case decl.kind of
        NS.FunctionDecl -> " onclick=\"toggleSource('" <> decl.declId <> "')\" style=\"cursor:pointer\""
        NS.ForeignDecl -> " onclick=\"toggleSource('" <> decl.declId <> "')\" style=\"cursor:pointer\""
        _ -> ""
  in "<div class=\"decl-item " <> kindClass <> "\"" <> clickAttr <> "><div class=\"decl-name\">" <> decl.name <> "</div>" <> sigHtml <> sourceHtml <> "</div>"

-- | Escape HTML entities
foreign import escapeHtml :: String -> String
  = "call 'erlang':'iolist_to_binary'(call 'lists':'map'(fun (C) -> case C of <60> when 'true' -> [38,108,116,59] <62> when 'true' -> [38,103,116,59] <38> when 'true' -> [38,97,109,112,59] <34> when 'true' -> [38,113,117,111,116,59] <_> when 'true' -> [C] end, call 'unicode':'characters_to_list'($0)))"

-- | REPL page
evalPage :: String
evalPage = htmlPage "Nova REPL" "<a href=\"/\" class=\"back-link\">&larr; Home</a><h1>Nova REPL</h1><div class=\"repl-container\"><div class=\"input-section\"><label>Enter Nova code:</label><textarea id=\"code\" rows=\"10\" placeholder=\"1 + 2 * 3\"></textarea><div class=\"buttons\"><button onclick=\"evalCode()\">Evaluate</button><button onclick=\"showCore()\">Show Core Erlang</button></div></div><div class=\"output-section\"><label>Result:</label><pre id=\"result\">-- Output will appear here</pre></div></div><script>async function evalCode(){const c=document.getElementById('code').value;const r=await fetch('/api/eval',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({action:'eval_expr',code:c})});const d=await r.json();document.getElementById('result').textContent=d.result||d.error}async function showCore(){const c=document.getElementById('code').value;const r=await fetch('/api/eval',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({action:'compile_to_core',code:c})});const d=await r.json();document.getElementById('result').textContent=d.result||d.error}</script>"

-- | 404 page
notFoundPage :: String
notFoundPage = htmlPage "Not Found" "<h1>404 - Not Found</h1><p><a href=\"/\">Go home</a></p>"
