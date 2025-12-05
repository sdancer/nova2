module Nova.Compiler.CstToAst where

import Prelude
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Nova.Compiler.Cst as Cst
import Nova.Compiler.Ast as Ast

-- ============================================================================
-- Expanded Type Aliases (for self-hosted type checker compatibility)
-- ============================================================================
-- The self-hosted type checker doesn't expand parameterized type aliases,
-- so we define concrete type aliases for the specific instantiations used.

-- Separated a = { head :: a, tail :: List (Tuple SourceToken a) }
type SeparatedDataCtor = { head :: Cst.DataCtor Void, tail :: List (Tuple Cst.SourceToken (Cst.DataCtor Void)) }
type SeparatedInstance = { head :: Cst.Instance Void, tail :: List (Tuple Cst.SourceToken (Cst.Instance Void)) }
type SeparatedPatternGuard = { head :: Cst.PatternGuard Void, tail :: List (Tuple Cst.SourceToken (Cst.PatternGuard Void)) }
type SeparatedBinder = { head :: Cst.Binder Void, tail :: List (Tuple Cst.SourceToken (Cst.Binder Void)) }

-- Labeled a b = { label :: a, separator :: SourceToken, value :: b }
type LabeledIdentType = { label :: Cst.Name Cst.Ident, separator :: Cst.SourceToken, value :: Cst.Type Void }

-- Wrapped a = { open :: SourceToken, value :: a, close :: SourceToken }
type WrappedExpr = { open :: Cst.SourceToken, value :: Cst.Expr Void, close :: Cst.SourceToken }

-- ImportDecl e = { keyword :: SourceToken, module :: Name ModuleName, names :: Maybe ..., qualified :: Maybe ... }
type CstImportDecl = Cst.ImportDecl Void

-- Separated (Import Void)
type CstSeparatedImport = { head :: Cst.Import Void, tail :: List (Tuple Cst.SourceToken (Cst.Import Void)) }

-- ============================================================================
-- Helper Functions (defined early for self-hosted type checker)
-- ============================================================================

unwrapIdent :: Cst.Ident -> String
unwrapIdent (Cst.Ident s) = s

unwrapProper :: Cst.Proper -> String
unwrapProper (Cst.Proper s) = s

unwrapLabel :: Cst.Label -> String
unwrapLabel (Cst.Label s) = s

unwrapOperator :: Cst.Operator -> String
unwrapOperator (Cst.Operator s) = s

unwrapModuleName :: Cst.ModuleName -> String
unwrapModuleName (Cst.ModuleName s) = s

extractTypeVar :: Cst.TypeVarBinding Void -> String
extractTypeVar (Cst.TypeVarKinded wrapped) = unwrapIdent wrapped.value.label.name
extractTypeVar (Cst.TypeVarName name) = unwrapIdent name.name

intValueToInt :: Cst.IntValue -> Int
intValueToInt (Cst.SmallInt n) = n
intValueToInt (Cst.BigInt _) = 0  -- TODO: Handle big ints properly

snd :: forall a b. Tuple a b -> b
snd (Tuple _ b) = b

traverse :: forall a b. (a -> Either String b) -> List a -> Either String (List b)
traverse f lst = case lst of
  Nil -> Right Nil
  (head : tail) -> do
    h <- f head
    t <- traverse f tail
    pure $ h : t

-- | Map with index for Lists
listMapWithIndex :: forall a b. (Int -> a -> b) -> List a -> List b
listMapWithIndex f list = go 0 list
  where
  go _ Nil = Nil
  go i (x : xs) = f i x : go (i + 1) xs

-- | Check if an expression is the underscore placeholder (for case _ of)
isUnderscore :: Cst.Expr Void -> Boolean
isUnderscore (Cst.ExprIdent qn) = unwrapIdent qn.name == "_"
isUnderscore _ = false

-- ============================================================================
-- CST to AST Conversion
-- ============================================================================

-- | Convert a CST Module to an AST Module
convertModule :: Cst.Module Void -> Either String Ast.Module
convertModule cstMod = do
  let moduleName = unwrapModuleName cstMod.header.name.name
  imports <- traverse convertImportDecl cstMod.header.imports
  decls <- convertDeclarations cstMod.body.decls
  let importDecls = map Ast.DeclImport imports
  pure { name: moduleName, declarations: importDecls <> decls }

-- | Convert a CST ImportDecl to AST ImportDeclaration
convertImportDecl :: CstImportDecl -> Either String Ast.ImportDeclaration
convertImportDecl imp = do
  let moduleName = unwrapModuleName imp.module.name
  let alias = map (\(Tuple _ qualName) -> unwrapModuleName qualName.name) imp.qualified
  items <- case imp.names of
    Nothing -> pure Nil  -- Implicit import of everything
    Just (Tuple _ delim) -> do
      importItems <- convertImportItems delim.value
      pure importItems
  let isHiding = case imp.names of
        Just (Tuple (Just _) _) -> true
        _ -> false
  pure { moduleName, alias, items, hiding: isHiding }

-- | Convert CST Import items to AST ImportItems
convertImportItems :: CstSeparatedImport -> Either String (List Ast.ImportItem)
convertImportItems sep = do
  let allImports = sep.head : (map (\(Tuple _ i) -> i) sep.tail)
  traverse convertImportItem allImports

-- | Convert a single CST Import to AST ImportItem
convertImportItem :: Cst.Import Void -> Either String Ast.ImportItem
convertImportItem imp = case imp of
  Cst.ImportValue name ->
    pure $ Ast.ImportValue (unwrapIdent name.name)
  Cst.ImportOp name ->
    pure $ Ast.ImportValue (unwrapOperator name.name)
  Cst.ImportType name mMembers ->
    let typeName = unwrapProper name.name
        spec = convertDataMembers mMembers
    in pure $ Ast.ImportType typeName spec
  Cst.ImportTypeOp _ name ->
    pure $ Ast.ImportValue (unwrapOperator name.name)
  Cst.ImportClass _ name ->
    pure $ Ast.ImportType (unwrapProper name.name) Ast.ImportNone
  Cst.ImportError _ ->
    Left "Cannot convert import error"

-- | Convert DataMembers to ImportSpec
convertDataMembers :: Maybe Cst.DataMembers -> Ast.ImportSpec
convertDataMembers Nothing = Ast.ImportNone
convertDataMembers (Just (Cst.DataAll _)) = Ast.ImportAll
convertDataMembers (Just (Cst.DataEnumerated delim)) =
  case delim.value of
    Nothing -> Ast.ImportNone
    Just sep ->
      let names = (unwrapProper sep.head.name) : (map (\(Tuple _ n) -> unwrapProper n.name) sep.tail)
      in Ast.ImportSome names

-- | Convert list of CST declarations to AST declarations
convertDeclarations :: List (Cst.Declaration Void) -> Either String (List Ast.Declaration)
convertDeclarations decls = traverse convertDeclaration decls

-- | Convert a single CST declaration to AST
convertDeclaration :: Cst.Declaration Void -> Either String Ast.Declaration
convertDeclaration decl = case decl of
  Cst.DeclData head ctors ->
    convertDataType head ctors

  Cst.DeclType head _ ty ->
    convertTypeAlias head ty

  Cst.DeclNewtype head _ ctorName wrappedTy ->
    convertNewtype head ctorName wrappedTy

  Cst.DeclClass head methods ->
    convertTypeClass head methods

  Cst.DeclInstanceChain chain ->
    convertInstance chain

  Cst.DeclDerive _ _ instHead ->
    convertDeriveInstance instHead

  Cst.DeclSignature labeled ->
    convertTypeSignature labeled

  Cst.DeclValue vbf ->
    convertFunction vbf

  Cst.DeclFixity fields ->
    convertFixity fields

  Cst.DeclForeign _ _ foreign' ->
    convertForeign foreign'

  Cst.DeclError _ ->
    Left "Cannot convert error declaration"

-- ============================================================================
-- Data Type Conversion
-- ============================================================================

convertDataType :: Cst.DataHead Void -> Maybe (Tuple Cst.SourceToken SeparatedDataCtor) -> Either String Ast.Declaration
convertDataType head mCtors = do
  let name = unwrapProper head.name.name
  let typeVars = map extractTypeVar head.vars
  ctors <- case mCtors of
    Nothing -> pure Nil
    Just (Tuple _ separated) -> convertDataCtors separated
  pure $ Ast.DeclDataType { name, typeVars, constructors: ctors }

convertDataCtors :: SeparatedDataCtor -> Either String (List Ast.DataConstructor)
convertDataCtors sep = do
  let allCtors = sep.head : (map (\(Tuple _ c) -> c) sep.tail)
  traverse convertDataCtor allCtors

convertDataCtor :: Cst.DataCtor Void -> Either String Ast.DataConstructor
convertDataCtor ctor = do
  let name = unwrapProper ctor.name.name
  fields <- traverse convertTypeToField ctor.fields
  pure { name, fields, isRecord: false }

convertTypeToField :: Cst.Type Void -> Either String Ast.DataField
convertTypeToField ty = do
  tyExpr <- convertType ty
  pure { label: "", ty: tyExpr }

-- ============================================================================
-- Type Alias Conversion
-- ============================================================================

convertTypeAlias :: Cst.DataHead Void -> Cst.Type Void -> Either String Ast.Declaration
convertTypeAlias head ty = do
  let name = unwrapProper head.name.name
  let typeVars = map extractTypeVar head.vars
  tyExpr <- convertType ty
  pure $ Ast.DeclTypeAlias { name, typeVars, ty: tyExpr }

-- ============================================================================
-- Newtype Conversion
-- ============================================================================

convertNewtype :: Cst.DataHead Void -> Cst.Name Cst.Proper -> Cst.Type Void -> Either String Ast.Declaration
convertNewtype head ctorName wrappedTy = do
  let name = unwrapProper head.name.name
  let typeVars = map extractTypeVar head.vars
  let constructor = unwrapProper ctorName.name
  wrappedType <- convertType wrappedTy
  pure $ Ast.DeclNewtype { name, typeVars, constructor, wrappedType }

-- ============================================================================
-- Type Class Conversion
-- ============================================================================

-- Note: Expanding Labeled type alias for self-hosted type checker
-- Labeled a b = { label :: a, separator :: SourceToken, value :: b }
convertTypeClass :: Cst.ClassHead Void -> Maybe (Tuple Cst.SourceToken (List { label :: Cst.Name Cst.Ident, separator :: Cst.SourceToken, value :: Cst.Type Void })) -> Either String Ast.Declaration
convertTypeClass head mMethods = do
  let name = unwrapProper head.name.name
  let typeVars = map extractTypeVar head.vars
  methods <- case mMethods of
    Nothing -> pure Nil
    Just (Tuple _ labeled) -> traverse convertMethodSig labeled
  pure $ Ast.DeclTypeClass { name, typeVars, methods, kind: Nothing }

convertMethodSig :: { label :: Cst.Name Cst.Ident, separator :: Cst.SourceToken, value :: Cst.Type Void } -> Either String Ast.TypeSignature
convertMethodSig labeled = do
  let name = unwrapIdent labeled.label.name
  ty <- convertType labeled.value
  pure { name, typeVars: Nil, constraints: Nil, ty }

-- ============================================================================
-- Instance Conversion
-- ============================================================================

convertInstance :: SeparatedInstance -> Either String Ast.Declaration
convertInstance chain = do
  -- Just convert the head instance for now
  let inst = chain.head
  let className = unwrapProper inst.head.className.name
  -- Get the first type argument as the instance type
  ty <- case inst.head.types of
    (t : _) -> convertType t
    Nil -> Left "Instance needs at least one type"
  methods <- case inst.body of
    Nothing -> pure Nil
    Just (Tuple _ bindings) -> traverse convertInstanceBinding bindings
  pure $ Ast.DeclTypeClassInstance { className, ty, methods, derived: false }

convertInstanceBinding :: Cst.InstanceBinding Void -> Either String Ast.FunctionDeclaration
convertInstanceBinding binding = case binding of
  Cst.InstanceBindingSignature _ -> Left "Type signatures in instances not yet supported"
  Cst.InstanceBindingName vbf -> convertValueBinding vbf

convertDeriveInstance :: Cst.InstanceHead Void -> Either String Ast.Declaration
convertDeriveInstance head = do
  let className = unwrapProper head.className.name
  ty <- case head.types of
    (t : _) -> convertType t
    Nil -> Left "Derived instance needs at least one type"
  pure $ Ast.DeclTypeClassInstance { className, ty, methods: Nil, derived: true }

-- ============================================================================
-- Function/Value Conversion
-- ============================================================================

convertFunction :: Cst.ValueBindingFields Void -> Either String Ast.Declaration
convertFunction vbf = do
  func <- convertValueBinding vbf
  pure $ Ast.DeclFunction func

convertValueBinding :: Cst.ValueBindingFields Void -> Either String Ast.FunctionDeclaration
convertValueBinding vbf = do
  let name = unwrapIdent vbf.name.name
  params <- traverse convertBinder vbf.binders
  case vbf.guarded of
    Cst.Unconditional _ wh -> do
      body <- convertWhereExpr wh
      pure { name, parameters: params, body, guards: Nil, typeSignature: Nothing }
    Cst.Guarded guardedExprs -> do
      -- For guarded expressions, use the first one as body and rest as guards
      guards <- traverse convertGuardedExpr guardedExprs
      case guards of
        (g : _) -> pure { name, parameters: params, body: g.body, guards, typeSignature: Nothing }
        Nil -> Left "Empty guarded expression"

-- | Convert a Where clause, wrapping the expression in ExprLet if there are bindings
convertWhereExpr :: Cst.Where Void -> Either String Ast.Expr
convertWhereExpr wh = do
  expr <- convertExpr wh.expr
  case wh.bindings of
    Nothing -> pure expr
    Just (Tuple _ bindings) -> do
      letBinds <- convertWhereBindings bindings
      pure $ Ast.ExprLet letBinds expr

-- | Convert where clause bindings to let bindings
convertWhereBindings :: List (Cst.LetBinding Void) -> Either String (List Ast.LetBind)
convertWhereBindings bindings = do
  -- Filter out type signatures and convert the rest
  let valueBindings = List.mapMaybe getValueBinding bindings
  traverse convertLetBind valueBindings
  where
    getValueBinding :: Cst.LetBinding Void -> Maybe (Cst.ValueBindingFields Void)
    getValueBinding (Cst.LetBindingName vbf) = Just vbf
    getValueBinding _ = Nothing

    convertLetBind :: Cst.ValueBindingFields Void -> Either String Ast.LetBind
    convertLetBind vbf = do
      let bindName = unwrapIdent vbf.name.name
      bindParams <- traverse convertBinder vbf.binders
      case vbf.guarded of
        Cst.Unconditional _ wh -> do
          bindBody <- convertWhereExpr wh
          -- If there are parameters, wrap in a lambda
          let value = case bindParams of
                Nil -> bindBody
                _ -> Ast.ExprLambda bindParams bindBody
          pure { pattern: Ast.PatVar bindName, value, typeAnn: Nothing }
        Cst.Guarded _ -> Left "Guarded where bindings not yet supported"

convertGuardedExpr :: Cst.GuardedExpr Void -> Either String Ast.GuardedExpr
convertGuardedExpr ge = do
  guards <- convertPatternGuards ge.patterns
  body <- convertWhereExpr ge.where
  pure { guards, body }

convertPatternGuards :: SeparatedPatternGuard -> Either String (List Ast.GuardClause)
convertPatternGuards sep = do
  let allGuards = sep.head : (map (\(Tuple _ g) -> g) sep.tail)
  traverse convertPatternGuard allGuards

convertPatternGuard :: Cst.PatternGuard Void -> Either String Ast.GuardClause
convertPatternGuard pg = case pg.binder of
  Nothing -> do
    expr <- convertExpr pg.expr
    pure $ Ast.GuardExpr expr
  Just (Tuple binder _) -> do
    pat <- convertBinder binder
    expr <- convertExpr pg.expr
    pure $ Ast.GuardPat pat expr

-- ============================================================================
-- Type Signature Conversion
-- ============================================================================

convertTypeSignature :: { label :: Cst.Name Cst.Ident, separator :: Cst.SourceToken, value :: Cst.Type Void } -> Either String Ast.Declaration
convertTypeSignature labeled = do
  let name = unwrapIdent labeled.label.name
  Tuple constraints ty <- extractConstraints labeled.value
  tyExpr <- convertType ty
  pure $ Ast.DeclTypeSig { name, typeVars: Nil, constraints, ty: tyExpr }

extractConstraints :: Cst.Type Void -> Either String (Tuple (List Ast.Constraint) (Cst.Type Void))
extractConstraints ty = case ty of
  Cst.TypeConstrained constraint _ rest -> do
    c <- convertConstraintType constraint
    Tuple restCs restTy <- extractConstraints rest
    pure $ Tuple (c : restCs) restTy
  _ -> pure $ Tuple Nil ty

convertConstraintType :: Cst.Type Void -> Either String Ast.Constraint
convertConstraintType ty = case ty of
  Cst.TypeConstructor qn -> do
    let className = unwrapProper qn.name
    pure { className, types: Nil }
  Cst.TypeApp (Cst.TypeConstructor qn) args -> do
    let className = unwrapProper qn.name
    types <- traverse convertType args
    pure { className, types }
  _ -> Left "Invalid constraint type"

-- ============================================================================
-- Fixity Conversion
-- ============================================================================

convertFixity :: Cst.FixityFields -> Either String Ast.Declaration
convertFixity fields = do
  let assoc = case (snd fields.keyword) of
        Cst.Infix -> Ast.AssocNone
        Cst.Infixl -> Ast.AssocLeft
        Cst.Infixr -> Ast.AssocRight
  let prec = snd fields.prec
  case fields.operator of
    Cst.FixityValue qn _ opName -> do
      let functionName = case qn.name of
            Left ident -> unwrapIdent ident
            Right proper -> unwrapProper proper
      let operator = unwrapOperator opName.name
      pure $ Ast.DeclInfix { associativity: assoc, precedence: prec, functionName, operator }
    Cst.FixityType _ qn _ opName -> do
      let functionName = unwrapProper qn.name
      let operator = unwrapOperator opName.name
      pure $ Ast.DeclInfix { associativity: assoc, precedence: prec, functionName, operator }

-- ============================================================================
-- Foreign Import Conversion
-- ============================================================================

convertForeign :: Cst.Foreign Void -> Either String Ast.Declaration
convertForeign foreign' = case foreign' of
  Cst.ForeignValue labeled -> do
    let functionName = unwrapIdent labeled.label.name
    ty <- convertType labeled.value
    pure $ Ast.DeclForeignImport { moduleName: "", functionName, alias: Nothing, typeSignature: ty }
  Cst.ForeignData _ labeled -> do
    -- Foreign data types become type aliases to opaque types
    let name = unwrapProper labeled.label.name
    ty <- convertType labeled.value
    pure $ Ast.DeclTypeAlias { name, typeVars: Nil, ty }

-- ============================================================================
-- Type Conversion
-- ============================================================================

convertType :: Cst.Type Void -> Either String Ast.TypeExpr
convertType ty = case ty of
  Cst.TypeVar name ->
    pure $ Ast.TyExprVar (unwrapIdent name.name)

  Cst.TypeConstructor qn ->
    pure $ Ast.TyExprCon (unwrapProper qn.name)

  Cst.TypeWildcard _ ->
    pure $ Ast.TyExprVar "_"

  Cst.TypeHole name ->
    pure $ Ast.TyExprVar ("?" <> unwrapIdent name.name)

  Cst.TypeString _ s ->
    pure $ Ast.TyExprCon ("\"" <> s <> "\"")

  Cst.TypeInt _ _ ->
    pure $ Ast.TyExprCon "Int"

  Cst.TypeRow wrapped -> do
    row <- convertRow wrapped.value
    pure $ Ast.TyExprRecord row.fields row.tail

  Cst.TypeRecord wrapped -> do
    row <- convertRow wrapped.value
    pure $ Ast.TyExprRecord row.fields row.tail

  Cst.TypeForall _ vars _ body -> do
    let varNames = map extractTypeVar vars
    bodyTy <- convertType body
    pure $ Ast.TyExprForAll varNames bodyTy

  Cst.TypeKinded t _ _ ->
    convertType t

  Cst.TypeApp head args -> do
    headTy <- convertType head
    argTys <- traverse convertType args
    pure $ List.foldl Ast.TyExprApp headTy argTys

  Cst.TypeOp head ops -> do
    -- Convert infix type operators to applications
    headTy <- convertType head
    foldTypeOps headTy ops

  Cst.TypeArrow from _ to -> do
    fromTy <- convertType from
    toTy <- convertType to
    pure $ Ast.TyExprArrow fromTy toTy

  Cst.TypeConstrained constraint _ body -> do
    c <- convertConstraintType constraint
    bodyTy <- convertType body
    pure $ Ast.TyExprConstrained (c : Nil) bodyTy

  Cst.TypeParens wrapped -> do
    inner <- convertType wrapped.value
    pure $ Ast.TyExprParens inner

  Cst.TypeError _ ->
    Left "Cannot convert type error"

foldTypeOps :: Ast.TypeExpr -> List (Tuple (Cst.QualifiedName Cst.Operator) (Cst.Type Void)) -> Either String Ast.TypeExpr
foldTypeOps acc ops = case ops of
  Nil -> pure acc
  (Tuple op ty : rest) -> do
    let opName = unwrapOperator op.name
    tyExpr <- convertType ty
    let combined = Ast.TyExprApp (Ast.TyExprApp (Ast.TyExprCon opName) acc) tyExpr
    foldTypeOps combined rest

convertRow :: Cst.Row Void -> Either String { fields :: List (Tuple String Ast.TypeExpr), tail :: Maybe String }
convertRow row = do
  fields <- case row.labels of
    Nothing -> pure Nil
    Just sep -> convertRowLabels sep
  let tail = case row.tail of
        Nothing -> Nothing
        Just (Tuple _ ty) -> case ty of
          Cst.TypeVar name -> Just (unwrapIdent name.name)
          _ -> Nothing
  pure { fields, tail }

-- Note: Expanding Separated and Labeled type aliases for self-hosted type checker
-- Separated a = { head :: a, tail :: List (Tuple SourceToken a) }
-- Labeled a b = { label :: a, separator :: SourceToken, value :: b }
type LabeledLabelType = { label :: Cst.Name Cst.Label, separator :: Cst.SourceToken, value :: Cst.Type Void }

convertRowLabels :: { head :: LabeledLabelType, tail :: List (Tuple Cst.SourceToken LabeledLabelType) } -> Either String (List (Tuple String Ast.TypeExpr))
convertRowLabels sep = do
  let allLabels = sep.head : (map (\(Tuple _ l) -> l) sep.tail)
  traverse convertRowLabel allLabels

convertRowLabel :: LabeledLabelType -> Either String (Tuple String Ast.TypeExpr)
convertRowLabel labeled = do
  let label = unwrapLabel labeled.label.name
  ty <- convertType labeled.value
  pure $ Tuple label ty

-- ============================================================================
-- Expression Conversion
-- ============================================================================

convertExpr :: Cst.Expr Void -> Either String Ast.Expr
convertExpr expr = case expr of
  Cst.ExprHole name ->
    -- _name variables are treated as regular variables, not typed holes
    pure $ Ast.ExprVar (unwrapIdent name.name)

  Cst.ExprSection _ ->
    pure $ Ast.ExprSection "_"

  Cst.ExprIdent qn ->
    case qn.module of
      Nothing -> pure $ Ast.ExprVar (unwrapIdent qn.name)
      Just modName -> pure $ Ast.ExprQualified (unwrapModuleName modName) (unwrapIdent qn.name)

  Cst.ExprConstructor qn ->
    case qn.module of
      Nothing -> pure $ Ast.ExprVar (unwrapProper qn.name)
      Just modName -> pure $ Ast.ExprQualified (unwrapModuleName modName) (unwrapProper qn.name)

  Cst.ExprBoolean _ b ->
    pure $ Ast.ExprLit (Ast.LitBool b)

  Cst.ExprChar _ c ->
    pure $ Ast.ExprLit (Ast.LitChar c)

  Cst.ExprString _ s ->
    pure $ Ast.ExprLit (Ast.LitString s)

  Cst.ExprInt _ intVal ->
    pure $ Ast.ExprLit (Ast.LitInt (intValueToInt intVal))

  Cst.ExprNumber _ n ->
    pure $ Ast.ExprLit (Ast.LitNumber n)

  Cst.ExprArray del -> do
    items <- case del.value of
      Nothing -> pure Nil
      Just sep -> do
        let allItems = sep.head : (map (\(Tuple _ e) -> e) sep.tail)
        traverse convertExpr allItems
    pure $ Ast.ExprList items

  Cst.ExprRecord del -> do
    fields <- case del.value of
      Nothing -> pure Nil
      Just sep -> do
        let allFields = sep.head : (map (\(Tuple _ f) -> f) sep.tail)
        traverse convertRecordField allFields
    pure $ Ast.ExprRecord fields

  Cst.ExprParens wrapped -> do
    inner <- convertExpr wrapped.value
    pure $ Ast.ExprParens inner

  Cst.ExprTyped e _ ty -> do
    expr' <- convertExpr e
    tyExpr <- convertType ty
    pure $ Ast.ExprTyped expr' tyExpr

  Cst.ExprInfix head ops -> do
    -- Convert infix function application `e1 \`f\` e2`
    headE <- convertExpr head
    foldInfixOps headE ops

  Cst.ExprOp head ops -> do
    headE <- convertExpr head
    foldBinOps headE ops

  Cst.ExprOpName qn ->
    case qn.module of
      Nothing -> pure $ Ast.ExprVar (unwrapOperator qn.name)
      Just modName -> pure $ Ast.ExprQualified (unwrapModuleName modName) (unwrapOperator qn.name)

  Cst.ExprNegate _ e -> do
    inner <- convertExpr e
    pure $ Ast.ExprUnaryOp "-" inner

  Cst.ExprRecordAccessor acc -> do
    base <- convertExpr acc.expr
    let path = acc.path.head : (map (\(Tuple _ l) -> l) acc.path.tail)
    let labels = map (\l -> unwrapLabel l.name) path
    case labels of
      (firstLabel : rest) -> do
        pure $ List.foldl Ast.ExprRecordAccess (Ast.ExprRecordAccess base firstLabel) rest
      Nil -> Left "Empty record accessor path"

  Cst.ExprRecordUpdate base updates -> do
    baseE <- convertExpr base
    let allUpdates = updates.value.head : (map (\(Tuple _ u) -> u) updates.value.tail)
    updateFields <- traverse convertRecordUpdate allUpdates
    pure $ Ast.ExprRecordUpdate baseE updateFields

  Cst.ExprApp head args -> do
    headE <- convertExpr head
    argEs <- traverse convertExpr args
    pure $ List.foldl Ast.ExprApp headE argEs

  Cst.ExprLambda lam -> do
    params <- traverse convertBinder lam.binders
    body <- convertExpr lam.body
    pure $ Ast.ExprLambda params body

  Cst.ExprIf ite -> do
    condE <- convertExpr ite.cond
    thenE <- convertExpr ite.thenBranch
    elseE <- convertExpr ite.elseBranch
    pure $ Ast.ExprIf condE thenE elseE

  Cst.ExprCase caseOf -> do
    -- Handle multiple scrutinees
    let allHeads = caseOf.head.head : (map (\(Tuple _ e) -> e) caseOf.head.tail)
    -- Check for lambda-case pattern: case _ of ...
    case allHeads of
      (single : Nil) ->
        -- Check if single head is underscore for lambda-case
        if isUnderscore single
        then do
          -- Convert to lambda: \x -> case x of ...
          clauses <- traverse convertCaseBranch caseOf.branches
          let lamParam = Ast.PatVar "lamcase__"
          let lamBody = Ast.ExprCase (Ast.ExprVar "lamcase__") clauses
          pure $ Ast.ExprLambda (lamParam : Nil) lamBody
        else do
          headE <- convertExpr single
          clauses <- traverse convertCaseBranch caseOf.branches
          pure $ Ast.ExprCase headE clauses
      multiple -> do
        es <- traverse convertExpr multiple
        clauses <- traverse convertCaseBranch caseOf.branches
        pure $ Ast.ExprCase (Ast.ExprTuple es) clauses

  Cst.ExprLet letIn -> do
    bindings <- traverse convertLetBinding letIn.bindings
    body <- convertExpr letIn.body
    pure $ Ast.ExprLet bindings body

  Cst.ExprDo doBlock -> do
    stmts <- traverse convertDoStatement doBlock.statements
    pure $ Ast.ExprDo stmts

  Cst.ExprAdo adoBlock -> do
    stmts <- traverse convertDoStatement adoBlock.statements
    result <- convertExpr adoBlock.result
    -- Convert ado to do with final pure
    pure $ Ast.ExprDo (stmts <> (Ast.DoExpr result : Nil))

  Cst.ExprError _ ->
    Left "Cannot convert expression error"

foldBinOps :: Ast.Expr -> List (Tuple (Cst.QualifiedName Cst.Operator) (Cst.Expr Void)) -> Either String Ast.Expr
foldBinOps acc ops = case ops of
  Nil -> pure acc
  (Tuple op e : rest) -> do
    let opName = unwrapOperator op.name
    exprE <- convertExpr e
    let combined = Ast.ExprBinOp opName acc exprE
    foldBinOps combined rest

-- Note: Expanding Wrapped type alias manually for self-hosted type checker compatibility
-- Wrapped a = { open :: SourceToken, value :: a, close :: SourceToken }
foldInfixOps :: Ast.Expr -> List (Tuple WrappedExpr (Cst.Expr Void)) -> Either String Ast.Expr
foldInfixOps acc ops = case ops of
  Nil -> pure acc
  (Tuple wrapped e : rest) -> do
    fn <- convertExpr wrapped.value
    exprE <- convertExpr e
    let combined = Ast.ExprApp (Ast.ExprApp fn acc) exprE
    foldInfixOps combined rest

convertRecordField :: Cst.RecordLabeled (Cst.Expr Void) -> Either String (Tuple String Ast.Expr)
convertRecordField field = case field of
  Cst.RecordPun name -> do
    let label = unwrapIdent name.name
    pure $ Tuple label (Ast.ExprVar label)
  Cst.RecordField name _ value -> do
    let label = unwrapLabel name.name
    expr <- convertExpr value
    pure $ Tuple label expr

convertRecordUpdate :: Cst.RecordUpdate Void -> Either String (Tuple String Ast.Expr)
convertRecordUpdate update = case update of
  Cst.RecordUpdateLeaf name _ expr -> do
    let label = unwrapLabel name.name
    e <- convertExpr expr
    pure $ Tuple label e
  Cst.RecordUpdateBranch name updates -> do
    -- Nested record update - convert to record with nested updates
    let label = unwrapLabel name.name
    let allUpdates = updates.value.head : (map (\(Tuple _ u) -> u) updates.value.tail)
    fields <- traverse convertRecordUpdate allUpdates
    pure $ Tuple label (Ast.ExprRecord fields)

convertCaseBranch :: Tuple SeparatedBinder (Cst.Guarded Void) -> Either String Ast.CaseClause
convertCaseBranch (Tuple patterns guarded) = do
  -- Handle multiple patterns (tuple patterns)
  let allPats = patterns.head : (map (\(Tuple _ p) -> p) patterns.tail)
  pat <- case allPats of
    (single : Nil) -> convertBinder single
    multiple -> do
      ps <- traverse convertBinder multiple
      pure $ Ast.PatRecord (listMapWithIndex (\i p -> Tuple (show i) p) ps)
  case guarded of
    Cst.Unconditional _ wh -> do
      body <- convertExpr wh.expr
      pure { pattern: pat, guard: Nothing, body }
    Cst.Guarded guards -> case guards of
      (ge : _) -> do
        guardClauses <- convertPatternGuards ge.patterns
        body <- convertExpr ge.where.expr
        -- Extract the first guard expression if it's a simple guard
        let guard = case guardClauses of
              (Ast.GuardExpr e : _) -> Just e
              _ -> Nothing
        pure { pattern: pat, guard, body }
      Nil -> Left "Empty guarded expression in case"

convertLetBinding :: Cst.LetBinding Void -> Either String Ast.LetBind
convertLetBinding binding = case binding of
  Cst.LetBindingSignature labeled -> do
    -- Type signature in let - we'll attach it to the next binding
    let name = unwrapIdent labeled.label.name
    ty <- convertType labeled.value
    pure { pattern: Ast.PatVar name, value: Ast.ExprVar name, typeAnn: Just ty }

  Cst.LetBindingName vbf -> do
    let name = unwrapIdent vbf.name.name
    params <- traverse convertBinder vbf.binders
    bodyExpr <- case vbf.guarded of
      Cst.Unconditional _ wh -> convertExpr wh.expr
      Cst.Guarded guards -> case guards of
        (ge : _) -> convertExpr ge.where.expr
        Nil -> Left "Empty guarded let binding"
    -- If there are parameters, wrap in lambda
    let body = if List.null params
               then bodyExpr
               else Ast.ExprLambda params bodyExpr
    pure { pattern: Ast.PatVar name, value: body, typeAnn: Nothing }

  Cst.LetBindingPattern binder _ wh -> do
    pat <- convertBinder binder
    body <- convertExpr wh.expr
    pure { pattern: pat, value: body, typeAnn: Nothing }

  Cst.LetBindingError _ ->
    Left "Cannot convert let binding error"

convertDoStatement :: Cst.DoStatement Void -> Either String Ast.DoStatement
convertDoStatement stmt = case stmt of
  Cst.DoLet _ bindings -> do
    binds <- traverse convertLetBinding bindings
    pure $ Ast.DoLet binds

  Cst.DoDiscard e -> do
    expr <- convertExpr e
    pure $ Ast.DoExpr expr

  Cst.DoBind binder _ e -> do
    pat <- convertBinder binder
    expr <- convertExpr e
    pure $ Ast.DoBind pat expr

  Cst.DoError _ ->
    Left "Cannot convert do statement error"

-- ============================================================================
-- Binder (Pattern) Conversion
-- ============================================================================

convertBinder :: Cst.Binder Void -> Either String Ast.Pattern
convertBinder binder = case binder of
  Cst.BinderWildcard _ ->
    pure Ast.PatWildcard

  Cst.BinderVar name ->
    pure $ Ast.PatVar (unwrapIdent name.name)

  Cst.BinderNamed name _ inner -> do
    innerPat <- convertBinder inner
    pure $ Ast.PatAs (unwrapIdent name.name) innerPat

  Cst.BinderConstructor qn args -> do
    let name = unwrapProper qn.name
    argPats <- traverse convertBinder args
    pure $ Ast.PatCon name argPats

  Cst.BinderBoolean _ b ->
    pure $ Ast.PatLit (Ast.LitBool b)

  Cst.BinderChar _ c ->
    pure $ Ast.PatLit (Ast.LitChar c)

  Cst.BinderString _ s ->
    pure $ Ast.PatLit (Ast.LitString s)

  Cst.BinderInt neg _ intVal -> do
    let n = intValueToInt intVal
    let n' = case neg of
          Nothing -> n
          Just _ -> -n
    pure $ Ast.PatLit (Ast.LitInt n')

  Cst.BinderNumber neg _ num -> do
    let n' = case neg of
          Nothing -> num
          Just _ -> -num
    pure $ Ast.PatLit (Ast.LitNumber n')

  Cst.BinderArray del -> do
    items <- case del.value of
      Nothing -> pure Nil
      Just sep -> do
        let allItems = sep.head : (map (\(Tuple _ b) -> b) sep.tail)
        traverse convertBinder allItems
    pure $ Ast.PatList items

  Cst.BinderRecord del -> do
    fields <- case del.value of
      Nothing -> pure Nil
      Just sep -> do
        let allFields = sep.head : (map (\(Tuple _ f) -> f) sep.tail)
        traverse convertRecordBinder allFields
    pure $ Ast.PatRecord fields

  Cst.BinderParens wrapped -> do
    inner <- convertBinder wrapped.value
    pure $ Ast.PatParens inner

  Cst.BinderTyped inner _ _ -> do
    -- Just ignore the type annotation for now
    convertBinder inner

  Cst.BinderOp head ops -> do
    -- Convert operator patterns like (x : xs)
    headPat <- convertBinder head
    foldBinderOps headPat ops

  Cst.BinderError _ ->
    Left "Cannot convert binder error"

foldBinderOps :: Ast.Pattern -> List (Tuple (Cst.QualifiedName Cst.Operator) (Cst.Binder Void)) -> Either String Ast.Pattern
foldBinderOps acc ops = case ops of
  Nil -> pure acc
  (Tuple op b : rest) -> do
    let opName = unwrapOperator op.name
    binderPat <- convertBinder b
    let combined = if opName == ":"
                   then Ast.PatCons acc binderPat
                   else Ast.PatCon opName (acc : binderPat : Nil)
    foldBinderOps combined rest

convertRecordBinder :: Cst.RecordLabeled (Cst.Binder Void) -> Either String (Tuple String Ast.Pattern)
convertRecordBinder field = case field of
  Cst.RecordPun name -> do
    let label = unwrapIdent name.name
    pure $ Tuple label (Ast.PatVar label)
  Cst.RecordField name _ binder -> do
    let label = unwrapLabel name.name
    pat <- convertBinder binder
    pure $ Tuple label pat
