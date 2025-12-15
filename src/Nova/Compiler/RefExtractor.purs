module Nova.Compiler.RefExtractor where

import Prelude
import Data.List (List)
import Data.Foldable (foldl)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Nova.Compiler.Ast (Declaration(..), Expr(..), Pattern(..), TypeExpr(..), LetBind, CaseClause, DoStatement(..), GuardedExpr, GuardClause(..), FunctionDeclaration, DataType, TypeAlias, TypeClass, TypeClassInstance, ForeignImport, TypeSignature, NewtypeDecl)

-- | Reference type between declarations
data RefType = CallRef | TypeRef | ImportRef | PatternRef | ConstructorRef

instance eqRefType :: Eq RefType where
  eq CallRef CallRef = true
  eq TypeRef TypeRef = true
  eq ImportRef ImportRef = true
  eq PatternRef PatternRef = true
  eq ConstructorRef ConstructorRef = true
  eq _ _ = false

-- | A raw reference extracted from the AST (before resolution)
newtype RawRef = RawRef { name :: String, refType :: RefType }

-- | Create a raw ref
mkRef :: String -> RefType -> RawRef
mkRef name refType = RawRef { name, refType }

-- | Get ref name
refName :: RawRef -> String
refName (RawRef r) = r.name

-- | Get ref type
refRefType :: RawRef -> RefType
refRefType (RawRef r) = r.refType

-- | Extract all references from a declaration
extractRefs :: Declaration -> Array RawRef
extractRefs decl = Set.toUnfoldable (extractRefsSet decl)

-- | Extract references as a Set (to dedupe)
extractRefsSet :: Declaration -> Set RawRef
extractRefsSet decl = case decl of
  DeclFunction f -> extractFunctionRefs f
  DeclDataType d -> extractDataTypeRefs d
  DeclTypeAlias a -> extractTypeAliasRefs a
  DeclTypeClass c -> extractTypeClassRefs c
  DeclTypeClassInstance i -> extractInstanceRefs i
  DeclForeignImport f -> extractForeignRefs f
  DeclTypeSig s -> extractTypeSigRefs s
  DeclNewtype n -> extractNewtypeRefs n
  DeclType t -> extractTypeExprRefs t.typeSignature
  DeclModule m -> foldl (\acc d -> Set.union acc (extractRefsSet d)) Set.empty m.declarations
  DeclImport i -> Set.singleton (mkRef i.moduleName ImportRef)
  DeclInfix _ -> Set.empty

-- | Extract refs from function declaration
extractFunctionRefs :: FunctionDeclaration -> Set RawRef
extractFunctionRefs f =
  let bodyRefs = extractExprRefs f.body
      guardRefs = foldl (\acc g -> Set.union acc (extractGuardedRefs g)) Set.empty f.guards
      paramRefs = foldl (\acc p -> Set.union acc (extractPatternRefs p)) Set.empty f.parameters
      sigRefs = case f.typeSignature of
        Nothing -> Set.empty
        Just sig -> extractTypeSigRefs sig
  in Set.union bodyRefs (Set.union guardRefs (Set.union paramRefs sigRefs))

-- | Extract refs from data type
extractDataTypeRefs :: DataType -> Set RawRef
extractDataTypeRefs d =
  foldl (\acc c -> Set.union acc (extractConstructorRefs c)) Set.empty d.constructors

-- | Extract refs from a data constructor
extractConstructorRefs :: { name :: String, fields :: List { label :: String, ty :: TypeExpr }, isRecord :: Boolean } -> Set RawRef
extractConstructorRefs c =
  foldl (\acc f -> Set.union acc (extractTypeExprRefs f.ty)) Set.empty c.fields

-- | Extract refs from type alias
extractTypeAliasRefs :: TypeAlias -> Set RawRef
extractTypeAliasRefs a = extractTypeExprRefs a.ty

-- | Extract refs from type class
extractTypeClassRefs :: TypeClass -> Set RawRef
extractTypeClassRefs c =
  foldl (\acc m -> Set.union acc (extractTypeSigRefs m)) Set.empty c.methods

-- | Extract refs from type class instance
extractInstanceRefs :: TypeClassInstance -> Set RawRef
extractInstanceRefs i =
  let classRef = Set.singleton (mkRef i.className TypeRef)
      tyRefs = extractTypeExprRefs i.ty
      methodRefs = foldl (\acc m -> Set.union acc (extractFunctionRefs m)) Set.empty i.methods
  in Set.union classRef (Set.union tyRefs methodRefs)

-- | Extract refs from foreign import
extractForeignRefs :: ForeignImport -> Set RawRef
extractForeignRefs f = extractTypeExprRefs f.typeSignature

-- | Extract refs from type signature
extractTypeSigRefs :: TypeSignature -> Set RawRef
extractTypeSigRefs s =
  let constraintRefs = foldl (\acc c ->
        let classRef = Set.singleton (mkRef c.className TypeRef)
            typeRefs = foldl (\acc2 t -> Set.union acc2 (extractTypeExprRefs t)) Set.empty c.types
        in Set.union acc (Set.union classRef typeRefs)
      ) Set.empty s.constraints
  in Set.union constraintRefs (extractTypeExprRefs s.ty)

-- | Extract refs from newtype
extractNewtypeRefs :: NewtypeDecl -> Set RawRef
extractNewtypeRefs n = extractTypeExprRefs n.wrappedType

-- | Extract refs from type expression
extractTypeExprRefs :: TypeExpr -> Set RawRef
extractTypeExprRefs ty = case ty of
  TyExprCon name -> Set.singleton (mkRef name TypeRef)
  TyExprVar _ -> Set.empty
  TyExprApp t1 t2 -> Set.union (extractTypeExprRefs t1) (extractTypeExprRefs t2)
  TyExprArrow t1 t2 -> Set.union (extractTypeExprRefs t1) (extractTypeExprRefs t2)
  TyExprRecord fields _ ->
    foldl (\acc (Tuple _ t) -> Set.union acc (extractTypeExprRefs t)) Set.empty fields
  TyExprForAll _ t -> extractTypeExprRefs t
  TyExprConstrained cs t ->
    let constraintRefs = foldl (\acc c ->
          let classRef = Set.singleton (mkRef c.className TypeRef)
              typeRefs = foldl (\acc2 ct -> Set.union acc2 (extractTypeExprRefs ct)) Set.empty c.types
          in Set.union acc (Set.union classRef typeRefs)
        ) Set.empty cs
    in Set.union constraintRefs (extractTypeExprRefs t)
  TyExprParens t -> extractTypeExprRefs t
  TyExprTuple ts -> foldl (\acc t -> Set.union acc (extractTypeExprRefs t)) Set.empty ts

-- | Extract refs from expression
extractExprRefs :: Expr -> Set RawRef
extractExprRefs expr = case expr of
  ExprVar name -> Set.singleton (mkRef name CallRef)
  ExprQualified ns name -> Set.singleton (mkRef (ns <> "." <> name) CallRef)
  ExprLit _ -> Set.empty
  ExprApp e1 e2 -> Set.union (extractExprRefs e1) (extractExprRefs e2)
  ExprLambda pats body ->
    let patRefs = foldl (\acc p -> Set.union acc (extractPatternRefs p)) Set.empty pats
    in Set.union patRefs (extractExprRefs body)
  ExprLet binds body ->
    let bindRefs = extractLetBindsRefs binds
    in Set.union bindRefs (extractExprRefs body)
  ExprIf c t e -> Set.union (extractExprRefs c) (Set.union (extractExprRefs t) (extractExprRefs e))
  ExprCase e clauses ->
    Set.union (extractExprRefs e) (foldl (\acc c -> Set.union acc (extractCaseClauseRefs c)) Set.empty clauses)
  ExprDo stmts -> extractDoStatementsRefs stmts
  ExprBinOp op e1 e2 -> Set.union (Set.singleton (mkRef op CallRef)) (Set.union (extractExprRefs e1) (extractExprRefs e2))
  ExprUnaryOp op e -> Set.union (Set.singleton (mkRef op CallRef)) (extractExprRefs e)
  ExprList es -> foldl (\acc e -> Set.union acc (extractExprRefs e)) Set.empty es
  ExprTuple es -> foldl (\acc e -> Set.union acc (extractExprRefs e)) Set.empty es
  ExprRecord fields -> foldl (\acc (Tuple _ e) -> Set.union acc (extractExprRefs e)) Set.empty fields
  ExprRecordAccess e _ -> extractExprRefs e
  ExprRecordUpdate e fields -> Set.union (extractExprRefs e) (foldl (\acc (Tuple _ v) -> Set.union acc (extractExprRefs v)) Set.empty fields)
  ExprTyped e t -> Set.union (extractExprRefs e) (extractTypeExprRefs t)
  ExprParens e -> extractExprRefs e
  ExprSection _ -> Set.empty
  ExprSectionLeft e _ -> extractExprRefs e
  ExprSectionRight _ e -> extractExprRefs e

-- | Extract refs from let bindings
extractLetBindsRefs :: List LetBind -> Set RawRef
extractLetBindsRefs binds = foldl (\acc b ->
  let patRefs = extractPatternRefs b.pattern
      valRefs = extractExprRefs b.value
      annRefs = case b.typeAnn of
        Nothing -> Set.empty
        Just t -> extractTypeExprRefs t
  in Set.union acc (Set.union patRefs (Set.union valRefs annRefs))
) Set.empty binds

-- | Extract refs from case clause
extractCaseClauseRefs :: CaseClause -> Set RawRef
extractCaseClauseRefs clause =
  let patRefs = extractPatternRefs clause.pattern
      bodyRefs = extractExprRefs clause.body
      guardRefs = case clause.guard of
        Nothing -> Set.empty
        Just g -> extractExprRefs g
  in Set.union patRefs (Set.union bodyRefs guardRefs)

-- | Extract refs from do statements
extractDoStatementsRefs :: List DoStatement -> Set RawRef
extractDoStatementsRefs stmts = foldl (\acc stmt -> case stmt of
  DoLet binds -> Set.union acc (extractLetBindsRefs binds)
  DoBind pat e -> Set.union acc (Set.union (extractPatternRefs pat) (extractExprRefs e))
  DoExpr e -> Set.union acc (extractExprRefs e)
) Set.empty stmts

-- | Extract refs from guarded expression
extractGuardedRefs :: GuardedExpr -> Set RawRef
extractGuardedRefs g =
  let guardRefs = foldl (\acc gc -> Set.union acc (extractGuardClauseRefs gc)) Set.empty g.guards
      bodyRefs = extractExprRefs g.body
  in Set.union guardRefs bodyRefs

-- | Extract refs from guard clause
extractGuardClauseRefs :: GuardClause -> Set RawRef
extractGuardClauseRefs gc = case gc of
  GuardExpr e -> extractExprRefs e
  GuardPat pat e -> Set.union (extractPatternRefs pat) (extractExprRefs e)

-- | Extract refs from pattern (constructors become PatternRef)
extractPatternRefs :: Pattern -> Set RawRef
extractPatternRefs pat = case pat of
  PatVar _ -> Set.empty
  PatWildcard -> Set.empty
  PatLit _ -> Set.empty
  PatCon name pats ->
    let conRef = Set.singleton (mkRef name PatternRef)
        subRefs = foldl (\acc p -> Set.union acc (extractPatternRefs p)) Set.empty pats
    in Set.union conRef subRefs
  PatRecord fields -> foldl (\acc (Tuple _ p) -> Set.union acc (extractPatternRefs p)) Set.empty fields
  PatList pats -> foldl (\acc p -> Set.union acc (extractPatternRefs p)) Set.empty pats
  PatCons p1 p2 -> Set.union (extractPatternRefs p1) (extractPatternRefs p2)
  PatAs _ p -> extractPatternRefs p
  PatParens p -> extractPatternRefs p

-- | Eq instance for RawRef so we can use Set
instance eqRawRef :: Eq RawRef where
  eq (RawRef r1) (RawRef r2) = r1.name == r2.name && r1.refType == r2.refType

-- | Ord instance for RawRef so we can use Set
instance ordRawRef :: Ord RawRef where
  compare (RawRef r1) (RawRef r2) =
    if r1.name < r2.name then LT
    else if r1.name > r2.name then GT
    else compare (refTypeToInt r1.refType) (refTypeToInt r2.refType)

-- | Convert RefType to Int for ordering
refTypeToInt :: RefType -> Int
refTypeToInt CallRef = 0
refTypeToInt TypeRef = 1
refTypeToInt ImportRef = 2
refTypeToInt PatternRef = 3
refTypeToInt ConstructorRef = 4
