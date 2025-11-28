module Nova.Compiler.Ast where

import Prelude
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Set (Set)

-- ============================================================================
-- Namespace Service Types (Phase 1 Foundation)
-- ============================================================================

-- | Unique identifier for a declaration
-- | Format: "namespace:kind:name:version"
type DeclId = String

-- | Status of a declaration in the namespace service
data DeclStatus
  = Fresh        -- Not yet type-checked
  | Valid        -- Type-checked successfully
  | Invalid      -- Has type errors
  | Stale        -- Needs re-check (dependency changed)

-- | Metadata for a managed declaration
type DeclMetadata =
  { declId :: DeclId
  , namespace :: String
  , name :: String
  , kind :: DeclKind
  , version :: Int
  , status :: DeclStatus
  , dependencies :: Set DeclId    -- What this declaration depends on
  , dependents :: Set DeclId      -- What depends on this declaration
  }

-- | Kind of declaration (for DeclId generation)
data DeclKind
  = KindFunction
  | KindDataType
  | KindTypeAlias
  | KindTypeClass
  | KindInstance
  | KindForeignImport

-- | A declaration managed by the namespace service
type ManagedDecl =
  { meta :: DeclMetadata
  , decl :: Declaration
  , sourceText :: String          -- Original source for re-parsing
  , errors :: Array String        -- Cached type errors
  }

-- | Generate a DeclId from components
makeDeclId :: String -> DeclKind -> String -> Int -> DeclId
makeDeclId namespace kind name version =
  namespace <> ":" <> kindToString kind <> ":" <> name <> ":" <> show version

-- | Convert DeclKind to string for DeclId
kindToString :: DeclKind -> String
kindToString KindFunction = "function"
kindToString KindDataType = "datatype"
kindToString KindTypeAlias = "typealias"
kindToString KindTypeClass = "typeclass"
kindToString KindInstance = "instance"
kindToString KindForeignImport = "foreign"

-- | Get the kind of a declaration
getDeclKind :: Declaration -> DeclKind
getDeclKind (DeclFunction _) = KindFunction
getDeclKind (DeclDataType _) = KindDataType
getDeclKind (DeclNewtype _) = KindDataType  -- Newtypes are like data types
getDeclKind (DeclTypeAlias _) = KindTypeAlias
getDeclKind (DeclTypeClass _) = KindTypeClass
getDeclKind (DeclTypeClassInstance _) = KindInstance
getDeclKind (DeclForeignImport _) = KindForeignImport
getDeclKind (DeclTypeSig _) = KindFunction  -- Type sigs are associated with functions
getDeclKind (DeclType _) = KindTypeAlias
getDeclKind (DeclModule _) = KindFunction  -- Shouldn't happen
getDeclKind (DeclImport _) = KindFunction  -- Imports are special
getDeclKind (DeclInfix _) = KindFunction  -- Infix declarations are metadata

-- | Get the name of a declaration
getDeclName :: Declaration -> String
getDeclName (DeclFunction f) = f.name
getDeclName (DeclDataType d) = d.name
getDeclName (DeclNewtype n) = n.name
getDeclName (DeclTypeAlias a) = a.name
getDeclName (DeclTypeClass c) = c.name
getDeclName (DeclTypeClassInstance i) = i.className <> "_" <> typeExprToString i.ty
getDeclName (DeclForeignImport f) = f.functionName
getDeclName (DeclTypeSig s) = s.name
getDeclName (DeclType t) = t.name
getDeclName (DeclModule m) = m.name
getDeclName (DeclImport i) = i.moduleName
getDeclName (DeclInfix inf) = inf.operator

-- | Simple type expression to string (for instance naming)
typeExprToString :: TypeExpr -> String
typeExprToString (TyExprCon s) = s
typeExprToString (TyExprVar s) = s
typeExprToString (TyExprApp t1 t2) = typeExprToString t1 <> "_" <> typeExprToString t2
typeExprToString _ = "complex"

-- ============================================================================
-- Original AST Types
-- ============================================================================

-- | Module definition
type Module =
  { name :: String
  , declarations :: Array Declaration
  }

-- | All possible declarations
data Declaration
  = DeclModule Module
  | DeclFunction FunctionDeclaration
  | DeclType TypeDeclaration
  | DeclTypeAlias TypeAlias
  | DeclDataType DataType
  | DeclNewtype NewtypeDecl
  | DeclTypeClass TypeClass
  | DeclTypeClassInstance TypeClassInstance
  | DeclImport ImportDeclaration
  | DeclForeignImport ForeignImport
  | DeclTypeSig TypeSignature
  | DeclInfix InfixDecl

-- | Function declaration
type FunctionDeclaration =
  { name :: String
  , parameters :: Array Pattern
  , body :: Expr
  , guards :: Array GuardedExpr
  , typeSignature :: Maybe TypeSignature
  }

-- | A guarded expression (for pattern guards)
-- | e.g., `| cond = expr` or `| Pat <- expr, cond = expr`
type GuardedExpr =
  { guards :: Array GuardClause
  , body :: Expr
  }

-- | A single guard clause within a guarded expression
data GuardClause
  = GuardExpr Expr                -- ^ Simple boolean guard: `| cond`
  | GuardPat Pattern Expr         -- ^ Pattern guard: `| Pat <- expr`

-- | Type declaration
type TypeDeclaration =
  { name :: String
  , typeSignature :: TypeExpr
  }

-- | Type signature (standalone)
type TypeSignature =
  { name :: String
  , typeVars :: Array String
  , constraints :: Array Constraint
  , ty :: TypeExpr
  }

-- | Type class definition
type TypeClass =
  { name :: String
  , typeVars :: Array String
  , methods :: Array TypeSignature
  , kind :: Maybe String
  }

-- | Type alias
type TypeAlias =
  { name :: String
  , typeVars :: Array String
  , ty :: TypeExpr
  }

-- | Type class instance
type TypeClassInstance =
  { className :: String
  , ty :: TypeExpr
  , methods :: Array FunctionDeclaration
  , derived :: Boolean
  }

-- | Algebraic data type
type DataType =
  { name :: String
  , typeVars :: Array String
  , constructors :: Array DataConstructor
  }

-- | Data constructor
type DataConstructor =
  { name :: String
  , fields :: Array DataField
  , isRecord :: Boolean
  }

-- | Data field (for record constructors)
type DataField =
  { label :: String
  , ty :: TypeExpr
  }

-- | Import declaration
type ImportDeclaration =
  { moduleName :: String
  , alias :: Maybe String
  , items :: Array ImportItem
  , hiding :: Boolean
  }

-- | Import item
data ImportItem
  = ImportValue String
  | ImportType String ImportSpec

data ImportSpec
  = ImportAll
  | ImportSome (Array String)
  | ImportNone

-- | Foreign import
type ForeignImport =
  { moduleName :: String
  , functionName :: String
  , alias :: Maybe String
  , typeSignature :: TypeExpr
  }

-- | Infix declaration (infixl, infixr, infix)
type InfixDecl =
  { associativity :: Associativity
  , precedence :: Int
  , functionName :: String
  , operator :: String
  }

-- | Associativity for infix operators
data Associativity
  = AssocLeft    -- infixl
  | AssocRight   -- infixr
  | AssocNone    -- infix

-- | Newtype declaration (like data but with exactly one constructor and one field)
type NewtypeDecl =
  { name :: String
  , typeVars :: Array String
  , constructor :: String
  , wrappedType :: TypeExpr
  }

-- | Constraint in type signature
type Constraint =
  { className :: String
  , types :: Array TypeExpr
  }

-- | Type expressions (in source, before type checking)
data TypeExpr
  = TyExprCon String
  | TyExprVar String
  | TyExprApp TypeExpr TypeExpr
  | TyExprArrow TypeExpr TypeExpr
  | TyExprRecord (Array (Tuple String TypeExpr)) (Maybe String)
  | TyExprForAll (Array String) TypeExpr
  | TyExprConstrained (Array Constraint) TypeExpr
  | TyExprParens TypeExpr
  | TyExprTuple (Array TypeExpr)

-- | Patterns for matching
data Pattern
  = PatVar String
  | PatWildcard
  | PatLit Literal
  | PatCon String (Array Pattern)
  | PatRecord (Array (Tuple String Pattern))
  | PatList (Array Pattern)
  | PatCons Pattern Pattern
  | PatAs String Pattern
  | PatParens Pattern

-- | Expressions
data Expr
  = ExprVar String
  | ExprQualified String String  -- namespace.name
  | ExprLit Literal
  | ExprApp Expr Expr
  | ExprLambda (Array Pattern) Expr
  | ExprLet (Array LetBind) Expr
  | ExprIf Expr Expr Expr
  | ExprCase Expr (Array CaseClause)
  | ExprDo (Array DoStatement)
  | ExprBinOp String Expr Expr
  | ExprUnaryOp String Expr
  | ExprList (Array Expr)
  | ExprTuple (Array Expr)
  | ExprRecord (Array (Tuple String Expr))
  | ExprRecordAccess Expr String
  | ExprRecordUpdate Expr (Array (Tuple String Expr))
  | ExprTyped Expr TypeExpr
  | ExprParens Expr
  | ExprSection String              -- record accessor section like (.field) or bare operator like (+)
  | ExprSectionLeft Expr String     -- left section like (1 +), apply left operand first
  | ExprSectionRight String Expr    -- right section like (+ 1), apply right operand first

-- | Literal values
data Literal
  = LitInt Int
  | LitNumber Number
  | LitString String
  | LitChar Char
  | LitBool Boolean

-- | Let binding
type LetBind =
  { pattern :: Pattern
  , value :: Expr
  , typeAnn :: Maybe TypeExpr
  }

-- | Case clause
type CaseClause =
  { pattern :: Pattern
  , guard :: Maybe Expr
  , body :: Expr
  }

-- | Do statement
data DoStatement
  = DoLet (Array LetBind)
  | DoBind Pattern Expr
  | DoExpr Expr
