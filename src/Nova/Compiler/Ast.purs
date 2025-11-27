module Nova.Compiler.Ast where

import Prelude
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

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
  | DeclTypeClass TypeClass
  | DeclTypeClassInstance TypeClassInstance
  | DeclImport ImportDeclaration
  | DeclForeignImport ForeignImport
  | DeclTypeSig TypeSignature

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
  | ExprSection String  -- operator section like (+ 1)

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
