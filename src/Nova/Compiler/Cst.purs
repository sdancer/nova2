module Nova.Compiler.Cst where

import Prelude
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.Tuple (Tuple)

-- ============================================================================
-- Source Positions
-- ============================================================================

type SourcePos =
  { line :: Int
  , column :: Int
  }

type SourceRange =
  { start :: SourcePos
  , end :: SourcePos
  }

-- ============================================================================
-- Tokens
-- ============================================================================

data SourceStyle = ASCII | Unicode

data IntValue
  = SmallInt Int
  | BigInt String

derive instance eqIntValue :: Eq IntValue

data Token
  -- Grouping
  = TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  -- Arrows and operators
  | TokLeftArrow        -- <-
  | TokRightArrow       -- ->
  | TokRightFatArrow    -- =>
  | TokDoubleColon      -- ::
  | TokForall           -- forall
  | TokEquals           -- =
  | TokPipe             -- |
  | TokTick             -- `
  | TokDot              -- .
  | TokComma            -- ,
  | TokUnderscore       -- _
  | TokBackslash        -- \
  | TokAt               -- @
  -- Names
  | TokLowerName (Maybe String) String    -- module prefix, name
  | TokUpperName (Maybe String) String    -- module prefix, name
  | TokOperator (Maybe String) String     -- module prefix, operator
  | TokSymbolName (Maybe String) String   -- module prefix, symbol in parens
  | TokHole String                        -- ?hole
  -- Literals
  | TokChar String Char
  | TokString String String               -- raw, parsed
  | TokRawString String
  | TokInt String IntValue
  | TokNumber String Number
  -- Layout
  | TokLayoutStart Int                    -- column where block starts
  | TokLayoutSep Int                      -- separator at column
  | TokLayoutEnd Int                      -- block end at column
  -- Special
  | TokEof

derive instance eqToken :: Eq Token

type SourceToken =
  { range :: SourceRange
  , leadingComments :: List String
  , trailingComments :: List String
  , value :: Token
  }

-- ============================================================================
-- Name Wrappers
-- ============================================================================

newtype Ident = Ident String
newtype Proper = Proper String
newtype Label = Label String
newtype Operator = Operator String
newtype ModuleName = ModuleName String

type Name a =
  { token :: SourceToken
  , name :: a
  }

type QualifiedName a =
  { token :: SourceToken
  , module :: Maybe ModuleName
  , name :: a
  }

-- ============================================================================
-- Structural Combinators
-- ============================================================================

-- | Wrapped in delimiters: (a), {a}, [a]
type Wrapped a =
  { open :: SourceToken
  , value :: a
  , close :: SourceToken
  }

-- | Comma-separated list: a, b, c
type Separated a =
  { head :: a
  , tail :: List (Tuple SourceToken a)  -- (comma, item)
  }

-- | Labeled with separator: label :: value
type Labeled a b =
  { label :: a
  , separator :: SourceToken
  , value :: b
  }

-- | Optional delimited: (a, b, c) or empty ()
type Delimited a = Wrapped (Maybe (Separated a))

-- | Non-empty delimited
type DelimitedNonEmpty a = Wrapped (Separated a)

-- ============================================================================
-- Types
-- ============================================================================

data Type e
  = TypeVar (Name Ident)
  | TypeConstructor (QualifiedName Proper)
  | TypeWildcard SourceToken
  | TypeHole (Name Ident)
  | TypeString SourceToken String
  | TypeInt SourceToken IntValue
  | TypeRow (Wrapped (Row e))
  | TypeRecord (Wrapped (Row e))
  | TypeForall SourceToken (List (TypeVarBinding e)) SourceToken (Type e)
  | TypeKinded (Type e) SourceToken (Type e)
  | TypeApp (Type e) (List (Type e))
  | TypeOp (Type e) (List (Tuple (QualifiedName Operator) (Type e)))
  | TypeArrow (Type e) SourceToken (Type e)
  | TypeConstrained (Type e) SourceToken (Type e)
  | TypeParens (Wrapped (Type e))
  | TypeError e

data TypeVarBinding e
  = TypeVarKinded (Wrapped (Labeled (Name Ident) (Type e)))
  | TypeVarName (Name Ident)

type Row e =
  { labels :: Maybe (Separated (Labeled (Name Label) (Type e)))
  , tail :: Maybe (Tuple SourceToken (Type e))
  }

-- ============================================================================
-- Module Structure
-- ============================================================================

type Module e =
  { header :: ModuleHeader e
  , body :: ModuleBody e
  }

type ModuleHeader e =
  { keyword :: SourceToken
  , name :: Name ModuleName
  , exports :: Maybe (DelimitedNonEmpty (Export e))
  , where :: SourceToken
  , imports :: List (ImportDecl e)
  }

type ModuleBody e =
  { decls :: List (Declaration e)
  , trailingComments :: List String
  , end :: SourcePos
  }

data Export e
  = ExportValue (Name Ident)
  | ExportOp (Name Operator)
  | ExportType (Name Proper) (Maybe DataMembers)
  | ExportTypeOp SourceToken (Name Operator)
  | ExportClass SourceToken (Name Proper)
  | ExportModule SourceToken (Name ModuleName)
  | ExportError e

data DataMembers
  = DataAll SourceToken
  | DataEnumerated (Delimited (Name Proper))

-- ============================================================================
-- Declarations
-- ============================================================================

data Declaration e
  = DeclData (DataHead e) (Maybe (Tuple SourceToken (Separated (DataCtor e))))
  | DeclType (DataHead e) SourceToken (Type e)
  | DeclNewtype (DataHead e) SourceToken (Name Proper) (Type e)
  | DeclClass (ClassHead e) (Maybe (Tuple SourceToken (List (Labeled (Name Ident) (Type e)))))
  | DeclInstanceChain (Separated (Instance e))
  | DeclDerive SourceToken (Maybe SourceToken) (InstanceHead e)
  | DeclSignature (Labeled (Name Ident) (Type e))
  | DeclValue (ValueBindingFields e)
  | DeclFixity FixityFields
  | DeclForeign SourceToken SourceToken (Foreign e)
  | DeclError e

type DataHead e =
  { keyword :: SourceToken
  , name :: Name Proper
  , vars :: List (TypeVarBinding e)
  }

type DataCtor e =
  { name :: Name Proper
  , fields :: List (Type e)
  }

type ClassHead e =
  { keyword :: SourceToken
  , super :: Maybe (Tuple (List (Type e)) SourceToken)
  , name :: Name Proper
  , vars :: List (TypeVarBinding e)
  , fundeps :: Maybe (Tuple SourceToken (Separated ClassFundep))
  }

data ClassFundep
  = FundepDetermined SourceToken (List (Name Ident))
  | FundepDetermines (List (Name Ident)) SourceToken (List (Name Ident))

type Instance e =
  { head :: InstanceHead e
  , body :: Maybe (Tuple SourceToken (List (InstanceBinding e)))
  }

data InstanceBinding e
  = InstanceBindingSignature (Labeled (Name Ident) (Type e))
  | InstanceBindingName (ValueBindingFields e)

type InstanceHead e =
  { keyword :: SourceToken
  , name :: Maybe (Tuple (Name Ident) SourceToken)
  , constraints :: Maybe (Tuple (List (Type e)) SourceToken)
  , className :: QualifiedName Proper
  , types :: List (Type e)
  }

type ImportDecl e =
  { keyword :: SourceToken
  , module :: Name ModuleName
  , names :: Maybe (Tuple (Maybe SourceToken) (DelimitedNonEmpty (Import e)))
  , qualified :: Maybe (Tuple SourceToken (Name ModuleName))
  }

data Import e
  = ImportValue (Name Ident)
  | ImportOp (Name Operator)
  | ImportType (Name Proper) (Maybe DataMembers)
  | ImportTypeOp SourceToken (Name Operator)
  | ImportClass SourceToken (Name Proper)
  | ImportError e

data Fixity = Infix | Infixl | Infixr

type FixityFields =
  { keyword :: Tuple SourceToken Fixity
  , prec :: Tuple SourceToken Int
  , operator :: FixityOp
  }

data FixityOp
  = FixityValue (QualifiedName (Either Ident Proper)) SourceToken (Name Operator)
  | FixityType SourceToken (QualifiedName Proper) SourceToken (Name Operator)

data Foreign e
  = ForeignValue (Labeled (Name Ident) (Type e))
  | ForeignData SourceToken (Labeled (Name Proper) (Type e))

-- ============================================================================
-- Expressions
-- ============================================================================

data Expr e
  = ExprHole (Name Ident)
  | ExprSection SourceToken
  | ExprIdent (QualifiedName Ident)
  | ExprConstructor (QualifiedName Proper)
  | ExprBoolean SourceToken Boolean
  | ExprChar SourceToken Char
  | ExprString SourceToken String
  | ExprInt SourceToken IntValue
  | ExprNumber SourceToken Number
  | ExprArray (Delimited (Expr e))
  | ExprRecord (Delimited (RecordLabeled (Expr e)))
  | ExprParens (Wrapped (Expr e))
  | ExprTyped (Expr e) SourceToken (Type e)
  | ExprInfix (Expr e) (List (Tuple (Wrapped (Expr e)) (Expr e)))
  | ExprOp (Expr e) (List (Tuple (QualifiedName Operator) (Expr e)))
  | ExprOpName (QualifiedName Operator)
  | ExprNegate SourceToken (Expr e)
  | ExprRecordAccessor (RecordAccessor e)
  | ExprRecordUpdate (Expr e) (DelimitedNonEmpty (RecordUpdate e))
  | ExprApp (Expr e) (List (Expr e))
  | ExprLambda (Lambda e)
  | ExprIf (IfThenElse e)
  | ExprCase (CaseOf e)
  | ExprLet (LetIn e)
  | ExprDo (DoBlock e)
  | ExprAdo (AdoBlock e)
  | ExprError e

data RecordLabeled a
  = RecordPun (Name Ident)
  | RecordField (Name Label) SourceToken a

data RecordUpdate e
  = RecordUpdateLeaf (Name Label) SourceToken (Expr e)
  | RecordUpdateBranch (Name Label) (DelimitedNonEmpty (RecordUpdate e))

type RecordAccessor e =
  { expr :: Expr e
  , dot :: SourceToken
  , path :: Separated (Name Label)
  }

type Lambda e =
  { symbol :: SourceToken
  , binders :: List (Binder e)
  , arrow :: SourceToken
  , body :: Expr e
  }

type IfThenElse e =
  { keyword :: SourceToken
  , cond :: Expr e
  , then :: SourceToken
  , true :: Expr e
  , else :: SourceToken
  , false :: Expr e
  }

type CaseOf e =
  { keyword :: SourceToken
  , head :: Separated (Expr e)
  , of :: SourceToken
  , branches :: List (Tuple (Separated (Binder e)) (Guarded e))
  }

type LetIn e =
  { keyword :: SourceToken
  , bindings :: List (LetBinding e)
  , in :: SourceToken
  , body :: Expr e
  }

type Where e =
  { expr :: Expr e
  , bindings :: Maybe (Tuple SourceToken (List (LetBinding e)))
  }

data LetBinding e
  = LetBindingSignature (Labeled (Name Ident) (Type e))
  | LetBindingName (ValueBindingFields e)
  | LetBindingPattern (Binder e) SourceToken (Where e)
  | LetBindingError e

type DoBlock e =
  { keyword :: SourceToken
  , statements :: List (DoStatement e)
  }

data DoStatement e
  = DoLet SourceToken (List (LetBinding e))
  | DoDiscard (Expr e)
  | DoBind (Binder e) SourceToken (Expr e)
  | DoError e

type AdoBlock e =
  { keyword :: SourceToken
  , statements :: List (DoStatement e)
  , in :: SourceToken
  , result :: Expr e
  }

-- ============================================================================
-- Value Bindings & Guards
-- ============================================================================

type ValueBindingFields e =
  { name :: Name Ident
  , binders :: List (Binder e)
  , guarded :: Guarded e
  }

data Guarded e
  = Unconditional SourceToken (Where e)
  | Guarded (List (GuardedExpr e))

type GuardedExpr e =
  { bar :: SourceToken
  , patterns :: Separated (PatternGuard e)
  , separator :: SourceToken
  , where :: Where e
  }

type PatternGuard e =
  { binder :: Maybe (Tuple (Binder e) SourceToken)
  , expr :: Expr e
  }

-- ============================================================================
-- Binders (Patterns)
-- ============================================================================

data Binder e
  = BinderWildcard SourceToken
  | BinderVar (Name Ident)
  | BinderNamed (Name Ident) SourceToken (Binder e)
  | BinderConstructor (QualifiedName Proper) (List (Binder e))
  | BinderBoolean SourceToken Boolean
  | BinderChar SourceToken Char
  | BinderString SourceToken String
  | BinderInt (Maybe SourceToken) SourceToken IntValue
  | BinderNumber (Maybe SourceToken) SourceToken Number
  | BinderArray (Delimited (Binder e))
  | BinderRecord (Delimited (RecordLabeled (Binder e)))
  | BinderParens (Wrapped (Binder e))
  | BinderTyped (Binder e) SourceToken (Type e)
  | BinderOp (Binder e) (List (Tuple (QualifiedName Operator) (Binder e)))
  | BinderError e

-- ============================================================================
-- Error Recovery
-- ============================================================================

-- | A recovered error contains the tokens that couldn't be parsed
type RecoveredError =
  { tokens :: List SourceToken
  , error :: String
  }

-- | Parse result that supports partial success
data ParseResult e a
  = ParseSucceeded a
  | ParseSucceededWithErrors a (List RecoveredError)
  | ParseFailed String
