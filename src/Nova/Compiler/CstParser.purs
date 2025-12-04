module Nova.Compiler.CstParser where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Control.Lazy as Control.Lazy
import Nova.Compiler.Cst as Cst

-- ============================================================================
-- Parser Type
-- ============================================================================

type TokenStream = Array Cst.SourceToken

type ParseResult a = Either String (Tuple a TokenStream)

-- | Parser monad
newtype Parser a = Parser (TokenStream -> ParseResult a)

runParser :: forall a. Parser a -> TokenStream -> ParseResult a
runParser (Parser p) = p

-- ============================================================================
-- Parser Combinators
-- ============================================================================

instance functorParser :: Functor Parser where
  map f (Parser p) = Parser \ts -> case p ts of
    Left err -> Left err
    Right (Tuple a rest) -> Right (Tuple (f a) rest)

instance applyParser :: Apply Parser where
  apply (Parser pf) (Parser pa) = Parser \ts -> case pf ts of
    Left err -> Left err
    Right (Tuple f rest) -> case pa rest of
      Left err -> Left err
      Right (Tuple a rest') -> Right (Tuple (f a) rest')

instance applicativeParser :: Applicative Parser where
  pure a = Parser \ts -> Right (Tuple a ts)

instance bindParser :: Bind Parser where
  bind (Parser pa) f = Parser \ts -> case pa ts of
    Left err -> Left err
    Right (Tuple a rest) -> runParser (f a) rest

instance monadParser :: Monad Parser

-- | Lazy instance for recursion
instance lazyParser :: Control.Lazy.Lazy (Parser a) where
  defer f = Parser \ts -> runParser (f unit) ts

-- | Alternative - try first, if fails try second
infixl 3 alt as <|>

alt :: forall a. Parser a -> Parser a -> Parser a
alt (Parser p1) (Parser p2) = Parser \ts -> case p1 ts of
  Right result -> Right result
  Left _ -> p2 ts

-- | Fail with message
fail :: forall a. String -> Parser a
fail msg = Parser \_ -> Left msg

-- | Get current position for error messages
getPosition :: Parser Cst.SourcePos
getPosition = Parser \ts -> case Array.head ts of
  Just tok -> Right (Tuple tok.range.start ts)
  Nothing -> Right (Tuple { line: 0, column: 0 } ts)

-- ============================================================================
-- Token Matchers
-- ============================================================================

-- | Match any token satisfying a predicate
satisfy :: (Cst.Token -> Boolean) -> Parser Cst.SourceToken
satisfy pred = Parser \ts -> case Array.uncons ts of
  Just { head: tok, tail: rest } ->
    if pred tok.value
    then Right (Tuple tok rest)
    else Left "Token did not match predicate"
  Nothing -> Left "Unexpected end of input"

-- | Match exact token
token :: Cst.Token -> Parser Cst.SourceToken
token expected = satisfy (_ == expected)

-- | Match and extract from token
expectMap :: forall a. (Cst.Token -> Maybe a) -> Parser (Tuple Cst.SourceToken a)
expectMap f = Parser \ts -> case Array.uncons ts of
  Just { head: tok, tail: rest } -> case f tok.value of
    Just a -> Right (Tuple (Tuple tok a) rest)
    Nothing -> Left "Token did not match"
  Nothing -> Left "Unexpected end of input"

-- | Specific token matchers
tokLeftParen :: Parser Cst.SourceToken
tokLeftParen = token Cst.TokLeftParen

tokRightParen :: Parser Cst.SourceToken
tokRightParen = token Cst.TokRightParen

tokLeftBrace :: Parser Cst.SourceToken
tokLeftBrace = token Cst.TokLeftBrace

tokRightBrace :: Parser Cst.SourceToken
tokRightBrace = token Cst.TokRightBrace

tokLeftSquare :: Parser Cst.SourceToken
tokLeftSquare = token Cst.TokLeftSquare

tokRightSquare :: Parser Cst.SourceToken
tokRightSquare = token Cst.TokRightSquare

tokEquals :: Parser Cst.SourceToken
tokEquals = token Cst.TokEquals

tokPipe :: Parser Cst.SourceToken
tokPipe = token Cst.TokPipe

tokDot :: Parser Cst.SourceToken
tokDot = token Cst.TokDot

tokComma :: Parser Cst.SourceToken
tokComma = token Cst.TokComma

tokBackslash :: Parser Cst.SourceToken
tokBackslash = token Cst.TokBackslash

tokRightArrow :: Parser Cst.SourceToken
tokRightArrow = token Cst.TokRightArrow

tokLeftArrow :: Parser Cst.SourceToken
tokLeftArrow = token Cst.TokLeftArrow

tokDoubleColon :: Parser Cst.SourceToken
tokDoubleColon = token Cst.TokDoubleColon

tokRightFatArrow :: Parser Cst.SourceToken
tokRightFatArrow = token Cst.TokRightFatArrow

tokForall :: Parser Cst.SourceToken
tokForall = token Cst.TokForall

-- | Layout tokens
tokLayoutStart :: Parser Cst.SourceToken
tokLayoutStart = satisfy isLayoutStart
  where
    isLayoutStart (Cst.TokLayoutStart _) = true
    isLayoutStart _ = false

tokLayoutSep :: Parser Cst.SourceToken
tokLayoutSep = satisfy isLayoutSep
  where
    isLayoutSep (Cst.TokLayoutSep _) = true
    isLayoutSep _ = false

tokLayoutEnd :: Parser Cst.SourceToken
tokLayoutEnd = satisfy isLayoutEnd
  where
    isLayoutEnd (Cst.TokLayoutEnd _) = true
    isLayoutEnd _ = false

-- | Match lowercase name (identifier)
tokLowerName :: Parser (Cst.Name Cst.Ident)
tokLowerName = do
  Tuple tok name <- expectMap extractLower
  pure { token: tok, name: Cst.Ident name }
  where
    extractLower (Cst.TokLowerName Nothing name) = Just name
    extractLower _ = Nothing

-- | Match qualified lowercase name
tokQualifiedLowerName :: Parser (Cst.QualifiedName Cst.Ident)
tokQualifiedLowerName = do
  Tuple tok (Tuple mod name) <- expectMap extractQualLower
  pure { token: tok, module: mod, name: Cst.Ident name }
  where
    extractQualLower (Cst.TokLowerName mod name) = Just (Tuple (map Cst.ModuleName mod) name)
    extractQualLower _ = Nothing

-- | Match uppercase name (constructor/type)
tokUpperName :: Parser (Cst.Name Cst.Proper)
tokUpperName = do
  Tuple tok name <- expectMap extractUpper
  pure { token: tok, name: Cst.Proper name }
  where
    extractUpper (Cst.TokUpperName Nothing name) = Just name
    extractUpper _ = Nothing

-- | Match qualified uppercase name
tokQualifiedUpperName :: Parser (Cst.QualifiedName Cst.Proper)
tokQualifiedUpperName = do
  Tuple tok (Tuple mod name) <- expectMap extractQualUpper
  pure { token: tok, module: mod, name: Cst.Proper name }
  where
    extractQualUpper (Cst.TokUpperName mod name) = Just (Tuple (map Cst.ModuleName mod) name)
    extractQualUpper _ = Nothing

-- | Match operator
tokOperator :: Parser (Cst.QualifiedName Cst.Operator)
tokOperator = do
  Tuple tok (Tuple mod op) <- expectMap extractOp
  pure { token: tok, module: mod, name: Cst.Operator op }
  where
    extractOp (Cst.TokOperator mod op) = Just (Tuple (map Cst.ModuleName mod) op)
    extractOp _ = Nothing

-- | Match keyword by name
tokKeyword :: String -> Parser Cst.SourceToken
tokKeyword kw = satisfy isKw
  where
    isKw (Cst.TokLowerName Nothing name) = name == kw
    isKw _ = false

-- | Match string literal
tokString :: Parser (Tuple Cst.SourceToken String)
tokString = expectMap extractStr
  where
    extractStr (Cst.TokString _ s) = Just s
    extractStr _ = Nothing

-- | Match integer literal
tokInt :: Parser (Tuple Cst.SourceToken Cst.IntValue)
tokInt = expectMap extractInt
  where
    extractInt (Cst.TokInt _ n) = Just n
    extractInt _ = Nothing

-- | Match number literal
tokNumber :: Parser (Tuple Cst.SourceToken Number)
tokNumber = expectMap extractNum
  where
    extractNum (Cst.TokNumber _ n) = Just n
    extractNum _ = Nothing

-- | Match char literal
tokChar :: Parser (Tuple Cst.SourceToken Char)
tokChar = expectMap extractChar
  where
    extractChar (Cst.TokChar _ c) = Just c
    extractChar _ = Nothing

-- ============================================================================
-- Combinators
-- ============================================================================

-- | Optional parser
optional :: forall a. Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- | Many (zero or more)
many :: forall a. Parser a -> Parser (Array a)
many p = Parser \ts -> go ts []
  where
    go tokens acc = case runParser p tokens of
      Right (Tuple a rest) -> go rest (Array.snoc acc a)
      Left _ -> Right (Tuple acc tokens)

-- | Some (one or more)
some :: forall a. Parser a -> Parser (Array a)
some p = do
  first <- p
  rest <- many p
  pure (Array.cons first rest)

-- | Separated by (one or more items separated by sep)
separated :: forall a. Parser a -> Parser Cst.SourceToken -> Parser (Cst.Separated a)
separated p sep = do
  head <- p
  tail <- many (do
    s <- sep
    a <- p
    pure (Tuple s a))
  pure { head, tail }

-- | Optional separated (zero or more)
optionalSeparated :: forall a. Parser a -> Parser Cst.SourceToken -> Parser (Maybe (Cst.Separated a))
optionalSeparated p sep = optional (separated p sep)

-- | Wrapped in delimiters
wrapped :: forall a. Parser Cst.SourceToken -> Parser a -> Parser Cst.SourceToken -> Parser (Cst.Wrapped a)
wrapped open p close = do
  o <- open
  value <- p
  c <- close
  pure { open: o, value, close: c }

-- | Delimited (wrapped optional separated)
delimited :: forall a. Parser Cst.SourceToken -> Parser a -> Parser Cst.SourceToken -> Parser Cst.SourceToken -> Parser (Cst.Delimited a)
delimited open p sep close = wrapped open (optionalSeparated p sep) close

-- | Layout block (between LayoutStart and LayoutEnd)
layoutBlock :: forall a. Parser a -> Parser (Array a)
layoutBlock p = do
  _ <- tokLayoutStart
  items <- layoutItems p
  _ <- tokLayoutEnd <|> pure dummyToken  -- Layout end might be implicit
  pure items
  where
    dummyToken = { range: { start: { line: 0, column: 0 }, end: { line: 0, column: 0 } }
                 , leadingComments: []
                 , trailingComments: []
                 , value: Cst.TokLayoutEnd 0
                 }

-- | Items in a layout block (separated by LayoutSep)
layoutItems :: forall a. Parser a -> Parser (Array a)
layoutItems p = do
  first <- p
  rest <- many (tokLayoutSep *> p)
  pure (Array.cons first rest)

-- ============================================================================
-- Type Parsers
-- ============================================================================

-- | Parse a type (full type with arrows)
parseType :: Parser (Cst.Type Void)
parseType = Control.Lazy.defer \_ -> parseType1

-- | Type with constraints: Constraint => Type
parseType1 :: Parser (Cst.Type Void)
parseType1 = Control.Lazy.defer \_ -> do
  t <- parseType2
  rest <- optional (do
    arr <- tokRightFatArrow
    t2 <- parseType1
    pure (Tuple arr t2))
  case rest of
    Just (Tuple arr t2) -> pure (Cst.TypeConstrained t arr t2)
    Nothing -> pure t

-- | Type with arrows: Type -> Type
parseType2 :: Parser (Cst.Type Void)
parseType2 = Control.Lazy.defer \_ -> do
  t <- parseType3
  rest <- optional (do
    arr <- tokRightArrow
    t2 <- parseType2
    pure (Tuple arr t2))
  case rest of
    Just (Tuple arr t2) -> pure (Cst.TypeArrow t arr t2)
    Nothing -> pure t

-- | Type application: Type Type ...
parseType3 :: Parser (Cst.Type Void)
parseType3 = Control.Lazy.defer \_ -> do
  head <- parseTypeAtom
  args <- many parseTypeAtom
  if Array.null args
    then pure head
    else pure (Cst.TypeApp head args)

-- | Atomic types
parseTypeAtom :: Parser (Cst.Type Void)
parseTypeAtom = Control.Lazy.defer \_ ->
  parseTypeVar
  <|> parseTypeCon
  <|> parseTypeParens
  <|> parseTypeRecord

-- | Type variable
parseTypeVar :: Parser (Cst.Type Void)
parseTypeVar = Cst.TypeVar <$> tokLowerName

-- | Type constructor
parseTypeCon :: Parser (Cst.Type Void)
parseTypeCon = Cst.TypeConstructor <$> tokQualifiedUpperName

-- | Parenthesized type
parseTypeParens :: Parser (Cst.Type Void)
parseTypeParens = Control.Lazy.defer \_ -> do
  w <- wrapped tokLeftParen parseType tokRightParen
  pure (Cst.TypeParens w)

-- | Record type { field :: Type, ... }
parseTypeRecord :: Parser (Cst.Type Void)
parseTypeRecord = Control.Lazy.defer \_ -> do
  w <- wrapped tokLeftBrace parseRow tokRightBrace
  pure (Cst.TypeRecord w)

-- | Row type (inside braces)
parseRow :: Parser (Cst.Row Void)
parseRow = Control.Lazy.defer \_ -> do
  labels <- optionalSeparated parseRowLabel tokComma
  tail <- optional (do
    pipe <- tokPipe
    t <- parseType
    pure (Tuple pipe t))
  pure { labels, tail }

parseRowLabel :: Parser (Cst.Labeled (Cst.Name Cst.Label) (Cst.Type Void))
parseRowLabel = Control.Lazy.defer \_ -> do
  label <- parseLabel
  sep <- tokDoubleColon
  ty <- parseType
  pure { label, separator: sep, value: ty }

parseLabel :: Parser (Cst.Name Cst.Label)
parseLabel = do
  name <- tokLowerName
  pure { token: name.token, name: Cst.Label (unwrapIdent name.name) }
  where
    unwrapIdent (Cst.Ident s) = s

-- ============================================================================
-- Expression Parsers (Precedence-based)
-- ============================================================================

-- | Parse expression (entry point)
parseExpr :: Parser (Cst.Expr Void)
parseExpr = Control.Lazy.defer \_ -> parseExpr1

-- | Expr1: Typed expression (lowest precedence)
parseExpr1 :: Parser (Cst.Expr Void)
parseExpr1 = Control.Lazy.defer \_ -> do
  e <- parseExpr2
  ann <- optional (do
    dc <- tokDoubleColon
    t <- parseType
    pure (Tuple dc t))
  case ann of
    Just (Tuple dc t) -> pure (Cst.ExprTyped e dc t)
    Nothing -> pure e

-- | Expr2: Infix operators
parseExpr2 :: Parser (Cst.Expr Void)
parseExpr2 = Control.Lazy.defer \_ -> do
  e <- parseExpr3
  ops <- many (do
    op <- tokOperator
    e2 <- parseExpr3
    pure (Tuple op e2))
  if Array.null ops
    then pure e
    else pure (Cst.ExprOp e ops)

-- | Expr3: Negation (prefix -)
parseExpr3 :: Parser (Cst.Expr Void)
parseExpr3 = Control.Lazy.defer \_ -> parseNegate <|> parseExpr4
  where
    parseNegate = do
      neg <- satisfy isNegate
      e <- parseExpr3
      pure (Cst.ExprNegate neg e)

    isNegate (Cst.TokOperator Nothing "-") = true
    isNegate _ = false

-- | Expr4: Application
parseExpr4 :: Parser (Cst.Expr Void)
parseExpr4 = Control.Lazy.defer \_ -> do
  head <- parseExpr5
  args <- many parseExpr5
  if Array.null args
    then pure head
    else pure (Cst.ExprApp head args)

-- | Expr5: Control structures and atoms
parseExpr5 :: Parser (Cst.Expr Void)
parseExpr5 = Control.Lazy.defer \_ ->
  parseIf
  <|> parseLet
  <|> parseLambda
  <|> parseCase
  <|> parseDo
  <|> parseExprAtom

-- | If-then-else
parseIf :: Parser (Cst.Expr Void)
parseIf = Control.Lazy.defer \_ -> do
  kw <- tokKeyword "if"
  cond <- parseExpr
  thenKw <- tokKeyword "then"
  trueExpr <- parseExpr
  elseKw <- tokKeyword "else"
  falseExpr <- parseExpr
  pure (Cst.ExprIf { keyword: kw, cond, "then": thenKw, "true": trueExpr, "else": elseKw, "false": falseExpr })

-- | Let-in
parseLet :: Parser (Cst.Expr Void)
parseLet = Control.Lazy.defer \_ -> do
  kw <- tokKeyword "let"
  bindings <- layoutBlock parseLetBinding
  inKw <- tokKeyword "in"
  body <- parseExpr
  pure (Cst.ExprLet { keyword: kw, bindings, "in": inKw, body })

-- | Lambda
parseLambda :: Parser (Cst.Expr Void)
parseLambda = Control.Lazy.defer \_ -> do
  bs <- tokBackslash
  binders <- some parseBinder
  arr <- tokRightArrow
  body <- parseExpr
  pure (Cst.ExprLambda { symbol: bs, binders, arrow: arr, body })

-- | Case expression
parseCase :: Parser (Cst.Expr Void)
parseCase = Control.Lazy.defer \_ -> do
  kw <- tokKeyword "case"
  head <- separated parseExpr tokComma
  ofKw <- tokKeyword "of"
  branches <- layoutBlock parseCaseBranch
  pure (Cst.ExprCase { keyword: kw, head, "of": ofKw, branches })

parseCaseBranch :: Parser (Tuple (Cst.Separated (Cst.Binder Void)) (Cst.Guarded Void))
parseCaseBranch = Control.Lazy.defer \_ -> do
  pats <- separated parseBinder tokComma
  guarded <- parseGuarded
  pure (Tuple pats guarded)

-- | Do block
parseDo :: Parser (Cst.Expr Void)
parseDo = Control.Lazy.defer \_ -> do
  kw <- tokKeyword "do"
  stmts <- layoutBlock parseDoStatement
  pure (Cst.ExprDo { keyword: kw, statements: stmts })

parseDoStatement :: Parser (Cst.DoStatement Void)
parseDoStatement = Control.Lazy.defer \_ ->
  parseDoLet
  <|> parseDoBind
  <|> parseDoDiscard

parseDoLet :: Parser (Cst.DoStatement Void)
parseDoLet = Control.Lazy.defer \_ -> do
  kw <- tokKeyword "let"
  bindings <- layoutBlock parseLetBinding
  pure (Cst.DoLet kw bindings)

parseDoBind :: Parser (Cst.DoStatement Void)
parseDoBind = Control.Lazy.defer \_ -> do
  binder <- parseBinder
  arr <- tokLeftArrow
  expr <- parseExpr
  pure (Cst.DoBind binder arr expr)

parseDoDiscard :: Parser (Cst.DoStatement Void)
parseDoDiscard = Control.Lazy.defer \_ -> Cst.DoDiscard <$> parseExpr

-- | Let binding
parseLetBinding :: Parser (Cst.LetBinding Void)
parseLetBinding = Control.Lazy.defer \_ ->
  parseLetSig
  <|> parseLetName

parseLetSig :: Parser (Cst.LetBinding Void)
parseLetSig = Control.Lazy.defer \_ -> do
  name <- tokLowerName
  dc <- tokDoubleColon
  ty <- parseType
  pure (Cst.LetBindingSignature { label: name, separator: dc, value: ty })

parseLetName :: Parser (Cst.LetBinding Void)
parseLetName = Control.Lazy.defer \_ -> do
  name <- tokLowerName
  binders <- many parseBinder
  guarded <- parseGuarded
  pure (Cst.LetBindingName { name, binders, guarded })

-- | Guarded expression (= expr or guards)
parseGuarded :: Parser (Cst.Guarded Void)
parseGuarded = Control.Lazy.defer \_ -> parseUnconditional <|> parseGuardedExprs

parseUnconditional :: Parser (Cst.Guarded Void)
parseUnconditional = Control.Lazy.defer \_ -> do
  eq <- tokEquals
  expr <- parseExpr
  -- Optional where clause
  whereClause <- optional (do
    wh <- tokKeyword "where"
    bindings <- layoutBlock parseLetBinding
    pure (Tuple wh bindings))
  pure (Cst.Unconditional eq { expr, bindings: whereClause })

parseGuardedExprs :: Parser (Cst.Guarded Void)
parseGuardedExprs = Control.Lazy.defer \_ -> do
  guards <- some parseGuardedExpr
  pure (Cst.Guarded guards)

parseGuardedExpr :: Parser (Cst.GuardedExpr Void)
parseGuardedExpr = Control.Lazy.defer \_ -> do
  bar <- tokPipe
  patterns <- separated parsePatternGuard tokComma
  sep <- tokEquals
  expr <- parseExpr
  whereClause <- optional (do
    wh <- tokKeyword "where"
    bindings <- layoutBlock parseLetBinding
    pure (Tuple wh bindings))
  pure { bar, patterns, separator: sep, "where": { expr, bindings: whereClause } }

parsePatternGuard :: Parser (Cst.PatternGuard Void)
parsePatternGuard = Control.Lazy.defer \_ -> do
  -- Try pattern guard first: binder <- expr
  patBind <- optional (do
    b <- parseBinder
    arr <- tokLeftArrow
    pure (Tuple b arr))
  expr <- parseExpr
  pure { binder: patBind, expr }

-- | Atomic expressions
parseExprAtom :: Parser (Cst.Expr Void)
parseExprAtom = Control.Lazy.defer \_ ->
  parseExprIdent
  <|> parseExprConstructor
  <|> parseExprLiteral
  <|> parseExprArray
  <|> parseExprRecord
  <|> parseExprParens

parseExprIdent :: Parser (Cst.Expr Void)
parseExprIdent = Cst.ExprIdent <$> tokQualifiedLowerName

parseExprConstructor :: Parser (Cst.Expr Void)
parseExprConstructor = Cst.ExprConstructor <$> tokQualifiedUpperName

parseExprLiteral :: Parser (Cst.Expr Void)
parseExprLiteral =
  parseExprString
  <|> parseExprInt
  <|> parseExprNumber
  <|> parseExprChar
  <|> parseExprBool

parseExprString :: Parser (Cst.Expr Void)
parseExprString = do
  Tuple tok s <- tokString
  pure (Cst.ExprString tok s)

parseExprInt :: Parser (Cst.Expr Void)
parseExprInt = do
  Tuple tok n <- tokInt
  pure (Cst.ExprInt tok n)

parseExprNumber :: Parser (Cst.Expr Void)
parseExprNumber = do
  Tuple tok n <- tokNumber
  pure (Cst.ExprNumber tok n)

parseExprChar :: Parser (Cst.Expr Void)
parseExprChar = do
  Tuple tok c <- tokChar
  pure (Cst.ExprChar tok c)

parseExprBool :: Parser (Cst.Expr Void)
parseExprBool = parseTrue <|> parseFalse
  where
    parseTrue = do
      tok <- tokKeyword "true"
      pure (Cst.ExprBoolean tok true)
    parseFalse = do
      tok <- tokKeyword "false"
      pure (Cst.ExprBoolean tok false)

parseExprArray :: Parser (Cst.Expr Void)
parseExprArray = Control.Lazy.defer \_ -> do
  d <- delimited tokLeftSquare parseExpr tokComma tokRightSquare
  pure (Cst.ExprArray d)

parseExprRecord :: Parser (Cst.Expr Void)
parseExprRecord = Control.Lazy.defer \_ -> do
  d <- delimited tokLeftBrace parseRecordField tokComma tokRightBrace
  pure (Cst.ExprRecord d)

parseRecordField :: Parser (Cst.RecordLabeled (Cst.Expr Void))
parseRecordField = Control.Lazy.defer \_ -> parseRecordFieldFull <|> parseRecordPun

parseRecordFieldFull :: Parser (Cst.RecordLabeled (Cst.Expr Void))
parseRecordFieldFull = Control.Lazy.defer \_ -> do
  label <- parseLabel
  sep <- tokDoubleColon <|> tokEquals  -- Allow both : and =
  value <- parseExpr
  pure (Cst.RecordField label sep value)

parseRecordPun :: Parser (Cst.RecordLabeled (Cst.Expr Void))
parseRecordPun = Cst.RecordPun <$> tokLowerName

parseExprParens :: Parser (Cst.Expr Void)
parseExprParens = Control.Lazy.defer \_ -> do
  w <- wrapped tokLeftParen parseExpr tokRightParen
  pure (Cst.ExprParens w)

-- ============================================================================
-- Binder (Pattern) Parsers
-- ============================================================================

parseBinder :: Parser (Cst.Binder Void)
parseBinder = Control.Lazy.defer \_ -> parseBinder1

-- | Binder with operators
parseBinder1 :: Parser (Cst.Binder Void)
parseBinder1 = Control.Lazy.defer \_ -> do
  b <- parseBinder2
  ops <- many (do
    op <- tokOperator
    b2 <- parseBinder2
    pure (Tuple op b2))
  if Array.null ops
    then pure b
    else pure (Cst.BinderOp b ops)

-- | Binder with type annotation
parseBinder2 :: Parser (Cst.Binder Void)
parseBinder2 = Control.Lazy.defer \_ -> do
  b <- parseBinder3
  ann <- optional (do
    dc <- tokDoubleColon
    t <- parseType
    pure (Tuple dc t))
  case ann of
    Just (Tuple dc t) -> pure (Cst.BinderTyped b dc t)
    Nothing -> pure b

-- | Constructor application
parseBinder3 :: Parser (Cst.Binder Void)
parseBinder3 = Control.Lazy.defer \_ -> parseBinderCon <|> parseBinderAtom

parseBinderCon :: Parser (Cst.Binder Void)
parseBinderCon = Control.Lazy.defer \_ -> do
  con <- tokQualifiedUpperName
  args <- many parseBinderAtom
  pure (Cst.BinderConstructor con args)

-- | Atomic binders
parseBinderAtom :: Parser (Cst.Binder Void)
parseBinderAtom = Control.Lazy.defer \_ ->
  parseBinderWildcard
  <|> parseBinderVar
  <|> parseBinderLiteral
  <|> parseBinderArray
  <|> parseBinderRecord
  <|> parseBinderParens

parseBinderWildcard :: Parser (Cst.Binder Void)
parseBinderWildcard = do
  tok <- token Cst.TokUnderscore
  pure (Cst.BinderWildcard tok)

parseBinderVar :: Parser (Cst.Binder Void)
parseBinderVar = Control.Lazy.defer \_ -> do
  name <- tokLowerName
  -- Check for named pattern: name@pattern
  named <- optional (do
    at <- token Cst.TokAt
    b <- parseBinderAtom
    pure (Tuple at b))
  case named of
    Just (Tuple at b) -> pure (Cst.BinderNamed name at b)
    Nothing -> pure (Cst.BinderVar name)

parseBinderLiteral :: Parser (Cst.Binder Void)
parseBinderLiteral =
  parseBinderString
  <|> parseBinderInt
  <|> parseBinderNumber
  <|> parseBinderChar
  <|> parseBinderBool

parseBinderString :: Parser (Cst.Binder Void)
parseBinderString = do
  Tuple tok s <- tokString
  pure (Cst.BinderString tok s)

parseBinderInt :: Parser (Cst.Binder Void)
parseBinderInt = do
  neg <- optional (satisfy isNeg)
  Tuple tok n <- tokInt
  pure (Cst.BinderInt neg tok n)
  where
    isNeg (Cst.TokOperator Nothing "-") = true
    isNeg _ = false

parseBinderNumber :: Parser (Cst.Binder Void)
parseBinderNumber = do
  neg <- optional (satisfy isNeg)
  Tuple tok n <- tokNumber
  pure (Cst.BinderNumber neg tok n)
  where
    isNeg (Cst.TokOperator Nothing "-") = true
    isNeg _ = false

parseBinderChar :: Parser (Cst.Binder Void)
parseBinderChar = do
  Tuple tok c <- tokChar
  pure (Cst.BinderChar tok c)

parseBinderBool :: Parser (Cst.Binder Void)
parseBinderBool = parseBinderTrue <|> parseBinderFalse
  where
    parseBinderTrue = do
      tok <- tokKeyword "true"
      pure (Cst.BinderBoolean tok true)
    parseBinderFalse = do
      tok <- tokKeyword "false"
      pure (Cst.BinderBoolean tok false)

parseBinderArray :: Parser (Cst.Binder Void)
parseBinderArray = Control.Lazy.defer \_ -> do
  d <- delimited tokLeftSquare parseBinder tokComma tokRightSquare
  pure (Cst.BinderArray d)

parseBinderRecord :: Parser (Cst.Binder Void)
parseBinderRecord = Control.Lazy.defer \_ -> do
  d <- delimited tokLeftBrace parseBinderRecordField tokComma tokRightBrace
  pure (Cst.BinderRecord d)

parseBinderRecordField :: Parser (Cst.RecordLabeled (Cst.Binder Void))
parseBinderRecordField = Control.Lazy.defer \_ -> parseBinderRecordFull <|> parseBinderRecordPun

parseBinderRecordFull :: Parser (Cst.RecordLabeled (Cst.Binder Void))
parseBinderRecordFull = Control.Lazy.defer \_ -> do
  label <- parseLabel
  sep <- tokDoubleColon <|> tokEquals
  value <- parseBinder
  pure (Cst.RecordField label sep value)

parseBinderRecordPun :: Parser (Cst.RecordLabeled (Cst.Binder Void))
parseBinderRecordPun = Cst.RecordPun <$> tokLowerName

parseBinderParens :: Parser (Cst.Binder Void)
parseBinderParens = Control.Lazy.defer \_ -> do
  w <- wrapped tokLeftParen parseBinder tokRightParen
  pure (Cst.BinderParens w)

-- ============================================================================
-- Module & Declaration Parsers
-- ============================================================================

-- | Parse a complete module
parseModule :: Parser (Cst.Module Void)
parseModule = do
  header <- parseModuleHeader
  body <- parseModuleBody
  pure { header, body }

parseModuleHeader :: Parser (Cst.ModuleHeader Void)
parseModuleHeader = do
  kw <- tokKeyword "module"
  name <- parseModuleName
  exports <- optional parseExports
  whereKw <- tokKeyword "where"
  imports <- many parseImport
  pure { keyword: kw, name, exports, where: whereKw, imports }

parseModuleName :: Parser (Cst.Name Cst.ModuleName)
parseModuleName = do
  name <- tokUpperName
  pure { token: name.token, name: Cst.ModuleName (unwrapProper name.name) }
  where
    unwrapProper (Cst.Proper s) = s

parseExports :: Parser (Cst.DelimitedNonEmpty (Cst.Export Void))
parseExports = do
  open <- tokLeftParen
  exports <- separated parseExport tokComma
  close <- tokRightParen
  pure { open, value: exports, close }

parseExport :: Parser (Cst.Export Void)
parseExport =
  parseExportValue
  <|> parseExportType
  <|> parseExportModule

parseExportValue :: Parser (Cst.Export Void)
parseExportValue = Cst.ExportValue <$> tokLowerName

parseExportType :: Parser (Cst.Export Void)
parseExportType = do
  name <- tokUpperName
  members <- optional parseDataMembers
  pure (Cst.ExportType name members)

parseExportModule :: Parser (Cst.Export Void)
parseExportModule = do
  kw <- tokKeyword "module"
  name <- parseModuleName
  pure (Cst.ExportModule kw name)

parseDataMembers :: Parser Cst.DataMembers
parseDataMembers = parseDataAll <|> parseDataEnumerated

parseDataAll :: Parser Cst.DataMembers
parseDataAll = do
  _ <- tokLeftParen
  dot <- tokDot
  _ <- tokDot
  _ <- tokRightParen
  pure (Cst.DataAll dot)

parseDataEnumerated :: Parser Cst.DataMembers
parseDataEnumerated = do
  d <- delimited tokLeftParen tokUpperName tokComma tokRightParen
  pure (Cst.DataEnumerated d)

parseImport :: Parser (Cst.ImportDecl Void)
parseImport = do
  kw <- tokKeyword "import"
  mod <- parseModuleName
  names <- optional (do
    hiding <- optional (tokKeyword "hiding")
    imports <- wrapped tokLeftParen (separated parseImportItem tokComma) tokRightParen
    pure (Tuple hiding imports))
  qualified <- optional (do
    as <- tokKeyword "as"
    alias <- parseModuleName
    pure (Tuple as alias))
  pure { keyword: kw, module: mod, names, qualified }

parseImportItem :: Parser (Cst.Import Void)
parseImportItem =
  parseImportValue
  <|> parseImportType
  <|> parseImportClass

parseImportValue :: Parser (Cst.Import Void)
parseImportValue = Cst.ImportValue <$> tokLowerName

parseImportType :: Parser (Cst.Import Void)
parseImportType = do
  name <- tokUpperName
  members <- optional parseDataMembers
  pure (Cst.ImportType name members)

parseImportClass :: Parser (Cst.Import Void)
parseImportClass = do
  kw <- tokKeyword "class"
  name <- tokUpperName
  pure (Cst.ImportClass kw name)

parseModuleBody :: Parser (Cst.ModuleBody Void)
parseModuleBody = do
  decls <- many parseDeclaration
  pure { decls, trailingComments: [], end: { line: 0, column: 0 } }

-- | Parse a declaration
parseDeclaration :: Parser (Cst.Declaration Void)
parseDeclaration =
  parseDeclData
  <|> parseDeclType
  <|> parseDeclNewtype
  <|> parseDeclClass
  <|> parseDeclInstance
  <|> parseDeclForeign
  <|> parseDeclFixity
  <|> parseDeclSignature
  <|> parseDeclValue

parseDeclData :: Parser (Cst.Declaration Void)
parseDeclData = do
  kw <- tokKeyword "data"
  name <- tokUpperName
  vars <- many parseTypeVarBinding
  ctors <- optional (do
    eq <- tokEquals
    cs <- separated parseDataCtor tokPipe
    pure (Tuple eq cs))
  pure (Cst.DeclData { keyword: kw, name, vars } ctors)

parseDataCtor :: Parser (Cst.DataCtor Void)
parseDataCtor = do
  name <- tokUpperName
  fields <- many parseTypeAtom
  pure { name, fields }

parseTypeVarBinding :: Parser (Cst.TypeVarBinding Void)
parseTypeVarBinding = parseTypeVarName

parseTypeVarName :: Parser (Cst.TypeVarBinding Void)
parseTypeVarName = Cst.TypeVarName <$> tokLowerName

parseDeclType :: Parser (Cst.Declaration Void)
parseDeclType = do
  kw <- tokKeyword "type"
  name <- tokUpperName
  vars <- many parseTypeVarBinding
  eq <- tokEquals
  ty <- parseType
  pure (Cst.DeclType { keyword: kw, name, vars } eq ty)

parseDeclNewtype :: Parser (Cst.Declaration Void)
parseDeclNewtype = do
  kw <- tokKeyword "newtype"
  name <- tokUpperName
  vars <- many parseTypeVarBinding
  eq <- tokEquals
  ctorName <- tokUpperName
  wrappedTy <- parseTypeAtom
  pure (Cst.DeclNewtype { keyword: kw, name, vars } eq ctorName wrappedTy)

parseDeclClass :: Parser (Cst.Declaration Void)
parseDeclClass = do
  kw <- tokKeyword "class"
  -- TODO: super constraints
  name <- tokUpperName
  vars <- many parseTypeVarBinding
  -- TODO: fundeps
  methods <- optional (do
    whereKw <- tokKeyword "where"
    ms <- layoutBlock parseClassMember
    pure (Tuple whereKw ms))
  pure (Cst.DeclClass { keyword: kw, super: Nothing, name, vars, fundeps: Nothing } methods)

parseClassMember :: Parser (Cst.Labeled (Cst.Name Cst.Ident) (Cst.Type Void))
parseClassMember = do
  name <- tokLowerName
  dc <- tokDoubleColon
  ty <- parseType
  pure { label: name, separator: dc, value: ty }

parseDeclInstance :: Parser (Cst.Declaration Void)
parseDeclInstance = do
  kw <- tokKeyword "instance"
  name <- optional (do
    n <- tokLowerName
    dc <- tokDoubleColon
    pure (Tuple n dc))
  -- TODO: constraints
  className <- tokQualifiedUpperName
  types <- many parseTypeAtom
  body <- optional (do
    whereKw <- tokKeyword "where"
    bindings <- layoutBlock parseInstanceBinding
    pure (Tuple whereKw bindings))
  let instanceHead = { keyword: kw
                     , name
                     , constraints: Nothing
                     , className
                     , types
                     }
  let inst = { head: instanceHead, body }
  pure (Cst.DeclInstanceChain { head: inst, tail: [] })

parseInstanceBinding :: Parser (Cst.InstanceBinding Void)
parseInstanceBinding = parseInstanceSig <|> parseInstanceName

parseInstanceSig :: Parser (Cst.InstanceBinding Void)
parseInstanceSig = do
  name <- tokLowerName
  dc <- tokDoubleColon
  ty <- parseType
  pure (Cst.InstanceBindingSignature { label: name, separator: dc, value: ty })

parseInstanceName :: Parser (Cst.InstanceBinding Void)
parseInstanceName = do
  name <- tokLowerName
  binders <- many parseBinder
  guarded <- parseGuarded
  pure (Cst.InstanceBindingName { name, binders, guarded })

parseDeclForeign :: Parser (Cst.Declaration Void)
parseDeclForeign = do
  foreign_ <- tokKeyword "foreign"
  import_ <- tokKeyword "import"
  foreign' <- parseForeignValue
  pure (Cst.DeclForeign foreign_ import_ foreign')

parseForeignValue :: Parser (Cst.Foreign Void)
parseForeignValue = do
  name <- tokLowerName
  dc <- tokDoubleColon
  ty <- parseType
  pure (Cst.ForeignValue { label: name, separator: dc, value: ty })

parseDeclFixity :: Parser (Cst.Declaration Void)
parseDeclFixity = do
  Tuple kw fixity <- parseFixityKeyword
  Tuple precTok prec <- tokInt
  op <- parseFixityOp
  pure (Cst.DeclFixity { keyword: Tuple kw fixity, prec: Tuple precTok (intValueToInt prec), operator: op })
  where
    intValueToInt (Cst.SmallInt n) = n
    intValueToInt (Cst.BigInt _) = 0

parseFixityKeyword :: Parser (Tuple Cst.SourceToken Cst.Fixity)
parseFixityKeyword =
  parseInfix <|> parseInfixl <|> parseInfixr
  where
    parseInfix = do
      tok <- tokKeyword "infix"
      pure (Tuple tok Cst.Infix)
    parseInfixl = do
      tok <- tokKeyword "infixl"
      pure (Tuple tok Cst.Infixl)
    parseInfixr = do
      tok <- tokKeyword "infixr"
      pure (Tuple tok Cst.Infixr)

parseFixityOp :: Parser Cst.FixityOp
parseFixityOp = do
  -- For now just parse: name as op
  name <- tokQualifiedLowerName
  as <- tokKeyword "as"
  op <- parseOpName
  pure (Cst.FixityValue (toEitherName name) as op)
  where
    toEitherName qn = { token: qn.token, module: qn.module, name: Left qn.name }

parseOpName :: Parser (Cst.Name Cst.Operator)
parseOpName = do
  op <- tokOperator
  pure { token: op.token, name: op.name }

parseDeclSignature :: Parser (Cst.Declaration Void)
parseDeclSignature = do
  name <- tokLowerName
  dc <- tokDoubleColon
  ty <- parseType
  pure (Cst.DeclSignature { label: name, separator: dc, value: ty })

parseDeclValue :: Parser (Cst.Declaration Void)
parseDeclValue = do
  name <- tokLowerName
  binders <- many parseBinder
  guarded <- parseGuarded
  pure (Cst.DeclValue { name, binders, guarded })
