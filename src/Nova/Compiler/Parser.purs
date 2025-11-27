module Nova.Compiler.Parser where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Int as Int
import Data.Number as Number
import Nova.Compiler.Tokenizer (Token, TokenType(..))
import Nova.Compiler.Ast as Ast

-- | Parser result type
type ParseResult a = Either String (Tuple a (Array Token))

-- | Helper to create success result
success a tokens = Right (Tuple a tokens)

-- | Helper to create error result
failure msg = Left msg

-- ------------------------------------------------------------
-- Helpers for newline-aware token handling
-- ------------------------------------------------------------

skipNewlines :: Array Token -> Array Token
skipNewlines tokens = Array.dropWhile (\t -> t.tokenType == TokNewline) tokens

dropNewlines :: Array Token -> Array Token
dropNewlines = skipNewlines

stripNewlines :: Array Token -> Array Token
stripNewlines = Array.filter (\t -> t.tokenType /= TokNewline)

-- | Check if a string starts with a lowercase letter
isLowerCase :: String -> Boolean
isLowerCase s = case CU.charAt 0 s of
  Just c -> c >= 'a' && c <= 'z'
  Nothing -> false

-- ------------------------------------------------------------
-- Token matching helpers
-- ------------------------------------------------------------

expectKeyword :: Array Token -> String -> ParseResult String
expectKeyword tokens expected =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokKeyword && t.value == expected
      then success expected (Array.drop 1 ts)
      else failure $ "Expected keyword '" <> expected <> "'"
    Nothing -> failure $ "Expected keyword '" <> expected <> "'"

expectOperator :: Array Token -> String -> ParseResult String
expectOperator tokens expected =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokOperator && t.value == expected
      then success expected (Array.drop 1 ts)
      else failure $ "Expected operator '" <> expected <> "'"
    Nothing -> failure $ "Expected operator '" <> expected <> "'"

expectDelimiter :: Array Token -> String -> ParseResult String
expectDelimiter tokens expected =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == expected
      then success expected (Array.drop 1 ts)
      else failure $ "Expected delimiter '" <> expected <> "'"
    Nothing -> failure $ "Expected delimiter '" <> expected <> "'"

expectColon :: Array Token -> ParseResult String
expectColon tokens =
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokOperator && t.value == ":"
      then success ":" (Array.drop 1 tokens)
      else failure "Expected ':'  operator"
    Nothing -> failure "Expected ':'  operator"

-- ------------------------------------------------------------
-- Basic parsers
-- ------------------------------------------------------------

parseIdentifier :: Array Token -> ParseResult Ast.Expr
parseIdentifier tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokIdentifier
      then success (Ast.ExprVar t.value) (Array.drop 1 ts)
      else failure "Expected identifier"
    Nothing -> failure "Expected identifier"

parseIdentifierName :: Array Token -> ParseResult String
parseIdentifierName tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokIdentifier
      then success t.value (Array.drop 1 ts)
      else failure "Expected identifier"
    Nothing -> failure "Expected identifier"

parseLabel :: Array Token -> ParseResult String
parseLabel tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokIdentifier || t.tokenType == TokKeyword
      then success t.value (Array.drop 1 ts)
      else failure "Expected label"
    Nothing -> failure "Expected label"

parseLiteral :: Array Token -> ParseResult Ast.Literal
parseLiteral tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t -> case t.tokenType of
      TokNumber ->
        -- Check if it's an integer or float
        if String.contains (String.Pattern ".") t.value
        then success (Ast.LitNumber (readNumber t.value)) (Array.drop 1 ts)
        else success (Ast.LitInt (readInt t.value)) (Array.drop 1 ts)
      TokString -> success (Ast.LitString t.value) (Array.drop 1 ts)
      TokChar -> success (Ast.LitChar (firstChar t.value)) (Array.drop 1 ts)
      TokIdentifier ->
        if t.value == "true"
        then success (Ast.LitBool true) (Array.drop 1 ts)
        else if t.value == "false"
        then success (Ast.LitBool false) (Array.drop 1 ts)
        else failure "Expected literal"
      _ -> failure "Expected literal"
    Nothing -> failure "Expected literal"
  where
    readInt :: String -> Int
    readInt s = case Int.fromString s of
      Just n -> n
      Nothing -> 0

    readNumber :: String -> Number
    readNumber s = case Number.fromString s of
      Just n -> n
      Nothing -> 0.0

    firstChar :: String -> Char
    firstChar s = case CU.charAt 0 s of
      Just c -> c
      Nothing -> ' '

parseStringLiteral :: Array Token -> ParseResult String
parseStringLiteral tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokString
      then success t.value (Array.drop 1 ts)
      else failure "Expected string literal"
    Nothing -> failure "Expected string literal"

-- ------------------------------------------------------------
-- Combinator helpers
-- ------------------------------------------------------------

parseAny parsers tokens = go parsers
  where
    go ps = case Array.head ps of
      Nothing -> failure "No parser succeeded"
      Just p -> case p tokens of
        Right result -> Right result
        Left _ -> go (Array.drop 1 ps)

parseMany parser tokens = go tokens []
  where
    go toks acc = case parser toks of
      Right (Tuple result rest) -> go rest (Array.snoc acc result)
      Left _ -> success acc toks

parseSeparated parser separator tokens =
  case parser tokens of
    Left err -> Left err
    Right (Tuple first rest) -> parseSeparatedRest parser separator rest [first]

parseSeparatedRest parser separator tokens acc =
  case separator tokens of
    Right (Tuple _ rest) -> case parser rest of
      Right (Tuple item rest') -> parseSeparatedRest parser separator rest' (Array.snoc acc item)
      Left _ -> failure "Expected item after separator"
    Left _ -> success acc tokens

-- ------------------------------------------------------------
-- Qualified identifier parsing
-- ------------------------------------------------------------

parseQualifiedIdentifier :: Array Token -> ParseResult Ast.Expr
parseQualifiedIdentifier tokens =
  -- Don't consume an identifier at column 1 followed by :: (type signature start)
  let tokens' = skipNewlines tokens
  in case Array.head tokens' of
    Just t ->
      if t.tokenType == TokIdentifier && t.column == 1
      then case Array.head (Array.drop 1 tokens') of
        Just t' ->
          if t'.tokenType == TokOperator && t'.value == "::"
          then failure "Not consuming top-level type signature"
          else parseQualifiedIdentifierInner tokens'
        _ -> parseQualifiedIdentifierInner tokens'
      else parseQualifiedIdentifierInner tokens'
    _ -> parseQualifiedIdentifierInner tokens'
  where
    parseQualifiedIdentifierInner toks =
      case parseSeparated parseIdentifierName (\t -> expectOperator t ".") toks of
        Left err -> Left err
        Right (Tuple parts rest) -> case Array.length parts of
          0 -> failure "Expected identifier"
          1 -> case Array.head parts of
            Just name -> success (Ast.ExprVar name) rest
            Nothing -> failure "Expected identifier"
          _ ->
            -- Multiple parts separated by dots
            -- If first part starts with lowercase, it's record field access chain
            -- If first part starts with uppercase, it's a qualified identifier
            case Array.head parts of
              Just first ->
                if not (isUpperCase first)
                then
                  -- Record field access: state.line.foo -> ((state).line).foo
                  let baseExpr = Ast.ExprVar first
                      fields = Array.drop 1 parts
                  in success (Array.foldl Ast.ExprRecordAccess baseExpr fields) rest
                else
                  -- Qualified identifier: Data.Array.head -> ExprQualified "Data.Array" "head"
                  let allButLast = Array.take (Array.length parts - 1) parts
                      lastName = Array.last parts
                  in case lastName of
                    Just name -> success (Ast.ExprQualified (String.joinWith "." allButLast) name) rest
                    Nothing -> failure "Expected qualified identifier"
              _ ->
                -- Qualified identifier: Data.Array.head -> ExprQualified "Data.Array" "head"
                let allButLast = Array.take (Array.length parts - 1) parts
                    lastName = Array.last parts
                in case lastName of
                  Just name -> success (Ast.ExprQualified (String.joinWith "." allButLast) name) rest
                  Nothing -> failure "Expected qualified identifier"

    isUpperCase :: String -> Boolean
    isUpperCase s = case CU.charAt 0 s of
      Just c -> c >= 'A' && c <= 'Z'
      Nothing -> false

-- ------------------------------------------------------------
-- Type parsing
-- ------------------------------------------------------------

parseType :: Array Token -> ParseResult Ast.TypeExpr
parseType tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokKeyword && t.value == "forall"
      then parseForallType ts
      else parseFunctionType tokens
    Nothing -> parseFunctionType tokens

parseForallType :: Array Token -> ParseResult Ast.TypeExpr
parseForallType tokens = do
  Tuple _ rest <- expectKeyword tokens "forall"
  Tuple vars rest' <- parseMany parseIdentifierName rest
  Tuple _ rest'' <- expectOperator rest' "."
  Tuple ty rest''' <- parseType rest''
  success (Ast.TyExprForAll vars ty) rest'''

parseFunctionType :: Array Token -> ParseResult Ast.TypeExpr
parseFunctionType tokens = do
  Tuple left rest <- parseTypeTerm tokens
  let ts = skipNewlines rest
  case Array.head ts of
    Just t ->
      if t.tokenType == TokOperator && t.value == "->"
      then do
        Tuple right rest' <- parseFunctionType (Array.drop 1 ts)
        success (Ast.TyExprArrow left right) rest'
      else success left rest
    Nothing -> success left rest

parseTypeTerm :: Array Token -> ParseResult Ast.TypeExpr
parseTypeTerm tokens =
  parseAny
    [ parseRecordType
    , parseListType
    , parseTupleType
    , parseBasicType
    ]
    tokens

parseRecordType :: Array Token -> ParseResult Ast.TypeExpr
parseRecordType tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "{"
      then do
        Tuple fields rest <- parseSeparated parseRecordField (\tok -> expectDelimiter tok ",") (Array.drop 1 ts)
        Tuple _ rest' <- expectDelimiter rest "}"
        success (Ast.TyExprRecord fields Nothing) rest'
      else failure "Expected record type"
    Nothing -> failure "Expected record type"

parseRecordField :: Array Token -> ParseResult (Tuple String Ast.TypeExpr)
parseRecordField tokens = do
  Tuple label rest <- parseLabel tokens
  Tuple _ rest' <- expectOperator rest "::"
  let rest'' = skipNewlines rest'
  Tuple ty rest''' <- parseType rest''
  success (Tuple label ty) rest'''

parseListType :: Array Token -> ParseResult Ast.TypeExpr
parseListType tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "["
      then do
        Tuple elemType rest <- parseType (Array.drop 1 ts)
        Tuple _ rest' <- expectDelimiter rest "]"
        success (Ast.TyExprApp (Ast.TyExprCon "Array") elemType) rest'
      else failure "Expected list type"
    Nothing -> failure "Expected list type"

parseTupleType :: Array Token -> ParseResult Ast.TypeExpr
parseTupleType tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "("
      then do
        Tuple elements rest <- parseSeparated parseType (\tok -> expectDelimiter tok ",") (Array.drop 1 ts)
        Tuple _ rest' <- expectDelimiter rest ")"
        case Array.length elements of
          1 -> case Array.head elements of
            Just e -> success e rest'
            Nothing -> failure "Expected type"
          _ -> success (Ast.TyExprTuple elements) rest'
      else failure "Expected tuple type"
    Nothing -> failure "Expected tuple type"

parseBasicType :: Array Token -> ParseResult Ast.TypeExpr
parseBasicType tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokIdentifier
      then do
        -- Check for qualified type name (e.g., Ast.Expr)
        Tuple name rest <- parseQualifiedTypeName ts
        Tuple args rest' <- parseMany parseTypeAtom rest
        -- Lowercase names are type variables, uppercase are type constructors
        let base = if isLowerCase name then Ast.TyExprVar name else Ast.TyExprCon name
        case Array.length args of
          0 -> success base rest'
          _ -> success (foldTypeApp base args) rest'
      else if t.tokenType == TokDelimiter && t.value == "("
      then do
        Tuple ty rest <- parseType (Array.drop 1 ts)
        Tuple _ rest' <- expectDelimiter rest ")"
        success ty rest'
      else failure "Expected basic type"
    Nothing -> failure "Expected basic type"
  where
    foldTypeApp :: Ast.TypeExpr -> Array Ast.TypeExpr -> Ast.TypeExpr
    foldTypeApp base args = Array.foldl Ast.TyExprApp base args

-- | Parse a qualified type name like "Ast.Expr" or just "Int"
parseQualifiedTypeName :: Array Token -> ParseResult String
parseQualifiedTypeName tokens =
  case parseSeparated parseIdentifierName (\t -> expectOperator t ".") tokens of
    Left err -> Left err
    Right (Tuple parts rest) -> success (String.joinWith "." parts) rest

parseTypeAtom :: Array Token -> ParseResult Ast.TypeExpr
parseTypeAtom tokens =
  -- Don't skip newlines here - a blank line (newline followed by token at column 1)
  -- indicates a new declaration, not a continuation of type arguments.
  -- Only accept atoms on the same line or properly indented.
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokNewline
      then
        -- Check what follows the newline
        let rest = Array.drop 1 tokens in
        case Array.head rest of
          Just t' ->
            if t'.tokenType == TokNewline
            then parseTypeAtom rest
            else if t'.column <= 1
            then failure "Expected type atom"
            else parseTypeAtom rest
          _ -> parseTypeAtom rest
      else if t.tokenType == TokDelimiter && t.value == "{"
      then parseRecordType tokens
      else if t.tokenType == TokIdentifier
      then do
        -- Handle qualified type names like Ast.Expr
        Tuple name rest <- parseQualifiedTypeName tokens
        -- Lowercase names are type variables, uppercase are type constructors
        success (if isLowerCase name then Ast.TyExprVar name else Ast.TyExprCon name) rest
      else if t.tokenType == TokDelimiter && t.value == "("
      then do
        Tuple ty rest <- parseType (Array.drop 1 tokens)
        Tuple _ rest' <- expectDelimiter rest ")"
        success ty rest'
      else if t.tokenType == TokDelimiter && t.value == "["
      then do
        Tuple elemType rest <- parseType (Array.drop 1 tokens)
        Tuple _ rest' <- expectDelimiter rest "]"
        success (Ast.TyExprApp (Ast.TyExprCon "Array") elemType) rest'
      else failure "Expected type atom"
    _ -> failure "Expected type atom"

-- ------------------------------------------------------------
-- Pattern parsing
-- ------------------------------------------------------------

parsePattern :: Array Token -> ParseResult Ast.Pattern
parsePattern tokens =
  parseAny
    [ parseRecordPattern
    , parseWildcardPattern
    , parseConsPattern
    , parseConstructorPattern
    , parseParenPattern  -- Must come before parseTuplePattern to handle (h : t)
    , parseTuplePattern
    , parseListPattern
    , parseLiteralPattern
    , parseVarPattern
    ]
    tokens

parseVarPattern :: Array Token -> ParseResult Ast.Pattern
parseVarPattern tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokIdentifier
      then success (Ast.PatVar t.value) (Array.drop 1 ts)
      else failure "Expected variable pattern"
    Nothing -> failure "Expected variable pattern"

parseWildcardPattern :: Array Token -> ParseResult Ast.Pattern
parseWildcardPattern tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokIdentifier && t.value == "_"
      then success Ast.PatWildcard (Array.drop 1 ts)
      else failure "Expected wildcard"
    Nothing -> failure "Expected wildcard"

parseLiteralPattern :: Array Token -> ParseResult Ast.Pattern
parseLiteralPattern tokens = do
  Tuple lit rest <- parseLiteral tokens
  success (Ast.PatLit lit) rest

parseConstructorPattern :: Array Token -> ParseResult Ast.Pattern
parseConstructorPattern tokens = do
  -- Handle qualified constructor names like Ast.PatVar
  Tuple name rest <- parseQualifiedConstructorName tokens
  if isCapital name then do
    Tuple args rest' <- parseMany parseSimplePattern rest
    case Array.length args of
      0 -> success (Ast.PatCon name []) rest'
      _ -> success (Ast.PatCon name args) rest'
  else
    failure "Expected constructor pattern"
  where
    isCapital :: String -> Boolean
    isCapital s = case CU.charAt 0 s of
      Just c -> c >= 'A' && c <= 'Z'
      Nothing -> false

-- | Parse a qualified constructor name like "Ast.PatVar" or just "Just"
parseQualifiedConstructorName :: Array Token -> ParseResult String
parseQualifiedConstructorName tokens =
  case parseSeparated parseIdentifierName (\t -> expectOperator t ".") tokens of
    Left err -> Left err
    Right (Tuple parts rest) -> success (String.joinWith "." parts) rest

parseConsPattern :: Array Token -> ParseResult Ast.Pattern
parseConsPattern tokens = do
  Tuple hd rest <- parseSimplePattern tokens
  Tuple _ rest' <- expectColon rest
  Tuple tl rest'' <- parsePattern rest'
  success (Ast.PatCons hd tl) rest''

parseTuplePattern :: Array Token -> ParseResult Ast.Pattern
parseTuplePattern tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "("
      then do
        Tuple elements rest <- parseSeparated parsePattern (\tok -> expectDelimiter tok ",") (Array.drop 1 ts)
        Tuple _ rest' <- expectDelimiter rest ")"
        success (Ast.PatRecord (Array.mapWithIndex (\i p -> Tuple (show i) p) elements)) rest'
      else failure "Expected tuple pattern"
    Nothing -> failure "Expected tuple pattern"

parseListPattern :: Array Token -> ParseResult Ast.Pattern
parseListPattern tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "["
      then
        case Array.head (Array.drop 1 ts) of
          Just t' ->
            if t'.tokenType == TokDelimiter && t'.value == "]"
            then success (Ast.PatList []) (Array.drop 2 ts)
            else do
              Tuple elements rest <- parseSeparated parsePattern (\tok -> expectDelimiter tok ",") (Array.drop 1 ts)
              Tuple _ rest' <- expectDelimiter rest "]"
              success (Ast.PatList elements) rest'
          Nothing -> failure "Expected list pattern"
      else failure "Expected list pattern"
    Nothing -> failure "Expected list pattern"

parseRecordPattern :: Array Token -> ParseResult Ast.Pattern
parseRecordPattern tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "{"
      then do
        Tuple fields rest <- parseSeparated parseRecordFieldPattern (\tok -> expectDelimiter tok ",") (Array.drop 1 ts)
        Tuple _ rest' <- expectDelimiter rest "}"
        success (Ast.PatRecord fields) rest'
      else failure "Expected record pattern"
    Nothing -> failure "Expected record pattern"

parseRecordFieldPattern :: Array Token -> ParseResult (Tuple String Ast.Pattern)
parseRecordFieldPattern tokens = do
  Tuple label rest <- parseLabel tokens
  case expectColon rest of
    Right (Tuple _ rest') -> do
      Tuple pat rest'' <- parsePattern rest'
      success (Tuple label pat) rest''
    Left _ -> case expectOperator rest "=" of
      Right (Tuple _ rest') -> do
        Tuple pat rest'' <- parsePattern rest'
        success (Tuple label pat) rest''
      Left _ -> success (Tuple label (Ast.PatVar label)) rest

parseParenPattern :: Array Token -> ParseResult Ast.Pattern
parseParenPattern tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "("
      then do
        Tuple pat rest <- parsePattern (Array.drop 1 ts)
        Tuple _ rest' <- expectDelimiter rest ")"
        success (Ast.PatParens pat) rest'
      else failure "Expected parenthesized pattern"
    Nothing -> failure "Expected parenthesized pattern"

parseSimplePattern :: Array Token -> ParseResult Ast.Pattern
parseSimplePattern tokens =
  parseAny
    [ parseLiteralPattern
    , parseRecordPattern  -- Added to handle { field, ... } patterns in constructor arguments
    , parseQualifiedConstructorPatternSimple  -- Must be before parseVarPattern to handle Ast.PatWildcard
    , parseVarPattern
    , parseParenPattern  -- Must come before parseTuplePattern to handle (h : t)
    , parseTuplePattern
    , parseListPattern
    ]
    tokens

-- | Parse a qualified constructor pattern WITHOUT arguments (simple pattern context)
-- | e.g., Ast.PatWildcard as a function parameter
parseQualifiedConstructorPatternSimple :: Array Token -> ParseResult Ast.Pattern
parseQualifiedConstructorPatternSimple tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokIdentifier && isCapital t.value
      then case Array.head (Array.drop 1 ts) of
        Just t2 ->
          if t2.tokenType == TokOperator && t2.value == "."
          then do
            -- We have "Identifier." - parse full qualified name
            Tuple name rest <- parseQualifiedConstructorName ts
            success (Ast.PatCon name []) rest
          else failure "Expected qualified constructor pattern"
        Nothing -> failure "Expected qualified constructor pattern"
      else failure "Expected qualified constructor pattern"
    Nothing -> failure "Expected qualified constructor pattern"
  where
    isCapital :: String -> Boolean
    isCapital s = case CU.charAt 0 s of
      Just c -> c >= 'A' && c <= 'Z'
      Nothing -> false

-- ------------------------------------------------------------
-- Expression parsing
-- ------------------------------------------------------------

parseExpression :: Array Token -> ParseResult Ast.Expr
parseExpression tokens =
  parseAny
    [ parseLetExpression
    , parseIfExpression
    , parseCaseExpression
    , parseDoBlock
    , parseLambda
    , parseDollarExpression
    ]
    tokens

parseDollarExpression :: Array Token -> ParseResult Ast.Expr
parseDollarExpression tokens = do
  Tuple left rest <- parseHashExpression tokens
  let rest' = skipNewlines rest
  case Array.head rest' of
    Just t ->
      if t.tokenType == TokOperator && t.value == "$"
      then do
        let rest'' = skipNewlines (Array.drop 1 rest')
        Tuple right rest''' <- parseDollarExpression rest''
        success (Ast.ExprApp left right) rest'''
      else success left rest
    _ -> success left rest

-- | Parse hash operator (#) - left associative reverse application
-- | x # f == f x
parseHashExpression :: Array Token -> ParseResult Ast.Expr
parseHashExpression tokens = do
  Tuple left rest <- parseLogicalExpression tokens
  parseHashExpressionRest left rest

parseHashExpressionRest :: Ast.Expr -> Array Token -> ParseResult Ast.Expr
parseHashExpressionRest left tokens =
  let tokens' = skipNewlines tokens
  in case Array.head tokens' of
    Just t ->
      if t.tokenType == TokOperator && t.value == "#"
      then do
        let rest = skipNewlines (Array.drop 1 tokens')
        Tuple right rest' <- parseLogicalExpression rest
        -- x # f becomes (f x)
        parseHashExpressionRest (Ast.ExprApp right left) rest'
      else success left tokens
    _ -> success left tokens

parseLogicalExpression :: Array Token -> ParseResult Ast.Expr
parseLogicalExpression tokens = do
  Tuple left rest <- parseConsExpression tokens
  case Array.head rest of
    Just t ->
      if t.tokenType == TokOperator && (t.value == "&&" || t.value == "||")
      then do
        Tuple right rest' <- parseLogicalExpression (Array.drop 1 rest)
        success (Ast.ExprBinOp t.value left right) rest'
      else success left rest
    _ -> success left rest

-- | Parse cons operator (:) - right associative, low precedence
parseConsExpression :: Array Token -> ParseResult Ast.Expr
parseConsExpression tokens = do
  Tuple left rest <- parseComparisonExpression tokens
  case Array.head rest of
    Just t ->
      if t.tokenType == TokOperator && t.value == ":"
      then do
        Tuple right rest' <- parseConsExpression (Array.drop 1 rest)
        success (Ast.ExprBinOp ":" left right) rest'
      else success left rest
    _ -> success left rest

parseComparisonExpression :: Array Token -> ParseResult Ast.Expr
parseComparisonExpression tokens = do
  let tokens' = skipNewlines tokens
  Tuple left rest <- parseAdditiveExpression tokens'
  let rest' = skipNewlines rest
  case Array.head rest' of
    Just t ->
      if t.tokenType == TokOperator && isComparisonOp t.value
      then do
        let rest'' = skipNewlines (Array.drop 1 rest')
        Tuple right rest''' <- parseComparisonExpression rest''
        success (Ast.ExprBinOp t.value left right) rest'''
      else success left rest
    _ -> success left rest
  where
    isComparisonOp op = op == "==" || op == "!=" || op == "/=" || op == "<" || op == "<=" || op == ">" || op == ">="

parseAdditiveExpression :: Array Token -> ParseResult Ast.Expr
parseAdditiveExpression tokens = do
  Tuple left rest <- parseMultiplicativeExpression tokens
  case Array.head rest of
    Just t ->
      if t.tokenType == TokOperator && isAdditiveOp t.value
      then do
        Tuple right rest' <- parseAdditiveExpression (Array.drop 1 rest)
        success (Ast.ExprBinOp t.value left right) rest'
      else success left rest
    _ -> success left rest
  where
    isAdditiveOp op = op == "+" || op == "-" || op == "++" || op == "<>"

parseMultiplicativeExpression :: Array Token -> ParseResult Ast.Expr
parseMultiplicativeExpression tokens = do
  Tuple left rest <- parseBacktickExpression tokens
  case Array.head rest of
    Just t ->
      if t.tokenType == TokOperator && isMultOp t.value
      then do
        Tuple right rest' <- parseMultiplicativeExpression (Array.drop 1 rest)
        success (Ast.ExprBinOp t.value left right) rest'
      else success left rest
    _ -> success left rest
  where
    isMultOp op = op == "*" || op == "/"

-- | Parse backtick infix expressions like: x `elem` ys
-- | Transforms to: elem x ys
parseBacktickExpression :: Array Token -> ParseResult Ast.Expr
parseBacktickExpression tokens = do
  Tuple left rest <- parseUnaryExpression tokens
  parseBacktickRest left rest
  where
    parseBacktickRest left rest =
      case Array.head rest of
        Just t ->
          if t.tokenType == TokOperator && t.value == "`"
          then do
            -- Parse the function name (may be qualified like Array.elem)
            Tuple fn rest' <- parseBacktickFn (Array.drop 1 rest)
            -- Expect closing backtick
            case Array.head rest' of
              Just t' ->
                if t'.tokenType == TokOperator && t'.value == "`"
                then do
                  -- Parse right argument
                  Tuple right rest'' <- parseUnaryExpression (Array.drop 1 rest')
                  -- Result is fn applied to left then right
                  let result = Ast.ExprApp (Ast.ExprApp fn left) right
                  -- Check for more backtick operators
                  parseBacktickRest result rest''
                else failure "Expected closing backtick"
              _ -> failure "Expected closing backtick"
          else success left rest
        _ -> success left rest

    -- Parse backtick function name (possibly qualified like Array.elem)
    parseBacktickFn toks =
      case Array.head toks of
        Just t ->
          if t.tokenType == TokIdentifier
          then parseBacktickQualified t.value (Array.drop 1 toks)
          else failure "Expected identifier in backtick expression"
        _ -> failure "Expected identifier in backtick expression"

    -- Build up qualified name as string, then create the appropriate expression
    parseBacktickQualified name toks =
      case Array.head toks of
        Just t ->
          if t.tokenType == TokOperator && t.value == "."
          then case Array.head (Array.drop 1 toks) of
            Just t' ->
              if t'.tokenType == TokIdentifier
              then parseBacktickQualified (name <> "." <> t'.value) (Array.drop 2 toks)
              else success (Ast.ExprVar name) toks
            _ -> success (Ast.ExprVar name) toks
          else success (Ast.ExprVar name) toks
        _ -> success (Ast.ExprVar name) toks

parseUnaryExpression :: Array Token -> ParseResult Ast.Expr
parseUnaryExpression tokens =
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokOperator && isUnaryOp t.value
      then do
        Tuple expr rest <- parseUnaryExpression (Array.drop 1 tokens)
        success (Ast.ExprUnaryOp t.value expr) rest
      else parseApplication tokens
    _ -> parseApplication tokens
  where
    isUnaryOp op = op == "-" || op == "+" || op == "!"

parseApplication :: Array Token -> ParseResult Ast.Expr
parseApplication tokens =
  case Array.head tokens of
    Just firstTok -> do
      Tuple fn rest <- parseTerm tokens
      -- Check for record update: expr { field = value }
      Tuple fn' rest' <- maybeParseRecordUpdate fn rest
      let Tuple args rest'' = collectApplicationArgs rest' [] firstTok.column
      case Array.length args of
        0 -> success fn' rest''
        _ -> success (foldApp fn' args) rest''
    Nothing -> failure "No tokens remaining"
  where
    foldApp :: Ast.Expr -> Array Ast.Expr -> Ast.Expr
    foldApp fn args = Array.foldl Ast.ExprApp fn args

-- | Parse record update syntax: expr { field = value, ... }
-- | Returns the original expression unchanged if not followed by update syntax
maybeParseRecordUpdate :: Ast.Expr -> Array Token -> ParseResult Ast.Expr
maybeParseRecordUpdate expr tokens =
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "{"
      then
        -- Check if this is a record update (uses =) or literal (uses :)
        case isRecordUpdate (Array.drop 1 tokens) of
          true -> do
            Tuple updates rest <- parseRecordUpdateFields (Array.drop 1 tokens)
            Tuple _ rest' <- expectDelimiter rest "}"
            -- Recursively check for chained updates
            maybeParseRecordUpdate (Ast.ExprRecordUpdate expr updates) rest'
          false -> success expr tokens
      else success expr tokens
    _ -> success expr tokens
  where
    -- Check if the brace content looks like record update (field =) rather than literal (field :)
    isRecordUpdate :: Array Token -> Boolean
    isRecordUpdate toks =
      case Array.head toks of
        Just t1 ->
          if t1.tokenType == TokIdentifier
          then case Array.head (Array.drop 1 toks) of
            Just t2 ->
              if t2.tokenType == TokOperator && t2.value == "="
              then true
              else false
            _ -> false
          else false
        _ -> false

parseRecordUpdateFields :: Array Token -> ParseResult (Array (Tuple String Ast.Expr))
parseRecordUpdateFields tokens =
  parseSeparated parseRecordUpdateField (\t -> expectDelimiter t ",") tokens

parseRecordUpdateField :: Array Token -> ParseResult (Tuple String Ast.Expr)
parseRecordUpdateField tokens = do
  Tuple label rest <- parseIdentifierName tokens
  Tuple _ rest' <- expectOperator rest "="
  Tuple expr rest'' <- parseExpression rest'
  success (Tuple label expr) rest''

collectApplicationArgs :: Array Token -> Array Ast.Expr -> Int -> Tuple (Array Ast.Expr) (Array Token)
collectApplicationArgs tokens acc base =
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokNewline
      then
        let rest = skipNewlines (Array.drop 1 tokens) in
        case Array.head rest of
          Just t' ->
            -- Don't continue if next looks like a binding (identifier = ...)
            if looksLikeBinding rest
            then Tuple acc rest
            -- Continue if next token is a bracket/paren (likely continuation)
            else if t'.column > 1 && isContinuationToken t'
            then case parseTerm rest of
              Right (Tuple arg rest') -> collectApplicationArgs rest' (Array.snoc acc arg) base
              Left _ -> Tuple acc rest
            -- Continue on next line if column > base (more indented)
            else if t'.column > 1 && t'.column > base
            then case parseTerm rest of
              Right (Tuple arg rest') -> collectApplicationArgs rest' (Array.snoc acc arg) base
              Left _ -> Tuple acc rest
            else Tuple acc rest
          _ -> Tuple acc rest
      else case parseTerm tokens of
        Right (Tuple arg rest) -> collectApplicationArgs rest (Array.snoc acc arg) base
        Left _ -> Tuple acc tokens
    _ -> case parseTerm tokens of
      Right (Tuple arg rest) -> collectApplicationArgs rest (Array.snoc acc arg) base
      Left _ -> Tuple acc tokens
  where
    -- Check if tokens look like a let/where binding: identifier = expression or { pattern } = expression
    looksLikeBinding :: Array Token -> Boolean
    looksLikeBinding toks =
      case Array.head toks of
        Just t1 ->
          if t1.tokenType == TokIdentifier
          then case Array.head (skipNewlines (Array.drop 1 toks)) of
            Just t2 ->
              if t2.tokenType == TokOperator && t2.value == "="
              then true
              else false
            _ -> false
          -- Also check for record pattern binding: { ... } = expression
          else if t1.tokenType == TokDelimiter && t1.value == "{"
          then looksLikeRecordBinding (Array.drop 1 toks)
          else false
        _ -> false

    -- Check if after '{' we have a record pattern followed by } and =
    looksLikeRecordBinding :: Array Token -> Boolean
    looksLikeRecordBinding toks = go toks 1
      where
        go :: Array Token -> Int -> Boolean
        go toks' depth = case Array.head toks' of
          Nothing -> false
          Just t ->
            if t.tokenType == TokDelimiter && t.value == "{"
            then go (Array.drop 1 toks') (depth + 1)
            else if t.tokenType == TokDelimiter && t.value == "}"
            then
              if depth == 1
              then
                -- Found matching }, check if followed by =
                case Array.head (skipNewlines (Array.drop 1 toks')) of
                  Just t2 ->
                    if t2.tokenType == TokOperator && t2.value == "="
                    then true
                    else false
                  _ -> false
              else go (Array.drop 1 toks') (depth - 1)
            else go (Array.drop 1 toks') depth

    -- Check if token indicates expression continuation
    isContinuationToken :: Token -> Boolean
    isContinuationToken t = t.tokenType == TokDelimiter && (t.value == "[" || t.value == "(" || t.value == "{")

parseTerm :: Array Token -> ParseResult Ast.Expr
parseTerm tokens =
  parseAny
    [ parseRecordLiteral
    , parseExprLiteral
    , parseListLiteral
    , parseTupleLiteral
    , parseQualifiedIdentifier
    , parseParenExpr
    ]
    tokens

parseExprLiteral :: Array Token -> ParseResult Ast.Expr
parseExprLiteral tokens = do
  Tuple lit rest <- parseLiteral tokens
  success (Ast.ExprLit lit) rest

parseParenExpr :: Array Token -> ParseResult Ast.Expr
parseParenExpr tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "("
      then do
        Tuple expr rest <- parseExpression (Array.drop 1 ts)
        Tuple _ rest' <- expectDelimiter rest ")"
        success (Ast.ExprParens expr) rest'
      else failure "Expected parenthesized expression"
    Nothing -> failure "Expected parenthesized expression"

parseRecordLiteral :: Array Token -> ParseResult Ast.Expr
parseRecordLiteral tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "{"
      then do
        Tuple fields rest <- parseSeparated parseRecordFieldExpr (\tok -> expectDelimiter tok ",") (Array.drop 1 ts)
        Tuple _ rest' <- expectDelimiter rest "}"
        success (Ast.ExprRecord fields) rest'
      else failure "Expected record literal"
    Nothing -> failure "Expected record literal"

parseRecordFieldExpr :: Array Token -> ParseResult (Tuple String Ast.Expr)
parseRecordFieldExpr tokens = do
  Tuple label rest <- parseIdentifierName tokens
  -- Check for colon (full syntax) or shorthand (just identifier)
  case expectColon rest of
    Right (Tuple _ rest') -> do
      let rest'' = skipNewlines rest'
      Tuple expr rest''' <- parseExpression rest''
      success (Tuple label expr) rest'''
    Left _ ->
      -- Shorthand: { x } means { x: x }
      success (Tuple label (Ast.ExprVar label)) rest

parseListLiteral :: Array Token -> ParseResult Ast.Expr
parseListLiteral tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "["
      then
        case Array.head (Array.drop 1 ts) of
          Just t' ->
            if t'.tokenType == TokDelimiter && t'.value == "]"
            then success (Ast.ExprList []) (Array.drop 2 ts)
            else do
              Tuple elements rest <- parseSeparated parseExpression (\tok -> expectDelimiter tok ",") (Array.drop 1 ts)
              Tuple _ rest' <- expectDelimiter rest "]"
              success (Ast.ExprList elements) rest'
          Nothing -> failure "Expected list literal"
      else failure "Expected list literal"
    Nothing -> failure "Expected list literal"

parseTupleLiteral :: Array Token -> ParseResult Ast.Expr
parseTupleLiteral tokens =
  let ts = skipNewlines tokens
  in case Array.head ts of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "("
      then do
        Tuple elements rest <- parseSeparated parseExpression (\tok -> expectDelimiter tok ",") (Array.drop 1 ts)
        Tuple _ rest' <- expectDelimiter rest ")"
        case Array.length elements of
          1 -> case Array.head elements of
            Just e -> success e rest'
            Nothing -> failure "Expected expression"
          _ -> success (Ast.ExprTuple elements) rest'
      else failure "Expected tuple literal"
    Nothing -> failure "Expected tuple literal"

-- ------------------------------------------------------------
-- Let expression
-- ------------------------------------------------------------

parseLetExpression :: Array Token -> ParseResult Ast.Expr
parseLetExpression tokens = do
  let tokens' = skipNewlines tokens
  Tuple _ rest <- expectKeyword tokens' "let"
  let rest' = skipNewlines rest
  Tuple bindings rest'' <- parseMany parseBinding rest'
  let rest''' = skipNewlines rest''
  Tuple _ rest4 <- expectKeyword rest''' "in"
  let rest5 = skipNewlines rest4
  Tuple body rest6 <- parseExpression rest5
  success (Ast.ExprLet bindings body) rest6

parseBinding :: Array Token -> ParseResult Ast.LetBind
parseBinding tokens = do
  let tokens' = skipNewlines tokens
  -- First try function-style binding: name params = expr
  case parseFunctionStyleBinding tokens' of
    Right r -> Right r
    Left _ -> do
      -- Fall back to simple pattern binding: pat = expr
      Tuple pat rest <- parsePattern tokens'
      Tuple _ rest' <- expectOperator rest "="
      Tuple expr rest'' <- parseExpression rest'
      success { pattern: pat, value: expr, typeAnn: Nothing } rest''

-- | Parse function-style let binding: name param1 param2 = body
-- Note: Constructor patterns like "Tuple x y" should NOT be parsed as function bindings
parseFunctionStyleBinding :: Array Token -> ParseResult Ast.LetBind
parseFunctionStyleBinding tokens =
  -- Must start with a lowercase identifier (function name)
  -- Uppercase identifiers are constructors and should be pattern bindings
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokIdentifier && not (isUpperCase t.value)
      then
        let name = t.value
            rest = Array.drop 1 tokens
            Tuple params rest' = collectParams rest []
        in -- If we found at least one parameter, it's a function binding
            case Array.length params of
              0 -> failure "Not a function binding"
              _ -> case expectOperator rest' "=" of
                Left err -> Left err
                Right (Tuple _ rest'') -> case parseExpression rest'' of
                  Left err -> Left err
                  Right (Tuple body rest''') ->
                    -- Convert to: name = \params -> body
                    let lambda = Ast.ExprLambda params body
                    in success { pattern: Ast.PatVar name, value: lambda, typeAnn: Nothing } rest'''
      else failure "Not a function binding"
    _ -> failure "Not a function binding"
  where
    isUpperCase s = case CU.charAt 0 s of
      Just c -> c >= 'A' && c <= 'Z'
      Nothing -> false

    -- Collect parameters until we hit = sign (returns params in order and remaining tokens)
    collectParams :: Array Token -> Array Ast.Pattern -> Tuple (Array Ast.Pattern) (Array Token)
    collectParams toks acc =
      let toks' = skipNewlines toks
      in case Array.head toks' of
        Just tok ->
          -- Stop at = sign
          if tok.tokenType == TokOperator && tok.value == "="
          then Tuple (Array.reverse acc) toks'
          -- Try to parse a simple pattern (var, wildcard, literal, or parens)
          else case parseSimplePattern toks' of
            Right (Tuple pat rest) -> collectParams rest (Array.cons pat acc)
            Left _ -> Tuple (Array.reverse acc) toks'
        -- Try to parse a simple pattern (var, wildcard, literal, or parens)
        _ -> case parseSimplePattern toks' of
          Right (Tuple pat rest) -> collectParams rest (Array.cons pat acc)
          Left _ -> Tuple (Array.reverse acc) toks'

-- ------------------------------------------------------------
-- If expression
-- ------------------------------------------------------------

parseIfExpression :: Array Token -> ParseResult Ast.Expr
parseIfExpression tokens = do
  let tokens' = skipNewlines tokens
  Tuple _ rest <- expectKeyword tokens' "if"
  Tuple cond rest' <- parseExpression rest
  Tuple _ rest'' <- expectKeyword rest' "then"
  Tuple thenBranch rest''' <- parseExpression rest''
  Tuple _ rest4 <- expectKeyword rest''' "else"
  Tuple elseBranch rest5 <- parseExpression rest4
  success (Ast.ExprIf cond thenBranch elseBranch) rest5

-- ------------------------------------------------------------
-- Case expression
-- ------------------------------------------------------------

parseCaseExpression :: Array Token -> ParseResult Ast.Expr
parseCaseExpression tokens = do
  Tuple _ rest <- expectKeyword tokens "case"
  Tuple expr rest' <- parseExpression rest
  let rest'' = skipNewlines rest'
  Tuple _ rest''' <- expectKeyword rest'' "of"
  -- Determine the expected clause indent from the first clause
  let rest4 = skipNewlines rest'''
  case Array.head rest4 of
    Nothing -> failure "Expected case clauses"
    Just firstTok -> do
      Tuple clauses rest5 <- parseCaseClausesAt rest4 firstTok.column []
      success (Ast.ExprCase expr clauses) rest5

-- | Parse case clauses at a specific indentation level
parseCaseClausesAt :: Array Token -> Int -> Array Ast.CaseClause -> ParseResult (Array Ast.CaseClause)
parseCaseClausesAt tokens indent acc =
  let tokens' = skipNewlines tokens in
  case Array.head tokens' of
    Nothing ->
      if Array.length acc > 0
      then success acc tokens'
      else failure "Expected case clause"
    Just t ->
      -- Check if this is an additional guard for the previous clause (starts with |)
      if t.tokenType == TokOperator && t.value == "|" && Array.length acc > 0
      then
        -- Get the pattern from the previous clause and parse this as a continuation
        case Array.last acc of
          Just prevClause -> parseAdditionalGuard tokens' prevClause.pattern indent acc
          Nothing -> failure "Internal error: no previous clause"
      else if t.column /= indent && Array.length acc > 0
      then success acc tokens'
      else if t.column /= indent
      then failure "Case clause at wrong indentation"
      else case parseCaseClause tokens' of
        Right (Tuple clause rest) ->
          -- Check if remaining tokens start with | (additional guards)
          let rest' = skipNewlines rest in
          case Array.head rest' of
            Just t2 ->
              if t2.tokenType == TokOperator && t2.value == "|"
              then parseAdditionalGuard rest' clause.pattern indent (Array.snoc acc clause)
              else parseCaseClausesAt rest indent (Array.snoc acc clause)
            _ -> parseCaseClausesAt rest indent (Array.snoc acc clause)
        Left _ ->
          if Array.length acc > 0
          then success acc tokens'
          else failure "Expected case clause"
  where
    -- Parse additional guards for the same pattern: | guard -> body
    parseAdditionalGuard :: Array Token -> Ast.Pattern -> Int -> Array Ast.CaseClause -> ParseResult (Array Ast.CaseClause)
    parseAdditionalGuard toks pat clauseIndent clauseAcc =
      let toks' = skipNewlines toks in
      case Array.head toks' of
        Just t ->
          if t.tokenType == TokOperator && t.value == "|"
          then do
            case parseGuardExpression (Array.drop 1 toks') of
              Right (Tuple guard afterGuard) -> do
                Tuple _ afterArrow <- expectOperator afterGuard "->"
                -- Determine the body extent based on the clause indent
                case Array.head (skipNewlines afterArrow) of
                  Just firstBodyTok -> do
                    let Tuple bodyTokens rest = takeBody afterArrow [] firstBodyTok.column
                    Tuple body remaining <- parseExpression bodyTokens
                    let clause = { pattern: pat, guard: Just guard, body: body }
                    let newAcc = Array.snoc clauseAcc clause
                    -- Check for more guards in `rest` (from takeBody), not `remaining` (from parseExpression)
                    -- The additional guards are in `rest`, since bodyTokens only contains the body expression
                    let restAfterBody = skipNewlines rest
                    case Array.head restAfterBody of
                      Just t' ->
                        if t'.tokenType == TokOperator && t'.value == "|"
                        then parseAdditionalGuard restAfterBody pat clauseIndent newAcc
                        else parseCaseClausesAt rest clauseIndent newAcc
                      _ -> parseCaseClausesAt rest clauseIndent newAcc
                  Nothing -> failure "Expected body after ->"
              Left err -> Left err
          else parseCaseClausesAt toks clauseIndent clauseAcc
        _ -> parseCaseClausesAt toks clauseIndent clauseAcc

-- | Parse one case clause. If there are multiple guarded alternatives (e.g., | guard1 -> body1 | guard2 -> body2),
-- | only parse the first one. The caller should handle additional guards.
parseCaseClause :: Array Token -> ParseResult Ast.CaseClause
parseCaseClause tokens = do
  let tokens' = skipNewlines tokens
  case Array.head tokens' of
    Nothing -> failure "No more tokens to parse"
    Just firstTok -> do
      Tuple pat rest <- parsePattern tokens'
      let Tuple guard rest' = maybeParseGuard rest
      Tuple _ rest'' <- expectOperator rest' "->"
      -- Use the pattern column for takeBody
      -- This allows nested case expressions to have clauses at lower indentation than the body
      -- Additional guards are detected by checking if remaining tokens after parseExpression start with |
      let Tuple bodyTokens rest''' = takeBody rest'' [] firstTok.column
      Tuple body remaining <- parseExpression bodyTokens
      case skipNewlines remaining of
        [] -> success { pattern: pat, guard: guard, body: body } (dropNewlines rest''')
        -- Check if remaining tokens are additional guards for the same pattern
        _ -> case hasMoreGuards remaining of
          -- Concatenate remaining guards with rest''' (tokens after the body)
          true -> success { pattern: pat, guard: guard, body: body } (remaining <> rest''')
          false -> failure "Unexpected tokens after case-clause body"
  where
    -- Check if remaining tokens look like another guard (| guard -> ...)
    hasMoreGuards :: Array Token -> Boolean
    hasMoreGuards toks =
      case Array.head (skipNewlines toks) of
        Just t ->
          if t.tokenType == TokOperator && t.value == "|"
          then true
          else false
        _ -> false

maybeParseGuard :: Array Token -> Tuple (Maybe Ast.Expr) (Array Token)
maybeParseGuard tokens =
  let tokens' = skipNewlines tokens in
  case Array.head tokens' of
    Just t ->
      if t.tokenType == TokOperator && t.value == "|"
      then case parseGuardExpression (Array.drop 1 tokens') of
        Right (Tuple guard rest) -> Tuple (Just guard) rest
        Left _ -> Tuple Nothing tokens
      else Tuple Nothing tokens
    _ -> Tuple Nothing tokens

-- | Parse guard expressions which can include pattern binds (pat <- expr) and boolean guards
-- | Multiple guards are separated by commas
parseGuardExpression :: Array Token -> ParseResult Ast.Expr
parseGuardExpression tokens = do
  Tuple guards rest <- parseGuardParts tokens []
  case Array.length guards of
    0 -> failure "Expected guard expression"
    1 -> case Array.head guards of
      Just g -> success g rest
      Nothing -> failure "No guard"
    _ -> success (foldGuards guards) rest
  where
    foldGuards :: Array Ast.Expr -> Ast.Expr
    foldGuards gs = case Array.uncons gs of
      Just { head, tail } -> Array.foldl (\acc g -> Ast.ExprBinOp "&&" acc g) head tail
      Nothing -> Ast.ExprLit (Ast.LitBool true)

parseGuardParts :: Array Token -> Array Ast.Expr -> ParseResult (Array Ast.Expr)
parseGuardParts tokens acc = do
  Tuple g rest <- parseGuardPart tokens
  let rest' = skipNewlines rest
  case Array.head rest' of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == ","
      then parseGuardParts (Array.drop 1 rest') (Array.snoc acc g)
      else success (Array.snoc acc g) rest'
    _ -> success (Array.snoc acc g) rest'

parseGuardPart :: Array Token -> ParseResult Ast.Expr
parseGuardPart tokens = do
  let tokens' = skipNewlines tokens
  -- Check for pattern bind: pat <- expr
  -- This requires looking ahead for <-
  case tryPatternBind tokens' of
    Right result -> Right result
    Left _ -> parseLogicalExpression tokens'  -- Use logical to handle && and ||
  where
    tryPatternBind toks = do
      -- Try to parse as pattern, then look for <-
      Tuple pat rest <- parsePattern toks
      let rest' = skipNewlines rest
      case Array.head rest' of
        Just t ->
          if t.tokenType == TokOperator && t.value == "<-"
          then do
            Tuple expr rest'' <- parseLogicalExpression (Array.drop 1 rest')  -- Use logical here too
            -- Convert pattern bind to expression for AST compatibility
            -- This is a simplification - ideally AST would have a GuardBind constructor
            success (Ast.ExprBinOp "<-" (patternToExpr pat) expr) rest''
          else failure "Not a pattern bind"
        _ -> failure "Not a pattern bind"

    patternToExpr :: Ast.Pattern -> Ast.Expr
    patternToExpr (Ast.PatVar v) = Ast.ExprVar v
    patternToExpr (Ast.PatCon c args) = foldl Ast.ExprApp (Ast.ExprVar c) (map patternToExpr args)
    patternToExpr (Ast.PatLit l) = Ast.ExprLit l
    patternToExpr Ast.PatWildcard = Ast.ExprVar "_"
    patternToExpr (Ast.PatRecord fields) = Ast.ExprRecord (map (\(Tuple k p) -> Tuple k (patternToExpr p)) fields)
    patternToExpr (Ast.PatCons h t) = Ast.ExprBinOp ":" (patternToExpr h) (patternToExpr t)
    patternToExpr (Ast.PatAs n p) = patternToExpr p  -- Just use inner pattern
    patternToExpr (Ast.PatList ps) = Ast.ExprList (map patternToExpr ps)
    patternToExpr (Ast.PatParens p) = Ast.ExprParens (patternToExpr p)

takeBody :: Array Token -> Array Token -> Int -> Tuple (Array Token) (Array Token)
takeBody tokens acc indent =
  case Array.head tokens of
    Nothing -> Tuple (Array.reverse acc) []
    Just t ->
      if t.tokenType == TokNewline
      then
        let rest = skipNewlines (Array.drop 1 tokens) in
        case Array.head rest of
          Just t' ->
            if t'.column < indent
            then Tuple (Array.reverse acc) rest
            else if t'.column == indent && clauseStart rest
            then Tuple (Array.reverse acc) rest
            -- Check for additional guard (| guard -> ...) - stop before it
            else if t'.tokenType == TokOperator && t'.value == "|" && guardStart rest
            then Tuple (Array.reverse acc) rest
            -- Continue collecting body tokens on next line (indented continuation)
            -- Include a newline token so nested case expressions can parse correctly
            else takeBody rest (Array.cons t acc) indent
          _ -> takeBody rest (Array.cons t acc) indent
      else takeBody (Array.drop 1 tokens) (Array.cons t acc) indent

-- | Check if tokens start with a guard: | expr ->
guardStart :: Array Token -> Boolean
guardStart tokens =
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokOperator && t.value == "|"
      then case parseGuardExpression (Array.drop 1 tokens) of
        Right (Tuple _ rest) ->
          case expectOperator rest "->" of
            Right _ -> true
            Left _ -> false
        Left _ -> false
      else false
    _ -> false

clauseStart :: Array Token -> Boolean
clauseStart tokens =
  case parsePattern tokens of
    Right (Tuple _ rest) ->
      case Array.head tokens of
        Just patTok ->
          -- Check for guard with indentation check
          let Tuple _ rest' = maybeParseGuardIndented patTok.column rest in
          case expectOperator rest' "->" of
            Right _ -> true
            Left _ -> false
        Nothing -> false
    Left _ -> false
  where
    -- Parse guard, but if we need to skip newlines, only accept guards that are
    -- MORE indented than the pattern (indicating continuation, not a new body line)
    maybeParseGuardIndented :: Int -> Array Token -> Tuple (Maybe Ast.Expr) (Array Token)
    maybeParseGuardIndented patCol toks =
      case Array.head toks of
        Just t ->
          -- Guard on same line (no newline)
          if t.tokenType == TokOperator && t.value == "|"
          then case parseGuardExpression (Array.drop 1 toks) of
            Right (Tuple guard rest) -> Tuple (Just guard) rest
            Left _ -> Tuple Nothing toks
          -- Newline - check if next line has a guard that's MORE indented
          else if t.tokenType == TokNewline
          then
            let toks' = skipNewlines toks in
            case Array.head toks' of
              Just t' ->
                if t'.tokenType == TokOperator && t'.value == "|" && t'.column > patCol
                then case parseGuardExpression (Array.drop 1 toks') of
                  Right (Tuple guard rest) -> Tuple (Just guard) rest
                  Left _ -> Tuple Nothing toks
                else Tuple Nothing toks
              _ -> Tuple Nothing toks
          else Tuple Nothing toks
        _ -> Tuple Nothing toks

-- ------------------------------------------------------------
-- Do block
-- ------------------------------------------------------------

parseDoBlock :: Array Token -> ParseResult Ast.Expr
parseDoBlock tokens = do
  Tuple _ rest <- expectKeyword tokens "do"
  -- Determine the expected indent from the first statement
  let rest' = skipNewlines rest
  case Array.head rest' of
    Nothing -> success (Ast.ExprDo []) rest'
    Just firstTok -> do
      let indent = firstTok.column
      Tuple stmts rest'' <- parseDoStatementsAt rest' indent []
      success (Ast.ExprDo stmts) rest''

-- | Parse do statements at a specific indentation level
parseDoStatementsAt :: Array Token -> Int -> Array Ast.DoStatement -> ParseResult (Array Ast.DoStatement)
parseDoStatementsAt tokens indent acc =
  let tokens' = skipNewlines tokens in
  case Array.head tokens' of
    Nothing -> success acc tokens'
    Just t ->
      if t.column < indent
      then success acc tokens'  -- Less indented = end of do block
      else if t.column > indent && Array.length acc == 0
      then
        -- First statement can be more indented (continuation)
        case parseDoStatement tokens' of
          Right (Tuple stmt rest) -> parseDoStatementsAt rest indent (Array.snoc acc stmt)
          Left _ -> success acc tokens'
      else if t.column /= indent && Array.length acc > 0
      then success acc tokens'  -- Different indent = end
      else case parseDoStatement tokens' of
        Right (Tuple stmt rest) -> parseDoStatementsAt rest indent (Array.snoc acc stmt)
        Left _ ->
          if Array.length acc > 0
          then success acc tokens'
          else failure "Expected do statement"

parseDoStatement :: Array Token -> ParseResult Ast.DoStatement
parseDoStatement tokens =
  parseAny
    [ parseDoLet
    , parseDoBind
    , parseDoExpr
    ]
    tokens

parseDoLet :: Array Token -> ParseResult Ast.DoStatement
parseDoLet tokens = do
  Tuple _ rest <- expectKeyword tokens "let"
  Tuple bindings rest' <- parseMany parseBinding rest
  success (Ast.DoLet bindings) rest'

parseDoBind :: Array Token -> ParseResult Ast.DoStatement
parseDoBind tokens = do
  Tuple pat rest <- parsePattern tokens
  Tuple _ rest' <- expectOperator rest "<-"
  Tuple expr rest'' <- parseExpression rest'
  success (Ast.DoBind pat expr) rest''

parseDoExpr :: Array Token -> ParseResult Ast.DoStatement
parseDoExpr tokens = do
  Tuple expr rest <- parseExpression tokens
  success (Ast.DoExpr expr) rest

-- ------------------------------------------------------------
-- Lambda
-- ------------------------------------------------------------

parseLambda :: Array Token -> ParseResult Ast.Expr
parseLambda tokens = do
  Tuple _ rest <- expectOperator tokens "\\"
  Tuple params rest' <- parseMany parseSimplePattern rest
  Tuple _ rest'' <- expectOperator rest' "->"
  Tuple body rest''' <- parseExpression rest''
  success (Ast.ExprLambda params body) rest'''

-- ------------------------------------------------------------
-- Type signature
-- ------------------------------------------------------------

parseTypeSignature :: Array Token -> ParseResult Ast.TypeSignature
parseTypeSignature tokens = do
  let tokens' = dropNewlines tokens
  Tuple name rest <- parseIdentifierName tokens'
  Tuple _ rest' <- expectOperator rest "::"
  Tuple ty rest'' <- parseType rest'
  success { name: name, typeVars: [], constraints: [], ty: ty } rest''

-- ------------------------------------------------------------
-- Declaration parsing
-- ------------------------------------------------------------

parseDeclaration :: Array Token -> ParseResult Ast.Declaration
parseDeclaration tokens =
  parseAny
    [ parseModuleHeader
    , parseImport
    , parseForeignImportSimple
    , parseDataDeclaration
    , parseTypeAlias
    , parseTypeClass
    , parseTypeClassInstance
    , parseFunctionWithTypeSignature
    , parseFunctionDeclaration
    , parseTypeSignatureDecl
    ]
    tokens

parseModuleHeader :: Array Token -> ParseResult Ast.Declaration
parseModuleHeader tokens = do
  let tokens' = dropNewlines tokens
  Tuple _ rest <- expectKeyword tokens' "module"
  Tuple name rest' <- parseQualifiedIdentifierName rest
  Tuple _ rest'' <- expectKeyword rest' "where"
  success (Ast.DeclModule { name: name, declarations: [] }) rest''

parseQualifiedIdentifierName :: Array Token -> ParseResult String
parseQualifiedIdentifierName tokens =
  case parseSeparated parseIdentifierName (\t -> expectOperator t ".") tokens of
    Left err -> Left err
    Right (Tuple parts rest) -> success (String.joinWith "." parts) rest

parseImport :: Array Token -> ParseResult Ast.Declaration
parseImport tokens = do
  let tokens' = dropNewlines tokens
  Tuple _ rest <- expectKeyword tokens' "import"
  Tuple modName rest' <- parseQualifiedIdentifierName rest
  let Tuple alias rest'' = parseImportAlias rest'
  Tuple result rest''' <- parseImportSelectors rest''
  let Tuple items hiding = result
  success (Ast.DeclImport { moduleName: modName, alias: alias, items: items, hiding: hiding }) (dropNewlines rest''')

parseImportAlias :: Array Token -> Tuple (Maybe String) (Array Token)
parseImportAlias tokens =
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokIdentifier && t.value == "as"
      then case parseIdentifierName (Array.drop 1 tokens) of
        Right (Tuple name rest) -> Tuple (Just name) rest
        Left _ -> Tuple Nothing tokens
      else Tuple Nothing tokens
    _ -> Tuple Nothing tokens

parseImportSelectors :: Array Token -> ParseResult (Tuple (Array Ast.ImportItem) Boolean)
parseImportSelectors tokens =
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokIdentifier && t.value == "hiding"
      then case parseParenImportList (Array.drop 1 tokens) of
        Right (Tuple items rest) -> success (Tuple items true) rest
        Left err -> Left err
      else case parseParenImportList tokens of
        Right (Tuple items rest) -> success (Tuple items false) rest
        Left _ -> success (Tuple [] false) tokens
    _ -> case parseParenImportList tokens of
      Right (Tuple items rest) -> success (Tuple items false) rest
      Left _ -> success (Tuple [] false) tokens

parseParenImportList :: Array Token -> ParseResult (Array Ast.ImportItem)
parseParenImportList tokens =
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "("
      then do
        Tuple items rest <- parseSeparated parseImportItem (\tok -> expectDelimiter tok ",") (Array.drop 1 tokens)
        Tuple _ rest' <- expectDelimiter rest ")"
        success items rest'
      else failure "No paren import list"
    _ -> failure "No paren import list"

parseImportItem :: Array Token -> ParseResult Ast.ImportItem
parseImportItem tokens =
  -- Try parsing an operator in parens like (:) or (<>)
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "("
      then case Array.head (Array.drop 1 tokens) of
        Just opTok ->
          if opTok.tokenType == TokOperator
          then case Array.head (Array.drop 2 tokens) of
            Just closeTok ->
              if closeTok.tokenType == TokDelimiter && closeTok.value == ")"
              then success (Ast.ImportValue ("(" <> opTok.value <> ")")) (Array.drop 3 tokens)
              else parseNormalImportItem tokens
            _ -> parseNormalImportItem tokens
          else parseNormalImportItem tokens
        _ -> parseNormalImportItem tokens
      else parseNormalImportItem tokens
    _ -> parseNormalImportItem tokens
  where
    parseNormalImportItem toks = do
      Tuple name rest <- parseIdentifierName toks
      case Array.head rest of
        Just t ->
          if t.tokenType == TokDelimiter && t.value == "("
          then do
            Tuple spec rest' <- parseImportSpec (Array.drop 1 rest)
            success (Ast.ImportType name spec) rest'
          else success (Ast.ImportValue name) rest
        _ -> success (Ast.ImportValue name) rest

parseImportSpec :: Array Token -> ParseResult Ast.ImportSpec
parseImportSpec tokens =
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokOperator && t.value == ".."
      then do
        Tuple _ rest <- expectDelimiter (Array.drop 1 tokens) ")"
        success Ast.ImportAll rest
      else do
        Tuple names rest <- parseSeparated parseIdentifierName (\tok -> expectDelimiter tok ",") tokens
        Tuple _ rest' <- expectDelimiter rest ")"
        success (Ast.ImportSome names) rest'
    _ -> do
      Tuple names rest <- parseSeparated parseIdentifierName (\tok -> expectDelimiter tok ",") tokens
      Tuple _ rest' <- expectDelimiter rest ")"
      success (Ast.ImportSome names) rest'

parseForeignImportSimple :: Array Token -> ParseResult Ast.Declaration
parseForeignImportSimple tokens = do
  Tuple _ rest <- expectKeyword tokens "foreign"
  Tuple _ rest' <- expectKeyword rest "import"
  Tuple name rest'' <- parseIdentifierName rest'
  Tuple _ rest''' <- expectOperator rest'' "::"
  Tuple ty rest4 <- parseType rest'''
  success (Ast.DeclForeignImport
    { moduleName: ""
    , functionName: name
    , alias: Just name
    , typeSignature: ty
    }) (dropNewlines rest4)

parseDataDeclaration :: Array Token -> ParseResult Ast.Declaration
parseDataDeclaration tokens = do
  Tuple _ rest <- expectKeyword tokens "data"
  Tuple name rest' <- parseIdentifierName rest
  Tuple vars rest'' <- parseMany parseIdentifierName rest'
  let rest''' = skipNewlines rest''
  Tuple _ rest4 <- expectOperator rest''' "="
  let rest5 = skipNewlines rest4
  Tuple ctors rest6 <- parseDataConstructors rest5
  success (Ast.DeclDataType { name: name, typeVars: vars, constructors: ctors }) rest6

parseDataConstructors :: Array Token -> ParseResult (Array Ast.DataConstructor)
parseDataConstructors tokens =
  parseSeparated parseDataConstructor (\t -> expectOperator t "|") tokens

parseDataConstructor :: Array Token -> ParseResult Ast.DataConstructor
parseDataConstructor tokens = do
  Tuple name rest <- parseIdentifierName tokens
  let rest' = skipNewlines rest
  case Array.head rest' of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "{"
      then do
        Tuple fields rest'' <- parseBracedRecordFields rest'
        success { name: name, fields: map fieldToDataField fields, isRecord: true } rest''
      else do
        Tuple fieldTypes rest'' <- parseMany parseTypeAtom rest'
        success { name: name, fields: map typeToDataField fieldTypes, isRecord: false } rest''
    _ -> do
      Tuple fieldTypes rest'' <- parseMany parseTypeAtom rest'
      success { name: name, fields: map typeToDataField fieldTypes, isRecord: false } rest''
  where
    fieldToDataField :: { label :: String, ty :: Ast.TypeExpr } -> Ast.DataField
    fieldToDataField f = { label: f.label, ty: f.ty }

    typeToDataField :: Ast.TypeExpr -> Ast.DataField
    typeToDataField ty = { label: "", ty: ty }

parseBracedRecordFields :: Array Token -> ParseResult (Array { label :: String, ty :: Ast.TypeExpr })
parseBracedRecordFields tokens =
  case Array.head tokens of
    Just t ->
      if t.tokenType == TokDelimiter && t.value == "{"
      then do
        Tuple fields rest <- parseSeparated parseRecordConstructorField (\tok -> expectDelimiter tok ",") (Array.drop 1 tokens)
        Tuple _ rest' <- expectDelimiter rest "}"
        success fields rest'
      else failure "Expected '{' for record constructor"
    _ -> failure "Expected '{' for record constructor"

parseRecordConstructorField :: Array Token -> ParseResult { label :: String, ty :: Ast.TypeExpr }
parseRecordConstructorField tokens = do
  Tuple label rest <- parseIdentifierName tokens
  Tuple _ rest' <- expectOperator rest "::"
  let rest'' = skipNewlines rest'
  Tuple ty rest''' <- parseType rest''
  success { label: label, ty: ty } rest'''

parseTypeAlias :: Array Token -> ParseResult Ast.Declaration
parseTypeAlias tokens = do
  Tuple _ rest <- expectKeyword tokens "type"
  Tuple name rest' <- parseIdentifierName rest
  Tuple vars rest'' <- parseMany parseIdentifierName rest'
  Tuple _ rest''' <- expectOperator rest'' "="
  let rest4 = skipNewlines rest'''
  Tuple aliased rest5 <- parseType rest4
  success (Ast.DeclTypeAlias { name: name, typeVars: vars, ty: aliased }) rest5

parseTypeClass :: Array Token -> ParseResult Ast.Declaration
parseTypeClass tokens = do
  Tuple _ rest <- expectKeyword tokens "class"
  let Tuple rest' _ = skipSuperclassConstraints rest
  Tuple name rest'' <- parseIdentifierName rest'
  let Tuple rest''' kind = maybeParseClassKind rest''
  Tuple vars rest4 <- parseMany parseIdentifierName rest'''
  let rest5 = skipNewlines rest4
  case Array.head rest5 of
    Just t ->
      if t.tokenType == TokKeyword && t.value == "where"
      then do
        Tuple methods rest6 <- parseMany parseTypeSignature (Array.drop 1 rest5)
        success (Ast.DeclTypeClass { name: name, typeVars: vars, methods: methods, kind: kind }) rest6
      else success (Ast.DeclTypeClass { name: name, typeVars: vars, methods: [], kind: kind }) rest5
    _ -> success (Ast.DeclTypeClass { name: name, typeVars: vars, methods: [], kind: kind }) rest5

skipSuperclassConstraints :: Array Token -> Tuple (Array Token) (Array Token)
skipSuperclassConstraints tokens =
  let tokens' = skipNewlines tokens
      { init: before, rest: after } = Array.span (\t -> not (t.tokenType == TokOperator && t.value == "<=")) tokens'
  in case Array.head after of
    Just t ->
      if t.tokenType == TokOperator && t.value == "<="
      then Tuple (Array.drop 1 after) before
      else Tuple tokens' []
    _ -> Tuple tokens' []

maybeParseClassKind :: Array Token -> Tuple (Array Token) (Maybe String)
maybeParseClassKind tokens =
  case expectOperator tokens "::" of
    Right (Tuple _ rest) -> case parseType rest of
      Right (Tuple _ rest') -> Tuple rest' Nothing  -- TODO: store kind
      Left _ -> Tuple tokens Nothing
    Left _ -> Tuple tokens Nothing

parseTypeClassInstance :: Array Token -> ParseResult Ast.Declaration
parseTypeClassInstance tokens = do
  let Tuple tokens' derived = case Array.head tokens of
        Just t ->
          if t.tokenType == TokKeyword && t.value == "derive"
          then Tuple (Array.drop 1 tokens) true
          else Tuple tokens false
        _ -> Tuple tokens false
  Tuple _ rest <- expectKeyword tokens' "instance"
  let rest' = dropNewlines rest
  -- Try named instance
  case Array.head rest' of
    Just t ->
      if t.tokenType == TokIdentifier
      then case Array.head (Array.drop 1 rest') of
        Just t' ->
          if t'.tokenType == TokOperator && t'.value == "::"
          then do
            let rest'' = dropInstanceConstraints (Array.drop 2 rest')
            Tuple ty rest''' <- parseType rest''
            -- For derive instance, 'where' is optional (no methods)
            case expectKeyword rest''' "where" of
              Right (Tuple _ rest4) -> do
                Tuple methods rest5 <- parseMany parseFunctionDeclarationRaw rest4
                let className = extractClassName ty
                success (Ast.DeclTypeClassInstance
                  { className: className
                  , ty: ty
                  , methods: methods
                  , derived: derived
                  }) rest5
              Left _ ->
                if derived then do
                  let className = extractClassName ty
                  success (Ast.DeclTypeClassInstance
                    { className: className
                    , ty: ty
                    , methods: []
                    , derived: derived
                    }) rest'''
                else
                  failure "Expected 'where' clause for non-derived instance"
          else parseUnnamedInstance rest' derived
        _ -> parseUnnamedInstance rest' derived
      else parseUnnamedInstance rest' derived
    _ -> parseUnnamedInstance rest' derived
  where
    parseUnnamedInstance rest isDerived = do
      let rest' = dropInstanceConstraints rest
      Tuple className rest'' <- parseIdentifierName rest'
      Tuple ty rest''' <- parseType rest''
      -- For derive instance, 'where' is optional (no methods)
      case expectKeyword rest''' "where" of
        Right (Tuple _ rest4) -> do
          Tuple methods rest5 <- parseMany parseFunctionDeclarationRaw rest4
          success (Ast.DeclTypeClassInstance
            { className: className
            , ty: ty
            , methods: methods
            , derived: isDerived
            }) rest5
        Left _ ->
          if isDerived then
            success (Ast.DeclTypeClassInstance
              { className: className
              , ty: ty
              , methods: []
              , derived: isDerived
              }) rest'''
          else
            failure "Expected 'where' clause for non-derived instance"

    extractClassName :: Ast.TypeExpr -> String
    extractClassName (Ast.TyExprCon name) = name
    extractClassName (Ast.TyExprApp fn _) = extractClassName fn
    extractClassName _ = "Unknown"

dropInstanceConstraints :: Array Token -> Array Token
dropInstanceConstraints tokens =
  -- Look for => which separates constraints from the instance head
  -- e.g., instance (Show a, Eq a) => MyClass a where ...
  -- We only look at the beginning of the token stream, not deep into the file
  let tokens' = skipNewlines tokens
      -- Check if we have constraints by looking for =>
      { init: before, rest: after } = Array.span (\t -> not (t.tokenType == TokOperator && t.value == "=>")) tokens'
  in case Array.head after of
    -- Only skip if we actually found => at a reasonable position (within first ~20 tokens)
    Just t ->
      if t.tokenType == TokOperator && t.value == "=>" && Array.length before < 20
      then skipNewlines (Array.drop 1 after)
      else tokens'
    _ -> tokens'

parseFunctionWithTypeSignature :: Array Token -> ParseResult Ast.Declaration
parseFunctionWithTypeSignature tokens = do
  let tokens' = dropNewlines tokens
  case Array.head tokens' of
    Just t ->
      if t.tokenType == TokIdentifier
      then do
        let name = t.value
        case expectOperator (Array.drop 1 tokens') "::" of
          Right (Tuple _ rest) -> do
            let Tuple typeTokens rest' = splitTypeAndRest rest name
            Tuple ty _ <- parseType (stripNewlines typeTokens)
            Tuple fun rest'' <- parseFunctionDeclarationRaw rest'
            if fun.name == name then
              success (Ast.DeclFunction
                { name: fun.name
                , parameters: fun.parameters
                , body: fun.body
                , guards: fun.guards
                , typeSignature: Just { name: name, typeVars: [], constraints: [], ty: ty }
                }) rest''
            else
              failure "Function name mismatch"
          Left _ -> failure "Expected '::'"
      else failure "Expected identifier"
    _ -> failure "Expected identifier"

-- | Split tokens into type signature tokens and the rest (starting from function definition)
-- | We look for the function name at column 1 (start of line) to find the function definition,
-- | not just any occurrence of the name (which might be inside the type or inside other expressions)
splitTypeAndRest :: Array Token -> String -> Tuple (Array Token) (Array Token)
splitTypeAndRest tokens name = go tokens []
  where
    go :: Array Token -> Array Token -> Tuple (Array Token) (Array Token)
    go toks acc = case Array.head toks of
      Nothing -> Tuple acc toks
      Just t ->
        -- Found the function name at column 1 (start of line definition)
        if t.tokenType == TokIdentifier && t.value == name && t.column == 1
        then Tuple acc toks
        -- Skip newlines but don't include them in type tokens
        else if t.tokenType == TokNewline
        then go (Array.drop 1 toks) acc
        -- Accumulate other tokens as part of the type
        else go (Array.drop 1 toks) (Array.snoc acc t)

parseFunctionDeclaration :: Array Token -> ParseResult Ast.Declaration
parseFunctionDeclaration tokens = do
  Tuple fun rest <- parseFunctionDeclarationRaw tokens
  success (Ast.DeclFunction fun) rest

parseFunctionDeclarationRaw :: Array Token -> ParseResult Ast.FunctionDeclaration
parseFunctionDeclarationRaw tokens = do
  Tuple name rest <- parseIdentifierName tokens
  Tuple params rest' <- parseMany parseSimplePattern rest
  -- Check if next is = (simple) or | (guards)
  let rest'' = skipNewlines rest'
  case Array.head rest'' of
    Just tok ->
      if tok.tokenType == TokOperator && tok.value == "|"
      then
        -- Has guards
        parseGuardedFunction name params rest''
      else do
        -- Simple function with =
        Tuple _ rest''' <- expectOperator rest' "="
        case Array.head rest''' of
          Just firstTok -> do
            Tuple body rest4 <- parseExpression rest'''
            Tuple body' rest5 <- maybeParseWhere rest4 firstTok.column body
            success { name: name, parameters: params, body: body', guards: [], typeSignature: Nothing } rest5
          Nothing -> failure "Expected expression"
    _ -> do
      -- Simple function with =
      Tuple _ rest''' <- expectOperator rest' "="
      case Array.head rest''' of
        Just firstTok -> do
          Tuple body rest4 <- parseExpression rest'''
          Tuple body' rest5 <- maybeParseWhere rest4 firstTok.column body
          success { name: name, parameters: params, body: body', guards: [], typeSignature: Nothing } rest5
        Nothing -> failure "Expected expression"

-- | Parse a function with guards
parseGuardedFunction :: String -> Array Ast.Pattern -> Array Token -> ParseResult Ast.FunctionDeclaration
parseGuardedFunction name params tokens = do
  Tuple guards rest <- parseGuardedExprs tokens
  -- The body will be a placeholder since we use guards
  -- We use ExprVar "undefined" as a sentinel - CodeGen should check guards first
  let body = Ast.ExprVar "__guarded__"
  success { name: name, parameters: params, body: body, guards: guards, typeSignature: Nothing } rest

-- | Parse multiple guarded expressions
parseGuardedExprs :: Array Token -> ParseResult (Array Ast.GuardedExpr)
parseGuardedExprs tokens = parseGuardedExprsAcc tokens []
  where
    parseGuardedExprsAcc :: Array Token -> Array Ast.GuardedExpr -> ParseResult (Array Ast.GuardedExpr)
    parseGuardedExprsAcc toks acc =
      let toks' = skipNewlines toks
      in case Array.head toks' of
        Just tok ->
          if tok.tokenType == TokOperator && tok.value == "|"
          then do
            Tuple guard rest <- parseOneGuardedExpr (Array.drop 1 toks')
            parseGuardedExprsAcc rest (Array.snoc acc guard)
          else success acc toks'
        _ -> success acc toks'

-- | Parse one guarded expression: guard-clause(, guard-clause)* = expr
parseOneGuardedExpr :: Array Token -> ParseResult Ast.GuardedExpr
parseOneGuardedExpr tokens = do
  Tuple clauses rest <- parseGuardClauses tokens []
  Tuple _ rest' <- expectOperator rest "="
  Tuple body rest'' <- parseExpression rest'
  success { guards: clauses, body: body } rest''

-- | Parse guard clauses separated by commas
parseGuardClauses :: Array Token -> Array Ast.GuardClause -> ParseResult (Array Ast.GuardClause)
parseGuardClauses tokens acc = do
  Tuple clause rest <- parseOneGuardClause tokens
  let rest' = skipNewlines rest
  case Array.head rest' of
    Just tok ->
      if tok.tokenType == TokDelimiter && tok.value == ","
      then parseGuardClauses (Array.drop 1 rest') (Array.snoc acc clause)
      else success (Array.snoc acc clause) rest'
    _ -> success (Array.snoc acc clause) rest'

-- | Parse a single guard clause: either `Pat <- expr` or just `expr`
parseOneGuardClause :: Array Token -> ParseResult Ast.GuardClause
parseOneGuardClause tokens =
  -- Try pattern guard first: Pat <- expr
  case tryPatternGuard tokens of
    Right result -> Right result
    Left _ ->
      -- Fall back to expression guard
      case parseFuncGuardExpr tokens of
        Right (Tuple expr rest) -> success (Ast.GuardExpr expr) rest
        Left err -> Left err

-- | Try to parse a pattern guard: Pat <- expr
tryPatternGuard :: Array Token -> ParseResult Ast.GuardClause
tryPatternGuard tokens = do
  -- Use full parsePattern to handle constructor patterns like `TyVar v'`
  Tuple pat rest <- parsePattern tokens
  let rest' = skipNewlines rest
  case Array.head rest' of
    Just tok ->
      if tok.tokenType == TokOperator && tok.value == "<-"
      then do
        Tuple expr rest'' <- parseFuncGuardExpr (Array.drop 1 rest')
        success (Ast.GuardPat pat expr) rest''
      else failure "Expected <- for pattern guard"
    _ -> failure "Expected <- for pattern guard"

-- | Parse guard expression for function guards (stops at = or ,)
parseFuncGuardExpr :: Array Token -> ParseResult Ast.Expr
parseFuncGuardExpr tokens = parseGuardExprOr tokens

parseGuardExprOr :: Array Token -> ParseResult Ast.Expr
parseGuardExprOr tokens = do
  Tuple left rest <- parseGuardExprAnd tokens
  let rest' = skipNewlines rest
  case Array.head rest' of
    Just tok ->
      if tok.tokenType == TokOperator && tok.value == "||"
      then do
        Tuple right rest'' <- parseGuardExprOr (Array.drop 1 rest')
        success (Ast.ExprBinOp "||" left right) rest''
      else success left rest'
    _ -> success left rest'

parseGuardExprAnd :: Array Token -> ParseResult Ast.Expr
parseGuardExprAnd tokens = do
  Tuple left rest <- parseGuardExprComparison tokens
  let rest' = skipNewlines rest
  case Array.head rest' of
    Just tok ->
      if tok.tokenType == TokOperator && tok.value == "&&"
      then do
        Tuple right rest'' <- parseGuardExprAnd (Array.drop 1 rest')
        success (Ast.ExprBinOp "&&" left right) rest''
      else success left rest'
    _ -> success left rest'

parseGuardExprComparison :: Array Token -> ParseResult Ast.Expr
parseGuardExprComparison tokens = do
  Tuple left rest <- parseGuardExprApp tokens
  let rest' = skipNewlines rest
  case Array.head rest' of
    Just tok ->
      if tok.tokenType == TokOperator && isComparisonOp tok.value
      then do
        Tuple right rest'' <- parseGuardExprApp (Array.drop 1 rest')
        success (Ast.ExprBinOp tok.value left right) rest''
      else success left rest'
    _ -> success left rest'
  where
    isComparisonOp op = Array.elem op ["==", "/=", "<", ">", "<=", ">="]

parseGuardExprApp :: Array Token -> ParseResult Ast.Expr
parseGuardExprApp tokens = do
  Tuple first rest <- parseGuardExprAtom tokens
  parseGuardExprAppRest first rest

parseGuardExprAppRest :: Ast.Expr -> Array Token -> ParseResult Ast.Expr
parseGuardExprAppRest func tokens =
  let tokens' = skipNewlines tokens
  in case Array.head tokens' of
    -- Stop at guard terminators
    Just tok ->
      if tok.tokenType == TokOperator && Array.elem tok.value ["=", ",", "|", "||", "&&", "==", "/=", "<", ">", "<=", ">="]
      then success func tokens'
      else if tok.tokenType == TokNewline
        then success func tokens'
        else case parseGuardExprAtom tokens' of
          Right (Tuple arg rest) -> parseGuardExprAppRest (Ast.ExprApp func arg) rest
          Left _ -> success func tokens'
    -- Try to parse another atom as argument
    _ -> case parseGuardExprAtom tokens' of
      Right (Tuple arg rest) -> parseGuardExprAppRest (Ast.ExprApp func arg) rest
      Left _ -> success func tokens'

parseGuardExprAtom :: Array Token -> ParseResult Ast.Expr
parseGuardExprAtom tokens =
  let tokens' = skipNewlines tokens
  in case Array.head tokens' of
    Just tok ->
      if tok.tokenType == TokIdentifier
      then
        -- Check for record field access: r.x.y
        let rest = Array.drop 1 tokens'
            baseExpr = Ast.ExprVar tok.value
        in parseRecordAccessChain baseExpr rest
      else if tok.tokenType == TokNumber
      then case Int.fromString tok.value of
        Just n -> success (Ast.ExprLit (Ast.LitInt n)) (Array.drop 1 tokens')
        Nothing -> failure ("Invalid integer: " <> tok.value)
      else if tok.tokenType == TokString
      then success (Ast.ExprLit (Ast.LitString tok.value)) (Array.drop 1 tokens')
      else if tok.tokenType == TokKeyword && tok.value == "true"
      then success (Ast.ExprLit (Ast.LitBool true)) (Array.drop 1 tokens')
      else if tok.tokenType == TokKeyword && tok.value == "false"
      then success (Ast.ExprLit (Ast.LitBool false)) (Array.drop 1 tokens')
      else if tok.tokenType == TokKeyword && tok.value == "otherwise"
      then success (Ast.ExprLit (Ast.LitBool true)) (Array.drop 1 tokens')
      else if tok.tokenType == TokDelimiter && tok.value == "("
      then do
        Tuple expr rest <- parseExpression (Array.drop 1 tokens')
        Tuple _ rest' <- expectDelimiter rest ")"
        success expr rest'
      else if tok.tokenType == TokOperator && tok.value == "."
      then
        -- Record accessor like .id
        case Array.head (Array.drop 1 tokens') of
          Just fld ->
            if fld.tokenType == TokIdentifier
            then success (Ast.ExprSection ("." <> fld.value)) (Array.drop 2 tokens')
            else failure "Expected field name after ."
          _ -> failure "Expected field name after ."
      else failure "Expected guard expression atom"
    _ -> failure "Expected guard expression atom"

-- | Parse record field access chain: .field1.field2...
parseRecordAccessChain :: Ast.Expr -> Array Token -> ParseResult Ast.Expr
parseRecordAccessChain expr tokens =
  case Array.head tokens of
    Just tok ->
      if tok.tokenType == TokOperator && tok.value == "."
      then case Array.head (Array.drop 1 tokens) of
        Just fld ->
          if fld.tokenType == TokIdentifier
          then parseRecordAccessChain (Ast.ExprRecordAccess expr fld.value) (Array.drop 2 tokens)
          else success expr tokens  -- No field name, stop here
        _ -> success expr tokens  -- No field name, stop here
      else success expr tokens  -- No dot, stop here
    _ -> success expr tokens  -- No dot, stop here

maybeParseWhere :: Array Token -> Int -> Ast.Expr -> ParseResult Ast.Expr
maybeParseWhere tokens _ body = do
  let tokens' = skipNewlines tokens
  case Array.head tokens' of
    Just t ->
      if t.tokenType == TokKeyword && t.value == "where"
      then
        let whereCol = t.column
            rest = skipNewlines (Array.drop 1 tokens')
        in case Array.head rest of
          Just firstTok ->
            if firstTok.column > whereCol
            then do
              Tuple bindings rest' <- collectWhereBindings rest whereCol []
              success (Ast.ExprLet bindings body) rest'
            else success body tokens'
          _ -> success body tokens'
      else success body tokens
    _ -> success body tokens

collectWhereBindings :: Array Token -> Int -> Array Ast.LetBind -> ParseResult (Array Ast.LetBind)
collectWhereBindings tokens whereCol acc = do
  let tokens' = skipNewlines tokens
  case Array.head tokens' of
    Just t ->
      if t.column > whereCol
      then do
        -- Check if this is a type signature (name ::) or a function definition (name params =)
        case isTypeSignatureLine tokens' of
          true -> do
            -- Skip the type signature line and continue with the actual binding
            let rest = skipToNextLine tokens'
            collectWhereBindings rest whereCol acc
          false -> do
            -- Where bindings are local function definitions
            Tuple fun rest <- parseFunctionDeclarationRaw tokens'
            let binding = { pattern: Ast.PatVar fun.name, value: wrapLambda fun.parameters fun.body, typeAnn: Nothing }
            collectWhereBindings rest whereCol (Array.snoc acc binding)
      else success acc tokens'
    _ -> success acc tokens'
  where
    wrapLambda :: Array Ast.Pattern -> Ast.Expr -> Ast.Expr
    wrapLambda params body = case Array.length params of
      0 -> body
      _ -> Ast.ExprLambda params body

    -- Check if the token stream starts with "name ::" (a type signature)
    isTypeSignatureLine :: Array Token -> Boolean
    isTypeSignatureLine toks =
      case Array.head toks of
        Just t1 ->
          if t1.tokenType == TokIdentifier
          then case Array.head (Array.drop 1 toks) of
            Just t2 ->
              if t2.tokenType == TokOperator && t2.value == "::"
              then true
              else false
            _ -> false
          else false
        _ -> false

    -- Skip tokens until we hit a newline at column 1 or a newline followed by content at whereCol level
    skipToNextLine :: Array Token -> Array Token
    skipToNextLine toks =
      case Array.head toks of
        Nothing -> toks
        Just t ->
          if t.tokenType == TokNewline
          then
            let rest = Array.drop 1 toks
            in case Array.head rest of
              Just t' ->
                if t'.column <= whereCol
                then toks  -- Stop before the newline - outdented content
                else if t'.tokenType == TokNewline
                then skipToNextLine rest  -- Skip blank line
                else rest  -- Continue with indented content
              _ -> rest  -- Continue with indented content
          else skipToNextLine (Array.drop 1 toks)

parseTypeSignatureDecl :: Array Token -> ParseResult Ast.Declaration
parseTypeSignatureDecl tokens = do
  Tuple sig rest <- parseTypeSignature tokens
  success (Ast.DeclTypeSig sig) rest

-- ------------------------------------------------------------
-- Module parsing
-- ------------------------------------------------------------

parseDeclarations :: Array Token -> ParseResult (Array Ast.Declaration)
parseDeclarations tokens = parseDeclarationsAcc tokens []

parseDeclarationsAcc :: Array Token -> Array Ast.Declaration -> ParseResult (Array Ast.Declaration)
parseDeclarationsAcc tokens acc = do
  let tokens' = skipNewlines tokens
  case Array.head tokens' of
    Nothing -> success acc []
    _ -> case parseDeclaration tokens' of
      Right (Tuple decl rest) -> parseDeclarationsAcc rest (Array.snoc acc decl)
      Left _ ->
        if Array.length acc > 0
        then success acc tokens'
        else failure "Failed to parse declaration"

parseModule :: Array Token -> ParseResult Ast.Module
parseModule tokens = do
  Tuple header rest <- parseModuleHeader tokens
  case header of
    Ast.DeclModule m -> do
      Tuple decls rest' <- parseDeclarations rest
      let rest'' = skipNewlines rest'
      case Array.length rest'' of
        0 -> success { name: m.name, declarations: decls } []
        _ -> failure "Unexpected tokens after module"
    _ -> failure "Expected module declaration"
