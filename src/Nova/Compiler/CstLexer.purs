-- | Lexer for CST parsing with layout token insertion
-- | Based on purescript-language-cst-parser
module Nova.Compiler.CstLexer
  ( lexModule
  , lexTokens
  ) where

import Prelude
import Data.Array as Array
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Int as Int
import Data.Number as Number
import Data.List (List(..), (:))
import Nova.Compiler.Cst as Cst
import Nova.Compiler.CstLayout as CstLayout
import Nova.Compiler.CstLayout (LayoutDelim(..), LayoutStack)

-- ============================================================================
-- Lexer State (for raw tokenization)
-- ============================================================================

type LexState =
  { chars :: List Char  -- Use List for O(1) pattern matching on BEAM
  , line :: Int
  , column :: Int
  }

initLexState :: String -> LexState
initLexState input =
  { chars: arrayToList (CU.toCharArray input)
  , line: 1
  , column: 1
  }

-- | Convert Array to List (for BEAM efficiency)
arrayToList :: forall a. Array a -> List a
arrayToList arr = Array.foldr (:) Nil arr

-- | Convert char list to string (reverse first since we build backwards)
charsToString :: List Char -> String
charsToString chars = CU.fromCharArray (listToArray (reverseList chars))

-- | Reverse a list
reverseList :: forall a. List a -> List a
reverseList = go Nil
  where
  go acc Nil = acc
  go acc (x : xs) = go (x : acc) xs

-- | Convert List to Array
listToArray :: forall a. List a -> Array a
listToArray lst = Array.fromFoldable lst

-- | All keywords
keywords :: Array String
keywords =
  [ "foreign", "module", "where", "import", "data", "type"
  , "class", "instance", "let", "in", "if", "then", "else"
  , "case", "of", "do", "ado", "derive", "newtype", "infixl", "infixr", "infix"
  , "forall", "as", "hiding", "true", "false", "role"
  , "nominal", "representational", "phantom"
  ]

-- ============================================================================
-- Layout Token Insertion (must be defined before lexModule)
-- ============================================================================

-- | Check if a layout delimiter requires indentation tracking
isIndented :: LayoutDelim -> Boolean
isIndented lyt = case lyt of
  LytLet -> true
  LytLetStmt -> true
  LytWhere -> true
  LytOf -> true
  LytDo -> true
  LytAdo -> true
  _ -> false

-- | Create a layout token at a position
lytToken :: Cst.SourcePos -> Cst.Token -> Cst.SourceToken
lytToken pos value =
  { range: { start: pos, end: pos }
  , leadingComments: Nil
  , trailingComments: Nil
  , value
  }

-- | Close remaining layout blocks at EOF - helper for insertLayoutTokens
closeLayouts :: LayoutStack -> List Cst.SourceToken
closeLayouts stack = closeLayoutsGo Nil stack

closeLayoutsGo :: List Cst.SourceToken -> LayoutStack -> List Cst.SourceToken
closeLayoutsGo acc Nil = acc
closeLayoutsGo acc (Tuple pos lyt : rest) =
  if isIndented lyt
  then
    let endTok = lytToken pos (Cst.TokLayoutEnd pos.column)
    in closeLayoutsGo (acc <> (endTok : Nil)) rest
  else closeLayoutsGo acc rest

-- | Local wrapper for insertLayout to work around code gen import issues
callInsertLayout :: Cst.SourceToken -> Cst.SourcePos -> LayoutStack -> Tuple LayoutStack (List (Tuple Cst.SourceToken LayoutStack))
callInsertLayout tok pos stk = CstLayout.insertLayout tok pos stk

-- | Helper for insertLayoutTokens - recursive layout insertion
insertLayoutGo :: LayoutStack -> List Cst.SourceToken -> List Cst.SourceToken -> List Cst.SourceToken
insertLayoutGo stack toks acc = case toks of
  Nil ->
    -- At EOF, close all open layout blocks
    acc <> closeLayouts stack
  (tok : rest) ->
    -- Get next token position for layout start determination
    let nextPos = case rest of
          (next : _) -> next.range.start
          Nil -> { line: tok.range.end.line + 1, column: 1 }
    in
      -- Insert layout based on this token
      case callInsertLayout tok nextPos stack of
        Tuple newStack outputTokens ->
          -- Extract just the tokens from the output
          let newToks = map fst outputTokens
          in insertLayoutGo newStack rest (acc <> newToks)

-- | Insert layout tokens into a raw token stream
insertLayoutTokens :: List Cst.SourceToken -> List Cst.SourceToken
insertLayoutTokens tokens =
  let initPos = { line: 1, column: 1 }
      -- Use rootLayoutDelim helper to get LytRoot (avoids code gen issues with imported constructors)
      initStack = Tuple initPos (getRootLayout {}) : Nil
  in insertLayoutGo initStack tokens Nil
  where
  getRootLayout _ = CstLayout.rootLayoutDelim

-- ============================================================================
-- Main Lexer Entry Points
-- ============================================================================

-- | Tokenize source with layout tokens inserted
lexModule :: String -> List Cst.SourceToken
lexModule source =
  let rawTokens = lexTokens source
  in insertLayoutTokens rawTokens

-- | Tokenize source WITHOUT layout tokens (raw tokens only)
lexTokens :: String -> List Cst.SourceToken
lexTokens source = lexTokensGo (initLexState source) Nil

-- Helper for lexTokens
lexTokensGo :: LexState -> List Cst.SourceToken -> List Cst.SourceToken
lexTokensGo state acc =
  case lexToken state of
    Nothing -> acc
    Just (Tuple tok state') -> lexTokensGo state' (acc <> (tok : Nil))

-- ============================================================================
-- Raw Token Lexing
-- ============================================================================

lexToken :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexToken state = case state.chars of
  Nil -> Nothing

  -- Skip whitespace
  (' ' : _) -> lexToken (advance state 1)
  ('\t' : _) -> lexToken (advanceTab state)

  -- Newlines - just skip them, layout handles the semantics
  ('\n' : _) -> lexToken (advanceNewline (advance state 1))
  ('\r' : '\n' : _) -> lexToken (advanceNewline (advance state 2))
  ('\r' : _) -> lexToken (advanceNewline (advance state 1))

  -- Line comments: --
  ('-' : '-' : _) ->
    let state' = skipLineComment (advance state 2)
    in lexToken state'

  -- Block comments: {-
  ('{' : '-' : _) ->
    let state' = skipBlockComment (advance state 2) 1
    in lexToken state'

  -- Raw string: """
  ('"' : '"' : '"' : _) -> lexRawString state

  -- String literal: "
  ('"' : _) -> lexString state

  -- Char literal: '
  ('\'' : _) -> lexChar state

  -- Delimiters (single char)
  ('(' : _) -> lexDelimiter state
  (')' : _) -> lexDelimiter state
  ('[' : _) -> lexDelimiter state
  (']' : _) -> lexDelimiter state
  ('{' : _) -> lexDelimiter state
  ('}' : _) -> lexDelimiter state
  (',' : _) -> lexDelimiter state
  ('`' : _) -> lexDelimiter state
  (';' : _) -> lexDelimiter state

  -- At symbol
  ('@' : _) -> Just (Tuple (makeToken Cst.TokAt state) (advance state 1))

  -- Catch-all: check character classes
  (c : _) ->
    if isDigit c then lexNumber state
    else if c == '_' then lexUnderscore state
    else if isIdentStart c then lexIdentOrKeyword state
    else if isUpper c then lexUpperIdentifier state
    else if isOperatorChar c then lexOperator state
    else lexToken (advance state 1)  -- Unknown character - skip it

-- ============================================================================
-- Character Classification
-- ============================================================================

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

isUpper :: Char -> Boolean
isUpper c = c >= 'A' && c <= 'Z'

isLower :: Char -> Boolean
isLower c = c >= 'a' && c <= 'z'

isAlpha :: Char -> Boolean
isAlpha c = isUpper c || isLower c

isAlphaNum :: Char -> Boolean
isAlphaNum c = isAlpha c || isDigit c

isIdentStart :: Char -> Boolean
isIdentStart c = isLower c || (c == '_')

isIdentChar :: Char -> Boolean
isIdentChar c = isAlphaNum c || (c == '_') || (c == '\'')

isOperatorChar :: Char -> Boolean
isOperatorChar c =
  c == ':' || c == '!' || c == '#' || c == '$' || c == '%' ||
  c == '&' || c == '*' || c == '+' || c == '.' || c == '/' ||
  c == '<' || c == '=' || c == '>' || c == '?' || c == '\\' ||
  c == '^' || c == '|' || c == '-' || c == '~'

-- ============================================================================
-- Position Helpers
-- ============================================================================

-- | Drop n characters from the front of the char list
dropChars :: Int -> List Char -> List Char
dropChars 0 xs = xs
dropChars _ Nil = Nil
dropChars n (_ : xs) = dropChars (n - 1) xs

-- | Advance state by n characters
advance :: LexState -> Int -> LexState
advance state n =
  state { chars = dropChars n state.chars, column = state.column + n }

-- | Advance to next line
advanceNewline :: LexState -> LexState
advanceNewline state =
  state { line = state.line + 1, column = 1 }

-- | Advance past a tab (drop 1 char, advance column to next tab stop)
advanceTab :: LexState -> LexState
advanceTab state =
  let col = state.column
      nextTab = ((col - 1) / 8 + 1) * 8 + 1
  in state { chars = dropChars 1 state.chars, column = nextTab }

-- ============================================================================
-- Token Construction
-- ============================================================================

makeToken :: Cst.Token -> LexState -> Cst.SourceToken
makeToken tok state =
  { range: { start: { line: state.line, column: state.column }
           , end: { line: state.line, column: state.column } }
  , leadingComments: Nil
  , trailingComments: Nil
  , value: tok
  }

makeTokenRange :: Cst.Token -> LexState -> LexState -> Cst.SourceToken
makeTokenRange tok startState endState =
  { range: { start: { line: startState.line, column: startState.column }
           , end: { line: endState.line, column: endState.column } }
  , leadingComments: Nil
  , trailingComments: Nil
  , value: tok
  }

-- ============================================================================
-- Individual Token Lexers
-- ============================================================================

lexIdentOrKeyword :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexIdentOrKeyword state =
  let startState = state
      Tuple name state' = consumeIdent state Nil
      tok = if Array.elem name keywords
            then makeKeywordToken name startState state'
            else makeIdentToken name startState state'
  in Just (Tuple tok state')

makeKeywordToken :: String -> LexState -> LexState -> Cst.SourceToken
makeKeywordToken name startState endState =
  let token = case name of
        "forall" -> Cst.TokForall
        _ -> Cst.TokLowerName Nothing name
  in makeTokenRange token startState endState

makeIdentToken :: String -> LexState -> LexState -> Cst.SourceToken
makeIdentToken name startState endState =
  makeTokenRange (Cst.TokLowerName Nothing name) startState endState

lexUpperIdentifier :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexUpperIdentifier state =
  let startState = state
      Tuple name state' = consumeUpperIdent state Nil
      -- Check for qualified name (handles chains like Nova.Compiler.Ast)
      result = case state'.chars of
        ('.' : _) -> checkQualifiedChain name (advance state' 1)
        _ -> Tuple (Cst.TokUpperName Nothing name) state'
      Tuple tok finalState = result
  in Just (Tuple (makeTokenRange tok startState finalState) finalState)

-- Handle chains like Data.Maybe.fromMaybe
checkQualifiedChain :: String -> LexState -> Tuple Cst.Token LexState
checkQualifiedChain prefix state = case state.chars of
  (c : _) | isUpper c ->
    let Tuple name state' = consumeUpperIdent state Nil
        fullPrefix = prefix <> "." <> name
    in case state'.chars of
      ('.' : _) -> checkQualifiedChain fullPrefix (advance state' 1)
      _ -> Tuple (Cst.TokUpperName (Just prefix) name) state'
  (c : _) | isLower c ->
    let Tuple name state' = consumeIdent state Nil
    in Tuple (Cst.TokLowerName (Just prefix) name) state'
  (c : _) | isOperatorChar c ->
    let Tuple op state' = consumeOperator state Nil
    in Tuple (Cst.TokOperator (Just prefix) op) state'
  _ ->
    -- Backtrack - the dot is separate
    Tuple (Cst.TokUpperName Nothing prefix) state

consumeIdent :: LexState -> List Char -> Tuple String LexState
consumeIdent state acc = case state.chars of
  (c : _) | isIdentChar c ->
    consumeIdent (advance state 1) (c : acc)
  _ -> Tuple (charsToString acc) state

consumeUpperIdent :: LexState -> List Char -> Tuple String LexState
consumeUpperIdent state acc = case state.chars of
  (c : _) | isAlphaNum c || (c == '_') ->
    consumeUpperIdent (advance state 1) (c : acc)
  _ -> Tuple (charsToString acc) state

lexOperator :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexOperator state =
  let startState = state
      Tuple op state' = consumeOperator state Nil
      tok = case op of
        "::" -> Cst.TokDoubleColon
        "=" -> Cst.TokEquals
        "|" -> Cst.TokPipe
        "." -> Cst.TokDot
        "\\" -> Cst.TokBackslash
        "->" -> Cst.TokRightArrow
        "<-" -> Cst.TokLeftArrow
        "=>" -> Cst.TokRightFatArrow
        "@" -> Cst.TokAt
        _ -> Cst.TokOperator Nothing op
  in Just (Tuple (makeTokenRange tok startState state') state')

consumeOperator :: LexState -> List Char -> Tuple String LexState
consumeOperator state acc = case state.chars of
  (c : _) | isOperatorChar c ->
    consumeOperator (advance state 1) (c : acc)
  _ -> Tuple (charsToString acc) state

lexDelimiter :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexDelimiter state = case state.chars of
  (c : _) ->
    let tok = case c of
          '(' -> Cst.TokLeftParen
          ')' -> Cst.TokRightParen
          '[' -> Cst.TokLeftSquare
          ']' -> Cst.TokRightSquare
          '{' -> Cst.TokLeftBrace
          '}' -> Cst.TokRightBrace
          ',' -> Cst.TokComma
          '`' -> Cst.TokTick
          ';' -> Cst.TokComma  -- Treat ; like comma for now
          _ -> Cst.TokComma  -- Shouldn't happen
    in Just (Tuple (makeToken tok state) (advance state 1))
  Nil -> Nothing

lexUnderscore :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexUnderscore state =
  let startState = state
      state' = advance state 1
  in case state'.chars of
    (c : _) | isIdentChar c ->
      -- It's a hole like _foo
      let Tuple name state'' = consumeIdent state' Nil
      in Just (Tuple (makeTokenRange (Cst.TokHole name) startState state'') state'')
    _ ->
      -- It's just underscore (wildcard)
      Just (Tuple (makeToken Cst.TokUnderscore state) state')

lexNumber :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexNumber state =
  let startState = state
  in case consumeNumber state Nil of
    Tuple intPart afterInt ->
      -- Check for decimal point followed by digit
      case afterInt.chars of
        ('.' : c : _) | isDigit c ->
          let afterDot = advance afterInt 1
          in case consumeNumber afterDot Nil of
            Tuple fracPart afterFrac ->
              let numStr = intPart <> "." <> fracPart
              in case Number.fromString numStr of
                Just n -> Just (Tuple (makeTokenRange (Cst.TokNumber numStr n) startState afterFrac) afterFrac)
                Nothing -> makeIntResult intPart startState afterInt
        _ -> makeIntResult intPart startState afterInt
  where
  makeIntResult intPart startState endState =
    let intVal = Cst.SmallInt (fromMaybe 0 (Int.fromString intPart))
    in Just (Tuple (makeTokenRange (Cst.TokInt intPart intVal) startState endState) endState)

consumeNumber :: LexState -> List Char -> Tuple String LexState
consumeNumber state acc = case state.chars of
  (c : _) | isDigit c ->
    consumeNumber (advance state 1) (c : acc)
  ('_' : _) ->
    consumeNumber (advance state 1) acc  -- Skip underscores in numbers
  _ -> Tuple (charsToString acc) state

lexString :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexString state =
  let startState = state
      afterOpen = advance state 1  -- Skip opening quote
  in case consumeString afterOpen Nil of
    Tuple content afterContent ->
      let afterClose = advance afterContent 1  -- Skip closing quote
      in Just (Tuple (makeTokenRange (Cst.TokString content content) startState afterClose) afterClose)

consumeString :: LexState -> List Char -> Tuple String LexState
consumeString state acc = case state.chars of
  Nil -> Tuple (charsToString acc) state
  ('"' : _) -> Tuple (charsToString acc) state
  ('\\' : 'n' : _) -> consumeString (advance state 2) ('\n' : acc)
  ('\\' : 't' : _) -> consumeString (advance state 2) ('\t' : acc)
  ('\\' : 'r' : _) -> consumeString (advance state 2) ('\r' : acc)
  ('\\' : '"' : _) -> consumeString (advance state 2) ('"' : acc)
  ('\\' : '\\' : _) -> consumeString (advance state 2) ('\\' : acc)
  ('\\' : c : _) -> consumeString (advance state 2) (c : acc)
  ('\n' : _) -> Tuple (charsToString acc) state  -- Unclosed string
  (c : _) -> consumeString (advance state 1) (c : acc)

lexRawString :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexRawString state =
  let startState = state
      afterOpen = advance state 3  -- Skip """
  in case consumeRawString afterOpen Nil of
    Tuple content afterContent ->
      let afterClose = advance afterContent 3  -- Skip closing """
      in Just (Tuple (makeTokenRange (Cst.TokRawString content) startState afterClose) afterClose)

consumeRawString :: LexState -> List Char -> Tuple String LexState
consumeRawString state acc = case state.chars of
  Nil -> Tuple (charsToString acc) state
  ('"' : '"' : '"' : _) -> Tuple (charsToString acc) state  -- Found closing """
  ('"' : _) -> consumeRawString (advance state 1) ('"' : acc)
  ('\n' : _) ->
    let state' = advanceNewline (advance state 1)
    in consumeRawString state' ('\n' : acc)
  (c : _) -> consumeRawString (advance state 1) (c : acc)

lexChar :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexChar state =
  let startState = state
      afterOpen = advance state 1  -- Skip opening quote
  in case parseCharContent afterOpen of
    Tuple ch afterChar ->
      let afterClose = advance afterChar 1  -- Skip closing quote
      in Just (Tuple (makeTokenRange (Cst.TokChar (CU.singleton ch) ch) startState afterClose) afterClose)
  where
  parseCharContent s = case s.chars of
    ('\\' : 'n' : _) -> Tuple '\n' (advance s 2)
    ('\\' : 't' : _) -> Tuple '\t' (advance s 2)
    ('\\' : 'r' : _) -> Tuple '\r' (advance s 2)
    ('\\' : '\'' : _) -> Tuple '\'' (advance s 2)
    ('\\' : '\\' : _) -> Tuple '\\' (advance s 2)
    ('\\' : c : _) -> Tuple c (advance s 2)
    (c : _) -> Tuple c (advance s 1)
    Nil -> Tuple '?' s

-- ============================================================================
-- Comment Handling
-- ============================================================================

skipLineComment :: LexState -> LexState
skipLineComment state = case state.chars of
  Nil -> state
  ('\n' : _) -> state  -- Don't consume the newline here, let main loop handle it
  ('\r' : _) -> state
  (_ : _) -> skipLineComment (advance state 1)

skipBlockComment :: LexState -> Int -> LexState
skipBlockComment state 0 = state
skipBlockComment state depth = case state.chars of
  Nil -> state
  ('{' : '-' : _) -> skipBlockComment (advance state 2) (depth + 1)
  ('-' : '}' : _) -> skipBlockComment (advance state 2) (depth - 1)
  ('{' : _) -> skipBlockComment (advance state 1) depth
  ('-' : _) -> skipBlockComment (advance state 1) depth
  ('\n' : _) -> skipBlockComment (advanceNewline (advance state 1)) depth
  (_ : _) -> skipBlockComment (advance state 1) depth
