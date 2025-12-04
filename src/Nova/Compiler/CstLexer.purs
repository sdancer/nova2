module Nova.Compiler.CstLexer where

import Prelude
import Data.Array as Array
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Int as Int
import Data.Number as Number
import Nova.Compiler.Cst as Cst

-- ============================================================================
-- Layout Stack for Indentation Tracking
-- ============================================================================

-- | Layout context types
data LayoutContext
  = LytRoot           -- Top level
  | LytWhere Int      -- where block at column
  | LytOf Int         -- case..of at column
  | LytDo Int         -- do block at column
  | LytAdo Int        -- ado block at column
  | LytLet Int        -- let..in at column
  | LytBrace          -- explicit braces {}
  | LytParen          -- parentheses ()
  | LytSquare         -- square brackets []

type LayoutStack = Array LayoutContext

-- | Get the column from a layout context
layoutColumn :: LayoutContext -> Maybe Int
layoutColumn LytRoot = Nothing
layoutColumn (LytWhere col) = Just col
layoutColumn (LytOf col) = Just col
layoutColumn (LytDo col) = Just col
layoutColumn (LytAdo col) = Just col
layoutColumn (LytLet col) = Just col
layoutColumn LytBrace = Nothing
layoutColumn LytParen = Nothing
layoutColumn LytSquare = Nothing

-- ============================================================================
-- Lexer State
-- ============================================================================

type LexState =
  { input :: String
  , chars :: Array Char
  , pos :: Int
  , line :: Int
  , column :: Int
  , layoutStack :: LayoutStack
  , pendingTokens :: Array Cst.SourceToken  -- Tokens to emit before continuing
  }

initLexState :: String -> LexState
initLexState input =
  { input
  , chars: CU.toCharArray input
  , pos: 0
  , line: 1
  , column: 1
  , layoutStack: [LytRoot]
  , pendingTokens: []
  }

-- | Keywords that start layout blocks
layoutKeywords :: Array String
layoutKeywords = ["where", "of", "do", "ado", "let"]

-- | All keywords
keywords :: Array String
keywords =
  [ "foreign", "module", "where", "import", "data", "type"
  , "class", "instance", "let", "in", "if", "then", "else"
  , "case", "of", "do", "ado", "derive", "newtype", "infixl", "infixr", "infix"
  , "forall", "as", "hiding"
  ]

-- ============================================================================
-- Main Lexer Entry Point
-- ============================================================================

-- | Tokenize source with layout tokens inserted
lexModule :: String -> Array Cst.SourceToken
lexModule source = go (initLexState source) []
  where
    go :: LexState -> Array Cst.SourceToken -> Array Cst.SourceToken
    go state acc =
      -- First emit any pending tokens
      case Array.uncons state.pendingTokens of
        Just { head: tok, tail: rest } ->
          go (state { pendingTokens = rest }) (Array.snoc acc tok)
        Nothing ->
          case lexToken state of
            Nothing ->
              -- At EOF, close all open layout blocks
              let closeToks = closeAllLayouts state
              in acc <> closeToks
            Just (Tuple tok state') ->
              go state' (Array.snoc acc tok)

-- | Close all remaining layout blocks at EOF
closeAllLayouts :: LexState -> Array Cst.SourceToken
closeAllLayouts state =
  Array.mapMaybe makeLayoutEnd state.layoutStack
  where
    makeLayoutEnd ctx = case layoutColumn ctx of
      Just col -> Just (makeToken Cst.TokLayoutEnd col state)
      Nothing -> Nothing

    makeToken :: (Int -> Cst.Token) -> Int -> LexState -> Cst.SourceToken
    makeToken mkTok col st =
      { range: { start: { line: st.line, column: st.column }
               , end: { line: st.line, column: st.column } }
      , leadingComments: []
      , trailingComments: []
      , value: mkTok col
      }

-- ============================================================================
-- Token Production
-- ============================================================================

lexToken :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexToken state = do
  c <- peek state
  case c of
    -- Skip whitespace
    ' ' -> lexToken (advance state 1)
    '\t' -> lexToken (advanceTab state)

    -- Newlines - check for layout
    '\n' -> handleNewline state
    '\r' -> case peekAt state 1 of
      Just '\n' -> handleNewline (advance state 1)  -- Skip \r in \r\n
      _ -> handleNewline state

    -- Comments
    '-' -> case peekAt state 1 of
      Just '-' ->
        let state' = skipLineComment (advance state 2)
        in lexToken state'
      _ -> lexOperator state

    '{' -> case peekAt state 1 of
      Just '-' ->
        let state' = skipBlockComment (advance state 2) 1
        in lexToken state'
      _ -> lexDelimiter state

    -- String literal
    '"' -> lexString state

    -- Char literal
    '\'' -> lexChar state

    -- Number
    _ | isDigit c -> lexNumber state

    -- Identifier/keyword
    _ | isAlpha c || c == '_' -> lexIdentOrKeyword state

    -- Operators
    _ | isOperatorChar c -> lexOperator state

    -- Delimiters
    _ | isDelimiter c -> lexDelimiter state

    -- Unknown
    _ -> Just (Tuple (makeUnknownToken state c) (advance state 1))

-- ============================================================================
-- Layout Handling
-- ============================================================================

-- | Handle newline - check indentation for layout tokens
handleNewline :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
handleNewline state =
  let state' = advanceNewline (advance state 1)
      -- Skip blank lines and leading whitespace to find actual column
      Tuple col state'' = findNextColumn state'
  in case currentLayoutColumn state'' of
    Nothing -> lexToken state''  -- No layout context, continue
    Just layoutCol ->
      if col == layoutCol then
        -- Same indentation = separator
        Just (Tuple (makeLayoutToken (Cst.TokLayoutSep col) state'') state'')
      else if col < layoutCol then
        -- Dedent = end layout block(s)
        let Tuple endToks state''' = closeLayoutsTo col state''
        in case Array.uncons endToks of
          Just { head: tok, tail: rest } ->
            Just (Tuple tok (state''' { pendingTokens = rest <> state'''.pendingTokens }))
          Nothing -> lexToken state'''
      else
        -- More indentation = continuation, just continue
        lexToken state''

-- | Find the column of the next non-whitespace character
findNextColumn :: LexState -> Tuple Int LexState
findNextColumn state = case peek state of
  Just ' ' -> findNextColumn (advance state 1)
  Just '\t' -> findNextColumn (advanceTab state)
  Just '\n' -> findNextColumn (advanceNewline (advance state 1))
  Just '\r' -> case peekAt state 1 of
    Just '\n' -> findNextColumn (advanceNewline (advance state 2))
    _ -> findNextColumn (advanceNewline (advance state 1))
  _ -> Tuple state.column state

-- | Get current layout column from stack
currentLayoutColumn :: LexState -> Maybe Int
currentLayoutColumn state = do
  ctx <- Array.head state.layoutStack
  layoutColumn ctx

-- | Close layout blocks until we reach one at or below the given column
closeLayoutsTo :: Int -> LexState -> Tuple (Array Cst.SourceToken) LexState
closeLayoutsTo col state = go state []
  where
    go st acc = case Array.head st.layoutStack of
      Nothing -> Tuple acc st
      Just ctx -> case layoutColumn ctx of
        Nothing -> Tuple acc st  -- Non-indentation context, stop
        Just layoutCol ->
          if layoutCol > col then
            let tok = makeLayoutToken (Cst.TokLayoutEnd layoutCol) st
                st' = st { layoutStack = fromMaybe [] (Array.tail st.layoutStack) }
            in go st' (Array.snoc acc tok)
          else
            Tuple acc st

-- | Start a new layout block after a layout keyword
startLayout :: String -> LexState -> LexState
startLayout keyword state =
  let ctx = case keyword of
        "where" -> LytWhere state.column
        "of" -> LytOf state.column
        "do" -> LytDo state.column
        "ado" -> LytAdo state.column
        "let" -> LytLet state.column
        _ -> LytWhere state.column  -- fallback
      startTok = makeLayoutToken (Cst.TokLayoutStart state.column) state
  in state
    { layoutStack = Array.cons ctx state.layoutStack
    , pendingTokens = Array.snoc state.pendingTokens startTok
    }

-- ============================================================================
-- Token Lexers
-- ============================================================================

lexIdentOrKeyword :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexIdentOrKeyword state =
  let startPos = { line: state.line, column: state.column }
      Tuple name state' = consumeIdent state ""
      endPos = { line: state'.line, column: state'.column }
      isKeyword = name `Array.elem` keywords
      tok = if isKeyword
            then makeKeywordToken name startPos endPos
            else makeIdentToken name startPos endPos
      -- Check if this is a layout keyword
      state'' = if name `Array.elem` layoutKeywords
                then startLayout name state'
                else state'
  in Just (Tuple tok state'')

makeKeywordToken :: String -> Cst.SourcePos -> Cst.SourcePos -> Cst.SourceToken
makeKeywordToken name start end =
  let token = case name of
        "forall" -> Cst.TokForall
        _ -> Cst.TokLowerName Nothing name  -- Keywords as lowercase names for simplicity
  in { range: { start, end }
     , leadingComments: []
     , trailingComments: []
     , value: token
     }

makeIdentToken :: String -> Cst.SourcePos -> Cst.SourcePos -> Cst.SourceToken
makeIdentToken name start end =
  let isUpper = case CU.charAt 0 name of
        Just c -> c >= 'A' && c <= 'Z'
        Nothing -> false
      token = if isUpper
              then Cst.TokUpperName Nothing name
              else Cst.TokLowerName Nothing name
  in { range: { start, end }
     , leadingComments: []
     , trailingComments: []
     , value: token
     }

lexOperator :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexOperator state =
  let startPos = { line: state.line, column: state.column }
      Tuple op state' = consumeOperator state ""
      endPos = { line: state'.line, column: state'.column }
      token = case op of
        "=" -> Cst.TokEquals
        "|" -> Cst.TokPipe
        "." -> Cst.TokDot
        "\\" -> Cst.TokBackslash
        "@" -> Cst.TokAt
        "_" -> Cst.TokUnderscore
        "`" -> Cst.TokTick
        "->" -> Cst.TokRightArrow
        "<-" -> Cst.TokLeftArrow
        "=>" -> Cst.TokRightFatArrow
        "::" -> Cst.TokDoubleColon
        _ -> Cst.TokOperator Nothing op
  in Just (Tuple { range: { start: startPos, end: endPos }
                 , leadingComments: []
                 , trailingComments: []
                 , value: token
                 } state')

lexDelimiter :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexDelimiter state = do
  c <- peek state
  let startPos = { line: state.line, column: state.column }
      state' = advance state 1
      endPos = { line: state'.line, column: state'.column }
      token = case c of
        '(' -> Cst.TokLeftParen
        ')' -> Cst.TokRightParen
        '{' -> Cst.TokLeftBrace
        '}' -> Cst.TokRightBrace
        '[' -> Cst.TokLeftSquare
        ']' -> Cst.TokRightSquare
        ',' -> Cst.TokComma
        _ -> Cst.TokOperator Nothing (CU.singleton c)
      -- Update layout stack for braces
      state'' = case c of
        '(' -> state' { layoutStack = Array.cons LytParen state'.layoutStack }
        '{' -> state' { layoutStack = Array.cons LytBrace state'.layoutStack }
        '[' -> state' { layoutStack = Array.cons LytSquare state'.layoutStack }
        ')' -> popUntil LytParen state'
        '}' -> popUntil LytBrace state'
        ']' -> popUntil LytSquare state'
        _ -> state'
  Just (Tuple { range: { start: startPos, end: endPos }
              , leadingComments: []
              , trailingComments: []
              , value: token
              } state'')

-- | Pop layout stack until we find the matching context
popUntil :: LayoutContext -> LexState -> LexState
popUntil target state = go state.layoutStack
  where
    go stack = case Array.uncons stack of
      Nothing -> state
      Just { head: ctx, tail: rest } ->
        if matchContext ctx target
        then state { layoutStack = rest }
        else go rest

    matchContext LytParen LytParen = true
    matchContext LytBrace LytBrace = true
    matchContext LytSquare LytSquare = true
    matchContext _ _ = false

lexString :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexString state =
  let startPos = { line: state.line, column: state.column }
      Tuple str state' = consumeString (advance state 1) ""
      endPos = { line: state'.line, column: state'.column }
  in Just (Tuple { range: { start: startPos, end: endPos }
                 , leadingComments: []
                 , trailingComments: []
                 , value: Cst.TokString str str
                 } state')

lexChar :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexChar state =
  let startPos = { line: state.line, column: state.column }
      state1 = advance state 1
  in case peek state1 of
    Just '\\' -> case peekAt state1 1 of
      Just c -> case peekAt state1 2 of
        Just '\'' ->
          let ch = case c of
                'n' -> '\n'
                't' -> '\t'
                'r' -> '\r'
                _ -> c
              state' = advance state1 3
              endPos = { line: state'.line, column: state'.column }
          in Just (Tuple { range: { start: startPos, end: endPos }
                         , leadingComments: []
                         , trailingComments: []
                         , value: Cst.TokChar (CU.singleton c) ch
                         } state')
        _ -> Nothing
      _ -> Nothing
    Just c -> case peekAt state1 1 of
      Just '\'' ->
        let state' = advance state1 2
            endPos = { line: state'.line, column: state'.column }
        in Just (Tuple { range: { start: startPos, end: endPos }
                       , leadingComments: []
                       , trailingComments: []
                       , value: Cst.TokChar (CU.singleton c) c
                       } state')
      _ -> Nothing
    _ -> Nothing

lexNumber :: LexState -> Maybe (Tuple Cst.SourceToken LexState)
lexNumber state =
  let startPos = { line: state.line, column: state.column }
      Tuple numStr state' = consumeNumber state ""
      endPos = { line: state'.line, column: state'.column }
      -- Check if it's a float
      isFloat = String.contains (String.Pattern ".") numStr
      token = if isFloat
              then Cst.TokNumber numStr (readNumber numStr)
              else Cst.TokInt numStr (Cst.SmallInt (readInt numStr))
  in Just (Tuple { range: { start: startPos, end: endPos }
                 , leadingComments: []
                 , trailingComments: []
                 , value: token
                 } state')
  where
    readInt s = fromMaybe 0 (Int.fromString s)
    readNumber s = fromMaybe 0.0 (Number.fromString s)

-- ============================================================================
-- Helpers
-- ============================================================================

makeLayoutToken :: Cst.Token -> LexState -> Cst.SourceToken
makeLayoutToken tok state =
  { range: { start: { line: state.line, column: state.column }
           , end: { line: state.line, column: state.column } }
  , leadingComments: []
  , trailingComments: []
  , value: tok
  }

makeUnknownToken :: LexState -> Char -> Cst.SourceToken
makeUnknownToken state c =
  { range: { start: { line: state.line, column: state.column }
           , end: { line: state.line, column: state.column + 1 } }
  , leadingComments: []
  , trailingComments: []
  , value: Cst.TokOperator Nothing (CU.singleton c)
  }

-- | Character predicates
isAlpha :: Char -> Boolean
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

isIdentChar :: Char -> Boolean
isIdentChar c = isAlpha c || isDigit c || c == '_' || c == '\''

isOperatorChar :: Char -> Boolean
isOperatorChar c = c `Array.elem` ['+', '-', '*', '/', '=', '<', '>', '!', ':', '.', '|', '\\', '&', '$', '`', '#', '@', '~', '^', '?']

isDelimiter :: Char -> Boolean
isDelimiter c = c `Array.elem` ['(', ')', '{', '}', '[', ']', ',', ';']

-- | State helpers
peek :: LexState -> Maybe Char
peek state = Array.head state.chars

peekAt :: LexState -> Int -> Maybe Char
peekAt state n = Array.index state.chars n

advance :: LexState -> Int -> LexState
advance state n = state
  { pos = state.pos + n
  , column = state.column + n
  , chars = Array.drop n state.chars
  }

advanceTab :: LexState -> LexState
advanceTab state =
  let nextCol = state.column + (8 - ((state.column - 1) `mod` 8))
  in state { pos = state.pos + 1, column = nextCol, chars = Array.drop 1 state.chars }

advanceNewline :: LexState -> LexState
advanceNewline state = state { line = state.line + 1, column = 1 }

-- | Consume helpers
consumeIdent :: LexState -> String -> Tuple String LexState
consumeIdent state acc = case peek state of
  Just c | isIdentChar c -> consumeIdent (advance state 1) (acc <> CU.singleton c)
  _ -> Tuple acc state

consumeOperator :: LexState -> String -> Tuple String LexState
consumeOperator state acc = case peek state of
  Just c | isOperatorChar c && c /= '(' && c /= ')' && c /= '{' && c /= '}' && c /= '[' && c /= ']' && c /= ',' ->
    consumeOperator (advance state 1) (acc <> CU.singleton c)
  _ -> Tuple acc state

consumeString :: LexState -> String -> Tuple String LexState
consumeString state acc = case peek state of
  Nothing -> Tuple acc state
  Just '"' -> Tuple acc (advance state 1)
  Just '\\' -> case peekAt state 1 of
    Just c ->
      let escaped = case c of
            'n' -> "\n"
            't' -> "\t"
            'r' -> "\r"
            '\\' -> "\\"
            '"' -> "\""
            _ -> CU.singleton c
      in consumeString (advance state 2) (acc <> escaped)
    Nothing -> Tuple acc state
  Just '\n' -> consumeString (advanceNewline (advance state 1)) (acc <> "\n")
  Just c -> consumeString (advance state 1) (acc <> CU.singleton c)

consumeNumber :: LexState -> String -> Tuple String LexState
consumeNumber state acc = case peek state of
  Just c | isDigit c -> consumeNumber (advance state 1) (acc <> CU.singleton c)
  Just '.' -> case peekAt state 1 of
    Just d | isDigit d -> consumeNumber (advance state 2) (acc <> "." <> CU.singleton d)
    _ -> Tuple acc state
  _ -> Tuple acc state

skipLineComment :: LexState -> LexState
skipLineComment state = case peek state of
  Nothing -> state
  Just '\n' -> advanceNewline (advance state 1)
  Just '\r' -> case peekAt state 1 of
    Just '\n' -> advanceNewline (advance state 2)
    _ -> advanceNewline (advance state 1)
  Just _ -> skipLineComment (advance state 1)

skipBlockComment :: LexState -> Int -> LexState
skipBlockComment state 0 = state
skipBlockComment state depth = case peek state of
  Nothing -> state
  Just '-' -> case peekAt state 1 of
    Just '}' -> skipBlockComment (advance state 2) (depth - 1)
    _ -> skipBlockComment (advance state 1) depth
  Just '{' -> case peekAt state 1 of
    Just '-' -> skipBlockComment (advance state 2) (depth + 1)
    _ -> skipBlockComment (advance state 1) depth
  Just '\n' -> skipBlockComment (advanceNewline (advance state 1)) depth
  Just _ -> skipBlockComment (advance state 1) depth
