module Nova.Compiler.Tokenizer where

import Prelude
import Data.Array as Array
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

-- | Token types
data TokenType
  = TokKeyword
  | TokIdentifier
  | TokNumber
  | TokString
  | TokChar
  | TokOperator
  | TokDelimiter
  | TokNewline
  | TokUnrecognized

derive instance eqTokenType :: Eq TokenType

-- | Token with position information
type Token =
  { tokenType :: TokenType
  , value :: String
  , line :: Int
  , column :: Int
  , pos :: Int
  }

mkToken :: TokenType -> String -> Int -> Int -> Int -> Token
mkToken tokenType value line column pos =
  { tokenType, value, line, column, pos }

-- | Keywords in the language
keywords :: Array String
keywords =
  [ "foreign", "module", "where", "import", "data", "type"
  , "class", "instance", "let", "in", "if", "then", "else"
  , "case", "of", "do", "derive", "newtype", "infixl", "infixr", "infix"
  , "forall"
  ]

-- | Multi-character operators (longer ones first for matching)
operators :: Array String
operators =
  [ "==", "/=", "!=", "<=", ">=", "->", "<-", "::", "++"
  , "++=", ">>=", ">>", "<<", "&&", "||", "<>", ".."
  , "+", "-", "*", "/", "<", ">", "=", "$", "`", ".", "|", "\\", "&", ":"
  ]

-- | Single-char operator starters
isOperatorChar :: Char -> Boolean
isOperatorChar c = c `Array.elem` ['+', '-', '*', '/', '=', '<', '>', '!', ':', '.', '|', '\\', '&', '$', '`', '#']

-- | Delimiter characters
isDelimiter :: Char -> Boolean
isDelimiter c = c `Array.elem` ['(', ')', '{', '}', '[', ']', ',', ';']

-- | Is alphabetic
isAlpha :: Char -> Boolean
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

-- | Is digit
isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

-- | Is alphanumeric or underscore/quote (for identifiers)
isIdentChar :: Char -> Boolean
isIdentChar c = isAlpha c || isDigit c || c == '_' || c == '\''

-- | Tokenizer state
type TokState =
  { input :: String
  , pos :: Int
  , line :: Int
  , column :: Int
  }

initState :: String -> TokState
initState input = { input, pos: 0, line: 1, column: 1 }

-- | Main tokenize function
tokenize :: String -> Array Token
tokenize source = go (initState source) []
  where
    go :: TokState -> Array Token -> Array Token
    go state acc = case nextToken state of
      Nothing -> Array.reverse acc
      Just (Tuple tok state') -> go state' (Array.cons tok acc)

-- | Get the next token, if any
nextToken :: TokState -> Maybe (Tuple Token TokState)
nextToken state = do
  c <- peek state
  case c of
    -- Whitespace (skip spaces and tabs)
    ' ' -> nextToken (advance state 1)
    '\t' -> nextToken (advanceTab state)

    -- Newlines
    '\r' -> case peekAt state 1 of
      Just '\n' -> Just (Tuple (mkToken TokNewline "\n" state.line state.column state.pos) (advanceNewline (advance state 2)))
      _ -> Just (Tuple (mkToken TokNewline "\n" state.line state.column state.pos) (advanceNewline (advance state 1)))
    '\n' -> Just (Tuple (mkToken TokNewline "\n" state.line state.column state.pos) (advanceNewline (advance state 1)))

    -- Line comment
    '-' -> case peekAt state 1 of
      Just '-' ->
        let state' = consumeLineComment (advance state 2)
            -- Consume the newline character itself (if present) to avoid double-counting
            state'' = case peek state' of
              Just '\n' -> advance state' 1
              Just '\r' -> case peekAt state' 1 of
                Just '\n' -> advance state' 2  -- \r\n
                _ -> advance state' 1  -- Just \r
              _ -> state'  -- EOF
        in Just (Tuple (mkToken TokNewline "\n" state.line state.column state.pos) (advanceNewline state''))
      _ -> tokenizeOperator state

    -- Block comment
    '{' -> case peekAt state 1 of
      Just '-' -> nextToken (consumeBlockComment (advance state 2) 1)
      _ -> tokenizeDelimiter state

    -- String literal
    '"' -> tokenizeString state

    -- Char literal
    '\'' -> tokenizeChar state

    -- Number
    _ | isDigit c -> tokenizeNumber state

    -- Identifier or keyword
    _ | isAlpha c || c == '_' -> tokenizeIdentifier state

    -- Operator
    _ | isOperatorChar c -> tokenizeOperator state

    -- Delimiter
    _ | isDelimiter c -> tokenizeDelimiter state

    -- Unrecognized
    _ -> Just (Tuple (mkToken TokUnrecognized (CU.singleton c) state.line state.column state.pos) (advance state 1))

-- | Peek at current character
peek :: TokState -> Maybe Char
peek state = CU.charAt state.pos state.input

-- | Peek at character at offset
peekAt :: TokState -> Int -> Maybe Char
peekAt state offset = CU.charAt (state.pos + offset) state.input

-- | Advance state by n characters
advance :: TokState -> Int -> TokState
advance state n = state { pos = state.pos + n, column = state.column + n }

-- | Advance for tab (to next 8-column stop)
advanceTab :: TokState -> TokState
advanceTab state =
  let nextCol = state.column + (8 - ((state.column - 1) `mod` 8))
  in state { pos = state.pos + 1, column = nextCol }

-- | Advance to new line
advanceNewline :: TokState -> TokState
advanceNewline state = state { line = state.line + 1, column = 1 }

-- | Consume until end of line
consumeLineComment :: TokState -> TokState
consumeLineComment state = case peek state of
  Nothing -> state
  Just '\n' -> state
  Just '\r' -> state
  Just _ -> consumeLineComment (advance state 1)

-- | Consume block comment with nesting
consumeBlockComment :: TokState -> Int -> TokState
consumeBlockComment state 0 = state
consumeBlockComment state depth = case peek state of
  Nothing -> state
  Just '-' -> case peekAt state 1 of
    Just '}' -> consumeBlockComment (advance state 2) (depth - 1)
    _ -> consumeBlockComment (advance state 1) depth
  Just '{' -> case peekAt state 1 of
    Just '-' -> consumeBlockComment (advance state 2) (depth + 1)
    _ -> consumeBlockComment (advance state 1) depth
  Just '\n' -> consumeBlockComment (advanceNewline (advance state 1)) depth
  Just _ -> consumeBlockComment (advance state 1) depth

-- | Tokenize string literal
tokenizeString :: TokState -> Maybe (Tuple Token TokState)
tokenizeString state =
  let startLine = state.line
      startCol = state.column
      startPos = state.pos
      Tuple str state' = consumeString (advance state 1) ""
      tok = mkToken TokString str startLine startCol startPos
  in Just (Tuple tok state')

consumeString :: TokState -> String -> Tuple String TokState
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

-- | Tokenize char literal
tokenizeChar :: TokState -> Maybe (Tuple Token TokState)
tokenizeChar state =
  let startLine = state.line
      startCol = state.column
      startPos = state.pos
      state1 = advance state 1  -- skip opening quote
  in case peek state1 of
    Just '\\' -> case peekAt state1 1 of
      Just c -> case peekAt state1 2 of
        Just '\'' ->
          let val = case c of
                'n' -> "\n"
                't' -> "\t"
                'r' -> "\r"
                '\\' -> "\\"
                '\'' -> "'"
                _ -> CU.singleton c
          in Just (Tuple (mkToken TokChar val startLine startCol startPos) (advance state1 3))
        _ -> Just (Tuple (mkToken TokUnrecognized "'" startLine startCol startPos) state1)
      _ -> Just (Tuple (mkToken TokUnrecognized "'" startLine startCol startPos) state1)
    Just c -> case peekAt state1 1 of
      Just '\'' -> Just (Tuple (mkToken TokChar (CU.singleton c) startLine startCol startPos) (advance state1 2))
      _ -> Just (Tuple (mkToken TokUnrecognized "'" startLine startCol startPos) state1)
    Nothing -> Just (Tuple (mkToken TokUnrecognized "'" startLine startCol startPos) state1)

-- | Tokenize number (integer or float)
tokenizeNumber :: TokState -> Maybe (Tuple Token TokState)
tokenizeNumber state =
  let startLine = state.line
      startCol = state.column
      startPos = state.pos
      Tuple num state' = consumeNumber state ""
  in Just (Tuple (mkToken TokNumber num startLine startCol startPos) state')

consumeNumber :: TokState -> String -> Tuple String TokState
consumeNumber state acc = case peek state of
  Just c | isDigit c -> consumeNumber (advance state 1) (acc <> CU.singleton c)
  Just '.' -> case peekAt state 1 of
    Just d | isDigit d -> consumeNumber (advance state 2) (acc <> "." <> CU.singleton d)
    _ -> Tuple acc state
  _ -> Tuple acc state

-- | Tokenize identifier or keyword
tokenizeIdentifier :: TokState -> Maybe (Tuple Token TokState)
tokenizeIdentifier state =
  let startLine = state.line
      startCol = state.column
      startPos = state.pos
      Tuple ident state' = consumeIdent state ""
      tokType = if ident `Array.elem` keywords then TokKeyword else TokIdentifier
  in Just (Tuple (mkToken tokType ident startLine startCol startPos) state')

consumeIdent :: TokState -> String -> Tuple String TokState
consumeIdent state acc = case peek state of
  Just c | isIdentChar c -> consumeIdent (advance state 1) (acc <> CU.singleton c)
  _ -> Tuple acc state

-- | Tokenize operator
tokenizeOperator :: TokState -> Maybe (Tuple Token TokState)
tokenizeOperator state =
  let startLine = state.line
      startCol = state.column
      startPos = state.pos
      remaining = CU.drop state.pos state.input
      -- Find longest matching operator
      op = findOperator remaining operators
      len = String.length op
      tokType = TokOperator
  in Just (Tuple (mkToken tokType op startLine startCol startPos) (advance state len))

findOperator :: String -> Array String -> String
findOperator input ops = case Array.find (\op -> String.take (String.length op) input == op) ops of
  Just op -> op
  Nothing -> String.take 1 input

-- | Tokenize delimiter
tokenizeDelimiter :: TokState -> Maybe (Tuple Token TokState)
tokenizeDelimiter state = do
  c <- peek state
  let tok = mkToken TokDelimiter (CU.singleton c) state.line state.column state.pos
  Just (Tuple tok (advance state 1))
