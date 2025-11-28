module Nova.Compiler.FastTokenizer where

import Prelude

-- | Token types (same as original tokenizer)
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

-- | Foreign import for the fast tokenizer
-- | This calls into Elixir's binary pattern matching implementation
foreign import tokenizeFFI :: String -> Array RawToken

-- | Raw token from FFI (uses atoms as strings)
type RawToken =
  { token_type :: String  -- "tok_keyword", "tok_identifier", etc.
  , value :: String
  , line :: Int
  , column :: Int
  , pos :: Int
  }

-- | Convert raw token type string to TokenType
toTokenType :: String -> TokenType
toTokenType s = case s of
  "tok_keyword" -> TokKeyword
  "tok_identifier" -> TokIdentifier
  "tok_number" -> TokNumber
  "tok_string" -> TokString
  "tok_char" -> TokChar
  "tok_operator" -> TokOperator
  "tok_delimiter" -> TokDelimiter
  "tok_newline" -> TokNewline
  _ -> TokUnrecognized

-- | Convert raw token to Token
fromRawToken :: RawToken -> Token
fromRawToken raw =
  { tokenType: toTokenType raw.token_type
  , value: raw.value
  , line: raw.line
  , column: raw.column
  , pos: raw.pos
  }

-- | Main tokenize function - delegates to FFI
tokenize :: String -> Array Token
tokenize source = map fromRawToken (tokenizeFFI source)
