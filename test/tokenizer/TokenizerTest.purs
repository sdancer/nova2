module Test.Tokenizer.TokenizerTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))

-- Simple test runner
main :: Effect Unit
main = do
  log "=== Tokenizer Tests ==="
  log ""

  -- Test 1: Keywords
  testCase "Keywords" "module where import let in if then else case of do" $ \tokens ->
    let kws = Array.filter (\t -> t.tokenType == TokKeyword) tokens
    in Array.length kws == 11

  -- Test 2: Identifiers
  testCase "Identifiers" "foo bar' _baz qux123" $ \tokens ->
    let ids = Array.filter (\t -> t.tokenType == TokIdentifier) tokens
    in Array.length ids == 4

  -- Test 3: Numbers
  testCase "Numbers" "42 3.14 0 999" $ \tokens ->
    let nums = Array.filter (\t -> t.tokenType == TokNumber) tokens
    in Array.length nums == 4

  -- Test 4: Strings
  testCase "String literal" "\"hello world\"" $ \tokens ->
    case Array.head tokens of
      Nothing -> false
      Just t -> t.tokenType == TokString && t.value == "hello world"

  -- Test 5: Operators
  testCase "Operators" "-> <- :: == /= <= >=" $ \tokens ->
    let ops = Array.filter (\t -> t.tokenType == TokOperator) tokens
    in Array.length ops == 7

  -- Test 6: Delimiters
  testCase "Delimiters" "( ) { } [ ] ," $ \tokens ->
    let delims = Array.filter (\t -> t.tokenType == TokDelimiter) tokens
    in Array.length delims == 7

  -- Test 7: Line comments
  testCase "Line comment" "foo -- this is a comment\nbar" $ \tokens ->
    let ids = Array.filter (\t -> t.tokenType == TokIdentifier) tokens
    in Array.length ids == 2

  -- Test 8: Block comments
  testCase "Block comment" "foo {- nested {- comment -} here -} bar" $ \tokens ->
    let ids = Array.filter (\t -> t.tokenType == TokIdentifier) tokens
    in Array.length ids == 2

  -- Test 9: Char literal
  testCase "Char literal" "'a' 'b' '\\n'" $ \tokens ->
    let chars = Array.filter (\t -> t.tokenType == TokChar) tokens
    in Array.length chars == 3

  -- Test 10: Simple function definition
  testCase "Function def" "add x y = x + y" $ \tokens ->
    Array.length tokens > 0

  -- Test 11: Real PureScript snippet
  testCase "PureScript snippet"
    """module Main where

import Prelude

main :: Effect Unit
main = log "Hello"
""" $ \tokens ->
    let
      kws = Array.filter (\t -> t.tokenType == TokKeyword) tokens
      ids = Array.filter (\t -> t.tokenType == TokIdentifier) tokens
    in Array.length kws >= 3 && Array.length ids >= 5

  log ""
  log "=== Tests Complete ==="

testCase :: String -> String -> (Array Token -> Boolean) -> Effect Unit
testCase name input check = do
  let tokens = tokenize input
  if check tokens
    then log $ "✓ " <> name
    else do
      log $ "✗ " <> name
      log $ "  Input: " <> show input
      log $ "  Tokens: " <> show (Array.length tokens)
      logTokens tokens

logTokens :: Array Token -> Effect Unit
logTokens tokens = traverse_ logToken tokens

logToken :: Token -> Effect Unit
logToken t = log $ "    " <> showTokenType t.tokenType <> ": " <> show t.value

showTokenType :: TokenType -> String
showTokenType TokKeyword = "Keyword"
showTokenType TokIdentifier = "Ident"
showTokenType TokNumber = "Number"
showTokenType TokString = "String"
showTokenType TokChar = "Char"
showTokenType TokOperator = "Operator"
showTokenType TokDelimiter = "Delim"
showTokenType TokNewline = "Newline"
showTokenType TokUnrecognized = "Unrecog"
