module Test.Parser.TestExprRecord where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Expr Record Test ==="
  
  -- Test 1: Just x
  let input1 = "x"
  let tokens1 = tokenize input1
  log $ "parseExpression 'x': "
  case P.parseExpression tokens1 of
    Right (Tuple expr rest) -> log $ "  OK, rest: " <> show (map showTok rest)
    Left err -> log $ "  Failed: " <> err
  
  -- Test 2: x { a: b }
  let input2 = "x { a: b }"
  let tokens2 = tokenize input2
  log $ "parseExpression 'x { a: b }': "
  case P.parseExpression tokens2 of
    Right (Tuple expr rest) -> log $ "  OK, rest: " <> show (map showTok rest)
    Left err -> log $ "  Failed: " <> err
  
  -- Test 3: x followed by newline and record
  let input3 = """x
{ a: b }"""
  let tokens3 = tokenize input3
  log $ "Tokens for 'x\\n{ a: b }': " <> show (map showTok tokens3)
  log $ "parseExpression 'x\\n{ a: b }': "
  case P.parseExpression tokens3 of
    Right (Tuple expr rest) -> log $ "  OK, rest: " <> show (map showTok rest)
    Left err -> log $ "  Failed: " <> err

showTok :: Token -> String
showTok t = t.value <> ":" <> showTokType t.tokenType

showTokType :: TokenType -> String
showTokType TokKeyword = "K"
showTokType TokIdentifier = "I"
showTokType TokOperator = "O"
showTokType TokDelimiter = "D"
showTokType TokNewline = "NL"
showTokType TokNumber = "N"
showTokType TokString = "S"
showTokType TokChar = "C"
showTokType TokUnrecognized = "?"

