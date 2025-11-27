module Test.Parser.TestLetTokens where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Let Tokens Test ==="
  
  -- Test: Simple then record
  let input = """let a = x
      { b: c } = x
  in c"""
  
  let tokens = tokenize input
  log $ "Tokens: " <> show (map showTok tokens)
  
  log ""
  log "parseLetExpression:"
  case P.parseExpression tokens of
    Right (Tuple expr rest) -> do
      log "✓ parsed"
      log $ "Rest: " <> show (map showTok (Array.take 5 rest))
    Left err -> log $ "✗ " <> err
  
  log ""
  log "parseBinding on 'a = x':"
  let bindTokens = tokenize "a = x"
  case P.parsePattern bindTokens of
    Right (Tuple pat rest) -> do
      log $ "Pattern parsed, rest: " <> show (map showTok rest)
    Left err -> log $ "Pattern failed: " <> err

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

