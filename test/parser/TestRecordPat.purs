module Test.Parser.TestRecordPat where

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
  log "=== Record Pattern Test ==="
  
  let input = """case y of
  Just { head, tail } -> head"""
  
  let tokens = tokenize input
  log $ "Tokens: " <> show (map showTok tokens)
  
  log "parseCaseExpression:"
  case P.parseExpression tokens of
    Right (Tuple expr rest) -> do
      log "✓ Success"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (map showTok (Array.take 10 rest'))
    Left err -> log $ "✗ " <> err

showTok :: Token -> String
showTok t = t.value <> "@L" <> show t.line <> "C" <> show t.column <> ":" <> showTokType t.tokenType

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
