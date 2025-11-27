module Test.Parser.TestTryPatternBind3 where

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
  log "=== expression parsing test ==="
  
  -- The body of tryPatternBind
  let input = """do
      success unit
    
    patternToExpr"""
  
  log $ "Input:\n" <> input
  log ""
  
  let tokens = tokenize input
  log $ "Tokens: " <> show (map showTok tokens)
  
  log ""
  log "Trying parseExpression:"
  case P.parseExpression tokens of
    Right (Tuple expr rest) -> do
      log "✓ expression parsed"
      log $ "Rest: " <> show (map showTok rest)
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
