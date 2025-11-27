module Test.Parser.TestGuardPartFuncE where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast as Ast

main :: Effect Unit
main = do
  log "=== Parse Function Raw Test ==="
  
  -- Exactly what rest3 should look like
  let input = """parseGuardPart tokens = do
  let tokens' = skipNewlines tokens
  case tryPatternBind tokens' of
    Right result -> Right result
    Left _ -> parseLogicalExpression tokens'
  where
    tryPatternBind toks = do
      success unit"""
  
  let tokens = tokenize input
  log $ "First 15 tokens: " <> show (map showTok (Array.take 15 tokens))
  
  log ""
  log "parseFunctionDeclarationRaw:"
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> fun.name
      log $ "Rest: " <> show (map showTok (Array.take 5 rest))
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
