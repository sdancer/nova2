module Test.Parser.TestParseFuncBody3 where

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
  log "=== Parse Function Body Test 3 ==="
  
  let input = """parseGuardExpression tokens = do
  Tuple guards rest <- parseGuardParts tokens []
  case Array.length guards of
    0 -> failure "Expected"
    1 -> success unit rest
    _ -> success (foldGuards guards) rest
  where
    foldGuards gs = pure unit"""
  
  let tokens = tokenize input
  
  -- Try parsing as function declaration using the raw parser
  log "parseFunctionDeclarationRaw:"
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple decl rest) -> do
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
