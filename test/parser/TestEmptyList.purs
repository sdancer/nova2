module Test.Parser.TestEmptyList where

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
  log "=== Empty List Test ==="
  
  -- Test: function with empty list in body
  let input = """foo x = bar x []"""
  let tokens = tokenize input
  log $ "Tokens: " <> show (map showTok tokens)
  
  log ""
  log "parseFunctionDeclarationRaw:"
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> fun.name
      log $ "Rest: " <> show (Array.length rest) <> " tokens"
    Left err -> log $ "✗ " <> err
  
  log ""
  log "---"
  log ""
  
  -- The actual case with where
  let input2 = """foo x = bar x []
  where
    bar y zs = y"""
  let tokens2 = tokenize input2
  log $ "With where - Tokens: " <> show (Array.length tokens2)
  
  log ""
  log "parseFunctionDeclarationRaw:"
  case P.parseFunctionDeclarationRaw tokens2 of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> fun.name
      log $ "Rest: " <> show (Array.length (P.skipNewlines rest)) <> " tokens"
    Left err -> log $ "✗ " <> err

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

