module Test.Parser.TestMultiClauseWhere3 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Multi-clause where test 3 ==="
  
  -- Just the constructor pattern function
  let input = """patternToExpr (Ast.PatVar v) = Ast.ExprVar v"""
  
  let tokens = tokenize input
  log $ "Tokens: " <> show (map showTok tokens)
  
  log ""
  log "parseFunctionDeclarationRaw:"
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> fun.name
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (map showTok rest')
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
