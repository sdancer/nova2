module Test.Parser.TestGuardPartFuncL where

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
  log "=== Test parseGuardPart simpler where ==="
  
  -- Simplified version with just patternToExpr
  let input = """parseGuardPart tokens = pure unit
  where
    patternToExpr :: Ast.Pattern -> Ast.Expr
    patternToExpr (Ast.PatVar v) = Ast.ExprVar v
    patternToExpr (Ast.PatCon c args) = foldl Ast.ExprApp (Ast.ExprVar c) (map patternToExpr args)"""
  
  let tokens = tokenize input
  log $ "Tokens count: " <> show (Array.length tokens)
  log $ "Tokens: " <> show (map showTok (Array.take 30 tokens))
  
  log ""
  log "parseFunctionDeclarationRaw:"
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> fun.name
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (map showTok (Array.take 5 rest'))
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
