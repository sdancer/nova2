module Test.Parser.TestGuardPartFuncF where

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
  log "=== Parse Function Raw Test F ==="
  
  -- Full parseGuardPart with both where bindings
  let input = """parseGuardPart tokens = do
  let tokens' = skipNewlines tokens
  case tryPatternBind tokens' of
    Right result -> Right result
    Left _ -> parseLogicalExpression tokens'
  where
    tryPatternBind toks = do
      success unit
    
    patternToExpr :: Ast.Pattern -> Ast.Expr
    patternToExpr (Ast.PatVar v) = Ast.ExprVar v
    patternToExpr (Ast.PatCon c args) = foldl Ast.ExprApp (Ast.ExprVar c) (map patternToExpr args)"""
  
  let tokens = tokenize input
  log $ "First 15 tokens: " <> show (map showTok (Array.take 15 tokens))
  
  log ""
  log "parseFunctionDeclarationRaw:"
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> fun.name
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
