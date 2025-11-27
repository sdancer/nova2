module Test.Parser.TestTryPatternBind2 where

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
  log "=== tryPatternBind + patternToExpr test ==="
  
  -- Both functions at the same indentation (in a where clause)
  let input = """tryPatternBind toks = do
      success unit
    
    patternToExpr :: Ast.Pattern -> Ast.Expr
    patternToExpr (Ast.PatVar v) = Ast.ExprVar v
    patternToExpr (Ast.PatCon c args) = foldl Ast.ExprApp (Ast.ExprVar c) (map patternToExpr args)"""
  
  log $ "Input:\n" <> input
  log ""
  
  let tokens = tokenize input
  log $ "Tokens count: " <> show (Array.length tokens)
  
  log ""
  log "Trying parseFunctionDeclarationRaw on first function:"
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> do
      log $ "✓ Got function: " <> fun.name
      let rest' = P.skipNewlines rest
      log $ "Rest count: " <> show (Array.length rest')
      log $ "Rest first 10: " <> show (map showTok (Array.take 10 rest'))
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
