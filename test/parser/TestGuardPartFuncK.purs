module Test.Parser.TestGuardPartFuncK where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Direct Test of parseGuardPart Function Decl ==="
  
  -- Input that should match rest3 from the actual Parser.purs parsing
  let input = """parseGuardPart tokens = do
  let tokens' = skipNewlines tokens
  case tryPatternBind tokens' of
    Right result -> Right result
    Left _ -> parseLogicalExpression tokens'
  where
    tryPatternBind toks = do
      Tuple pat rest <- parsePattern toks
      let rest' = skipNewlines rest
      case Array.head rest' of
        Just t | t.tokenType == TokOperator, t.value == "<-" -> do
          Tuple expr rest'' <- parseLogicalExpression (Array.drop 1 rest')
          success (Ast.ExprBinOp "<-" (patternToExpr pat) expr) rest''
        _ -> failure "Not a pattern bind"

    patternToExpr :: Ast.Pattern -> Ast.Expr
    patternToExpr (Ast.PatVar v) = Ast.ExprVar v
    patternToExpr (Ast.PatCon c args) = foldl Ast.ExprApp (Ast.ExprVar c) (map patternToExpr args)
    patternToExpr (Ast.PatLit l) = Ast.ExprLit l
    patternToExpr Ast.PatWildcard = Ast.ExprVar "_"
    patternToExpr (Ast.PatRecord fields) = Ast.ExprRecord (map foo fields)
    patternToExpr (Ast.PatCons h t) = Ast.ExprBinOp ":" (patternToExpr h) (patternToExpr t)
    patternToExpr (Ast.PatAs n p) = patternToExpr p
    patternToExpr (Ast.PatList ps) = Ast.ExprList (map patternToExpr ps)
    patternToExpr (Ast.PatParens p) = Ast.ExprParens (patternToExpr p)"""
  
  let tokens = tokenize input
  log $ "Tokens count: " <> show (Array.length tokens)
  
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
