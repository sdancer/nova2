module Test.Parser.TestTryPatternBind where

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
  log "=== tryPatternBind parsing test ==="
  
  -- Just the tryPatternBind function from parseGuardPart's where
  let input = """tryPatternBind toks = do
      Tuple pat rest <- parsePattern toks
      let rest' = skipNewlines rest
      case Array.head rest' of
        Just t | t.tokenType == TokOperator, t.value == "<-" -> do
          Tuple expr rest'' <- parseLogicalExpression (Array.drop 1 rest')
          success (Ast.ExprBinOp "<-" (patternToExpr pat) expr) rest''
        _ -> failure "Not a pattern bind" """
  
  log $ "Input:\n" <> input
  log ""
  
  let tokens = tokenize input
  log $ "Tokens count: " <> show (Array.length tokens)
  log $ "Last 5 tokens: " <> show (map showTok (Array.takeEnd 5 tokens))
  
  log ""
  log "Trying parseFunctionDeclarationRaw:"
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> do
      log $ "✓ Got function: " <> fun.name
      log $ "Rest count: " <> show (Array.length rest)
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
