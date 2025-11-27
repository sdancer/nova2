module Test.Parser.TestFuncWithSig where

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
  log "=== parseFunctionWithTypeSignature Test ==="
  
  let input = """parseGuardPart :: Array Token -> ParseResult Ast.Expr
parseGuardPart tokens = do
  let tokens' = skipNewlines tokens
  case tryPatternBind tokens' of
    Right result -> Right result
    Left _ -> parseLogicalExpression tokens'
  where
    tryPatternBind toks = failure "Not a pattern bind" """
  
  let tokens = tokenize input
  log $ "Input: " <> input
  log ""
  log $ "First 10 tokens: " <> show (map showTok (Array.take 10 tokens))
  
  log ""
  log "parseFunctionWithTypeSignature:"
  case P.parseFunctionWithTypeSignature tokens of
    Right (Tuple decl rest) -> do
      log "✓ Success"
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
