module Test.Parser.TestParseFuncBody5 where

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
  log "=== Parse Function Body Test 5 ==="
  
  -- Test with just the problematic where clause
  let input = """parseGuardExpression tokens = do
  pure unit
  where
    foldGuards gs = case Array.uncons gs of
      Just { head, tail } -> Array.foldl foo head tail
      Nothing -> Ast.ExprLit (Ast.LitBool true)"""
  
  let tokens = tokenize input
  
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
