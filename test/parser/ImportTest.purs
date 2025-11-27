module Test.Parser.ImportTest where

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
  log "=== Qualified Type Debug ==="
  
  let input = """foo :: ParseResult Ast.Expr
foo = unit"""
  log $ "Input:"
  log input
  log ""
  let tokens = tokenize input
  log $ "Tokens: " <> show (map showTok tokens)
  
  log ""
  log "Parsing type:"
  case P.parseType (Array.drop 2 tokens) of  -- Skip "foo ::"
    Right (Tuple _ rest) -> do
      log "✓ Parsed"
      log $ "Rest: " <> show (map showTok (Array.take 10 rest))
    Left err -> log $ "✗ Error: " <> err

showTok :: Token -> String
showTok t = t.value <> "@" <> show t.column <> ":" <> showTokType t.tokenType

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
