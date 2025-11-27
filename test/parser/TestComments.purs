module Test.Parser.TestComments where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))

main :: Effect Unit
main = do
  log "=== Comments Test ==="
  
  let input = """foo x = bar
  -- comment here
  where
    bar = x"""
  let tokens = tokenize input
  log "Tokens:"
  void $ Array.foldl (\acc t -> acc *> log ("  " <> showTok t)) (pure unit) tokens

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

