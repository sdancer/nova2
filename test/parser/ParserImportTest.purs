module Test.Parser.ParserImportTest where

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
  log "=== Import As Test ==="
  
  let input = "import Data.Array as Array"
  log $ "Input: " <> input
  let tokens = tokenize input
  log $ "Tokens: " <> show (map showTok tokens)
  
  log ""
  log "Parsing import:"
  case P.parseDeclaration tokens of
    Right (Tuple _ rest) -> do
      log "✓ Parsed"
      log $ "Rest: " <> show (map showTok rest)
    Left err -> log $ "✗ Error: " <> err

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
