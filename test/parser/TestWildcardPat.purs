module Test.Parser.TestWildcardPat where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Wildcard Pattern Test ==="
  
  let input = "Ast.PatWildcard"
  
  let tokens = tokenize input
  log $ "Tokens: " <> show (map showTok tokens)
  
  log ""
  log "parseSimplePattern:"
  case P.parseSimplePattern tokens of
    Right (Tuple pat rest) -> do
      log "✓ parsed"
      log $ "Rest: " <> show (map showTok rest)
    Left err -> log $ "✗ " <> err

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
