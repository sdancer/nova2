module Test.Parser.BacktickTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Backtick Test ==="

  -- Test tokenizing backtick infix
  let input1 = "c `Array.elem` xs"
  log $ "Input: " <> input1
  let tokens1 = tokenize input1
  log $ "Tokens: " <> show (map showTok tokens1)

  -- Test parsing it
  log "Parsing as expression:"
  case P.parseExpression tokens1 of
    Right (Tuple _ rest) -> log $ "✓ Parsed, remaining: " <> show (Array.length rest)
    Left err -> log $ "✗ Error: " <> err

  -- Test a full function
  let input2 = "isOp c = c `elem` ops"
  log $ "\nInput: " <> input2
  let tokens2 = tokenize input2
  log $ "Tokens: " <> show (map showTok tokens2)
  case P.parseDeclaration tokens2 of
    Right (Tuple _ rest) -> log $ "✓ Parsed, remaining: " <> show (Array.length rest)
    Left err -> log $ "✗ Error: " <> err

showTok :: forall r. { value :: String, tokenType :: TokenType | r } -> String
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
