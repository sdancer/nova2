module Test.Parser.TestRecordUpdate where

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
  log "=== Record Update Test ==="
  
  -- Test record with colon
  let input1 = "{ b: c }"
  let tokens1 = tokenize input1
  log "Record literal tokens:"
  log $ show (map showTok tokens1)
  log $ "isRecordUpdate: " <> show (isRecordUpdate (Array.drop 1 tokens1))
  
  log ""
  
  -- Test record with equals  
  let input2 = "{ b = c }"
  let tokens2 = tokenize input2
  log "Record update tokens:"
  log $ show (map showTok tokens2)
  log $ "isRecordUpdate: " <> show (isRecordUpdate (Array.drop 1 tokens2))

isRecordUpdate :: Array Token -> Boolean
isRecordUpdate toks =
  case Array.head toks of
    Just t1 | t1.tokenType == TokIdentifier ->
      case Array.head (Array.drop 1 toks) of
        Just t2 | t2.tokenType == TokOperator, t2.value == "=" -> true
        _ -> false
    _ -> false

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

