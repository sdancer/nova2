module Test.Parser.TestLetParsing where

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
  log "=== Let Parsing Debug ==="
  
  -- The problematic input
  let input = """let a = x
      { b: c } = y
  in c"""
  let tokens = tokenize input
  log $ "Input: " <> show input
  log $ "Tokens: " <> show (map showTok tokens)
  
  log ""
  log "Manual parsing:"
  
  -- 1. Expect "let"
  let tokens' = P.skipNewlines tokens
  case Array.head tokens' of
    Just t | t.tokenType == TokKeyword, t.value == "let" -> do
      log "1. Found 'let'"
      let rest = Array.drop 1 tokens'
      let rest' = P.skipNewlines rest
      log $ "   After let: " <> show (map showTok (Array.take 5 rest'))
      
      -- 2. Parse first binding
      case P.parsePattern rest' of
        Right (Tuple pat rest2) -> do
          log "2. First pattern OK"
          log $ "   After pattern: " <> show (map showTok (Array.take 3 rest2))
          case expectEquals rest2 of
            Right (Tuple _ rest3) -> do
              log "3. Found '='"
              case P.parseExpression rest3 of
                Right (Tuple expr rest4) -> do
                  log "4. First expr OK"
                  log $ "   After expr: " <> show (map showTok (Array.take 7 rest4))
                  
                  -- 3. Try second binding
                  let rest5 = P.skipNewlines rest4
                  log $ "   After skipNewlines: " <> show (map showTok (Array.take 7 rest5))
                  
                  -- The question: is this "in" or another binding?
                  case Array.head rest5 of
                    Just t2 | t2.tokenType == TokKeyword, t2.value == "in" -> 
                      log "5. Found 'in' - but shouldn't we have second binding?"
                    Just t2 | t2.tokenType == TokDelimiter, t2.value == "{" -> do
                      log "5. Found '{' - trying second binding"
                      case P.parsePattern rest5 of
                        Right (Tuple pat2 rest6) -> do
                          log "6. Second pattern OK"
                          log $ "   After pattern: " <> show (map showTok (Array.take 5 rest6))
                        Left err -> log $ "6. Second pattern failed: " <> err
                    Just t2 -> log $ "5. Unexpected token: " <> t2.value
                    Nothing -> log "5. No more tokens"
                Left err -> log $ "4. First expr failed: " <> err
            Left err -> log $ "3. '=' not found: " <> err
        Left err -> log $ "2. First pattern failed: " <> err
    _ -> log "1. 'let' not found"

expectEquals :: Array Token -> Either String (Tuple Unit (Array Token))
expectEquals tokens = case Array.head tokens of
  Just t | t.tokenType == TokOperator, t.value == "=" -> Right (Tuple unit (Array.drop 1 tokens))
  _ -> Left "Expected '='"

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

