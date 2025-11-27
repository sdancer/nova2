module Test.Parser.TestBindingMany where

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
  log "=== parseBinding Test ==="
  
  -- Test parseBinding on individual bindings
  let input1 = "a = x"
  log $ "parseBinding on '" <> input1 <> "':"
  testBinding input1
  
  let input2 = "{ b: c } = x"
  log $ "parseBinding on '" <> input2 <> "':"
  testBinding input2
  
  -- Now test the sequence
  log ""
  log "Sequence test:"
  let seqInput = """a = x
{ b: c } = y"""
  let seqTokens = tokenize seqInput
  log $ "Tokens: " <> show (map showTok seqTokens)
  
  -- Try parsing first binding
  log "First binding:"
  case P.parsePattern (P.skipNewlines seqTokens) of
    Right (Tuple pat rest) -> do
      log $ "  Pattern OK, rest starts with: " <> show (map showTok (Array.take 3 rest))
      case expectEquals rest of
        Right (Tuple _ rest') -> do
          log $ "  '=' found"
          case P.parseExpression rest' of
            Right (Tuple expr rest'') -> do
              log $ "  Expr OK, rest: " <> show (map showTok (Array.take 5 rest''))
              -- Now try second binding
              let rest''' = P.skipNewlines rest''
              log $ "  After skipNewlines: " <> show (map showTok (Array.take 5 rest'''))
              case P.parsePattern rest''' of
                Right (Tuple pat2 rest4) -> do
                  log $ "  Second pattern OK"
                  log $ "  Rest: " <> show (map showTok (Array.take 3 rest4))
                Left err -> log $ "  Second pattern failed: " <> err
            Left err -> log $ "  Expr failed: " <> err
        Left err -> log $ "  '=' not found: " <> err
    Left err -> log $ "  Pattern failed: " <> err

expectEquals :: Array Token -> Either String (Tuple Unit (Array Token))
expectEquals tokens = case Array.head tokens of
  Just t | t.tokenType == TokOperator, t.value == "=" -> Right (Tuple unit (Array.drop 1 tokens))
  _ -> Left "Expected '='"

testBinding :: String -> Effect Unit
testBinding input = do
  let tokens = tokenize input
  case P.parsePattern (P.skipNewlines tokens) of
    Right (Tuple pat rest) -> do
      log $ "  Pattern parsed, next tokens: " <> show (map showTok (Array.take 3 rest))
    Left err -> log $ "  Pattern failed: " <> err

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

