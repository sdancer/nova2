module Test.Parser.TestLetDebug where

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
  log "=== Let Debug ==="
  
  let input = """let a = x
      { b: c } = y
  in c"""
  
  let tokens = tokenize input
  log $ "All tokens:"
  logTokens tokens
  
  log ""
  log "Starting parse..."
  
  -- After let keyword
  let afterLet = Array.drop 1 tokens  -- drop "let"
  let afterLetSkipped = P.skipNewlines afterLet
  log $ "After 'let' and skipNewlines:"
  logTokens (Array.take 5 afterLetSkipped)
  
  -- After first pattern (a)
  case P.parsePattern afterLetSkipped of
    Right (Tuple pat afterPat) -> do
      log ""
      log "After pattern 'a':"
      logTokens (Array.take 5 afterPat)
      
      -- After =
      case expectOp afterPat "=" of
        Right (Tuple _ afterEq) -> do
          log ""
          log "After '=':"
          logTokens (Array.take 5 afterEq)
          
          -- After expression x
          case P.parseExpression afterEq of
            Right (Tuple expr afterExpr) -> do
              log ""
              log "After expression (should be just 'x'):"
              logTokens (Array.take 8 afterExpr)
            Left err -> log $ "Expr failed: " <> err
        Left err -> log $ "= failed: " <> err
    Left err -> log $ "Pattern failed: " <> err

expectOp :: Array Token -> String -> Either String (Tuple Unit (Array Token))
expectOp tokens op = case Array.head tokens of
  Just t | t.tokenType == TokOperator, t.value == op -> Right (Tuple unit (Array.drop 1 tokens))
  _ -> Left $ "Expected '" <> op <> "'"

logTokens :: Array Token -> Effect Unit
logTokens tokens = do
  let fmt t = "  " <> t.value <> " (" <> showTokType t.tokenType <> ") L" <> show t.line <> "C" <> show t.column
  void $ Array.foldl (\acc t -> acc *> log (fmt t)) (pure unit) tokens

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

