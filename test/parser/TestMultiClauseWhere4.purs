module Test.Parser.TestMultiClauseWhere4 where

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
  log "=== Multi-clause where test 4 ==="
  
  let input = """patternToExpr (Ast.PatVar v) = Ast.ExprVar v"""
  
  let tokens = tokenize input
  
  log "Step 1: parseIdentifierName"
  case P.parseIdentifierName tokens of
    Right (Tuple name rest) -> do
      log $ "  ✓ name = " <> name
      log $ "  rest: " <> show (map showTok rest)
      
      log "Step 2: parseMany parseSimplePattern"
      case P.parseMany P.parseSimplePattern rest of
        Right (Tuple params rest') -> do
          log $ "  ✓ params count = " <> show (Array.length params)
          log $ "  rest': " <> show (map showTok rest')
          
          log "Step 3: expectOperator ="
          case P.expectOperator rest' "=" of
            Right _ -> log "  ✓ found ="
            Left err -> log $ "  ✗ " <> err
        Left err -> log $ "  ✗ " <> err
    Left err -> log $ "  ✗ " <> err

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
