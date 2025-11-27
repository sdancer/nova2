module Test.Parser.TestLetRecordBind where

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
  log "=== Let Record Bind Test ==="
  
  let input = """skipSuperclassConstraints tokens =
  let tokens' = skipNewlines tokens
      { init: before, rest: after } = Array.span foo tokens'
  in case Array.head after of
    Just t | t.tokenType == TokOperator, t.value == "<=" -> Tuple (Array.drop 1 after) before
    _ -> Tuple tokens' []"""
  
  let tokens = tokenize input
  log $ "Tokens count: " <> show (Array.length tokens)
  
  log ""
  log "parseFunctionDeclarationRaw:"
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> fun.name
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
    Left err -> log $ "✗ " <> err

