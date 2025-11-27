module Test.Parser.TestWhereTypeSig where

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
  log "=== Where with Type Sig Test ==="
  
  let input = """splitTypeAndRest tokens name = go tokens []
  where
    go :: Array Token -> Tuple (Array Token) (Array Token)
    go toks acc = Tuple acc toks"""
  let tokens = tokenize input
  log $ "Tokens: " <> show (Array.length tokens)
  
  log ""
  log "parseFunctionDeclarationRaw:"
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> fun.name
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
    Left err -> log $ "✗ " <> err

