module Test.Parser.TestSplitTypeRest where

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
  log "=== Split Type Rest Test ==="
  
  -- Test the actual function with type signature
  let input = """splitTypeAndRest :: Array Token -> String -> Tuple (Array Token) (Array Token)
splitTypeAndRest tokens name = go tokens []"""
  let tokens = tokenize input
  log $ "Tokens: " <> show (Array.length tokens)
  
  log ""
  log "parseDeclaration:"
  case P.parseDeclaration tokens of
    Right (Tuple decl rest) -> do
      log "✓ parsed"
      log $ "Rest: " <> show (Array.length (P.skipNewlines rest)) <> " tokens"
    Left err -> log $ "✗ " <> err

