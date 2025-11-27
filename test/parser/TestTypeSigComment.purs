module Test.Parser.TestTypeSigComment where

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
  log "=== Type Sig with Comment Test ==="
  
  let input = """foo :: Int -> Int
foo x =
  -- comment
  x"""
  let tokens = tokenize input
  log $ "Tokens: " <> show (Array.length tokens)
  log $ "All tokens:"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> show t.value)) (pure unit) tokens
  
  log ""
  log "parseDeclaration:"
  case P.parseDeclaration tokens of
    Right (Tuple decl rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
    Left err -> log $ "✗ " <> err

