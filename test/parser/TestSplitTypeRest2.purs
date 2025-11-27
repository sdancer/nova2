module Test.Parser.TestSplitTypeRest2 where

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
  log "=== Split Type Rest Test 2 ==="
  
  -- Test the actual function with type signature and where
  let input = """splitTypeAndRest :: Array Token -> String -> Tuple (Array Token) (Array Token)
splitTypeAndRest tokens name = go tokens []
  where
    go :: Array Token -> Array Token -> Tuple (Array Token) (Array Token)
    go toks acc = case Array.head toks of
      Nothing -> Tuple acc toks
      Just t -> go (Array.drop 1 toks) (Array.snoc acc t)"""
  let tokens = tokenize input
  log $ "Tokens: " <> show (Array.length tokens)
  
  log ""
  log "parseDeclaration:"
  case P.parseDeclaration tokens of
    Right (Tuple decl rest) -> do
      log "✓ parsed"
      log $ "Rest: " <> show (Array.length (P.skipNewlines rest)) <> " tokens"
    Left err -> log $ "✗ " <> err

