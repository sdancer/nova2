module Test.Parser.TestDropInstance where

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
  log "=== Drop Instance Test ==="
  
  let input = """dropInstanceConstraints :: Array Token -> Array Token
dropInstanceConstraints tokens =
  let tokens' = skipNewlines tokens
      { init: before, rest: after } = Array.span foo tokens'
  in case Array.head after of
    Just t -> bar
    _ -> tokens'"""
  let tokens = tokenize input
  log $ "Tokens: " <> show (Array.length tokens)
  
  log ""
  log "parseDeclaration:"
  case P.parseDeclaration tokens of
    Right (Tuple decl rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
    Left err -> log $ "✗ " <> err

