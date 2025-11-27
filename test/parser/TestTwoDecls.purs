module Test.Parser.TestTwoDecls where

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
  log "=== Two Declarations Test ==="
  
  -- The exact transition from dropInstanceConstraints to parseFunctionWithTypeSignature
  let input = """dropInstanceConstraints :: Array Token -> Array Token
dropInstanceConstraints tokens =
  -- comment 1
  let tokens' = skipNewlines tokens
      -- comment 2
      { init: before, rest: after } = Array.span foo tokens'
  in case Array.head after of
    -- comment 3
    Just t | x == y, z < 20 ->
      skipNewlines bar
    _ -> tokens'

parseFunctionWithTypeSignature :: Array Token -> ParseResult Ast.Declaration"""
  let tokens = tokenize input
  log $ "Tokens: " <> show (Array.length tokens)
  
  log ""
  log "First declaration:"
  case P.parseDeclaration tokens of
    Right (Tuple decl rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
      log $ "First few of rest:"
      void $ Array.foldl (\acc t -> acc *> log ("  " <> t.value <> " L" <> show t.line <> "C" <> show t.column)) (pure unit) (Array.take 5 rest')
    Left err -> log $ "✗ " <> err

