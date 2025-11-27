module Test.Parser.TestFunctionWithCase where

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
  log "=== Function With Case Test ==="
  
  let input = """go toks acc = case Array.head toks of
      Nothing -> Tuple acc toks
      Just t
        -- comment
        | guard -> body"""
  let tokens = tokenize input
  
  log "parseFunctionDeclarationRaw:"
  case P.parseFunctionDeclarationRaw tokens of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> fun.name
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
      when (Array.length rest' > 0) $ do
        log "First rest tokens:"
        void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> " " <> t.value)) (pure unit) (Array.take 10 rest')
    Left err -> log $ "✗ " <> err

