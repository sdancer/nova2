module Test.Parser.TestExactCase where

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
  log "=== Exact Case Test ==="
  
  -- Exact structure from earlier failing test
  let input = """go toks acc = case Array.head toks of
      Nothing -> Tuple acc toks
      Just t
        -- Found the function name at column 1 (start of line definition)
        | t.tokenType == TokIdentifier, t.value == name, t.column == 1 ->
            Tuple acc toks
        -- Skip newlines but don't include them in type tokens
        | t.tokenType == TokNewline ->
            go (Array.drop 1 toks) acc
        -- Accumulate other tokens as part of the type
        | otherwise ->
            go (Array.drop 1 toks) (Array.snoc acc t)"""
  let tokens = tokenize input
  log $ "Tokens: " <> show (Array.length tokens)
  
  log ""
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

