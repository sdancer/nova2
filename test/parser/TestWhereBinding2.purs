module Test.Parser.TestWhereBinding2 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Where Binding Test 2 ==="
  
  -- Properly indented where binding
  let input = """    go :: Array Token -> Array Token -> Tuple (Array Token) (Array Token)
    go toks acc = case Array.head toks of
      Nothing -> Tuple acc toks
      Just t
        -- comment
        | test -> x"""
  let tokens = tokenize input
  
  log $ "First 15 tokens:"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> t.value)) (pure unit) (Array.take 15 tokens)
  
  log ""
  log "skipToNextLine with whereCol=3:"
  let skipped = skipToNextLine tokens 3
  log $ "After skip, first 10 tokens:"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> t.value)) (pure unit) (Array.take 10 skipped)
  
  log ""
  log "parseFunctionDeclarationRaw on skipped:"
  case P.parseFunctionDeclarationRaw skipped of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> fun.name
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
    Left err -> log $ "✗ " <> err

-- Copy of skipToNextLine
skipToNextLine :: Array Token -> Int -> Array Token
skipToNextLine toks whereCol =
  case Array.head toks of
    Nothing -> toks
    Just t | t.tokenType == TokNewline ->
      let rest = Array.drop 1 toks
      in case Array.head rest of
        Just t' | t'.column <= whereCol -> toks  -- Stop before the newline - outdented content
        Just t' | t'.tokenType == TokNewline -> skipToNextLine rest whereCol  -- Skip blank line
        _ -> rest  -- Continue with indented content
    _ -> skipToNextLine (Array.drop 1 toks) whereCol

