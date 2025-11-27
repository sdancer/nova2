module Test.Parser.TestWhereBinding where

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
  log "=== Where Binding Test ==="
  
  -- Start after 'where'
  let input = """go :: Array Token -> Array Token -> Tuple (Array Token) (Array Token)
    go toks acc = case Array.head toks of
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
  
  log $ "First 15 tokens:"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> t.value)) (pure unit) (Array.take 15 tokens)
  
  log ""
  log "isTypeSignatureLine check:"
  let isTypeSig = case Array.head tokens of
        Just t1 | t1.tokenType == TokIdentifier ->
          case Array.head (Array.drop 1 tokens) of
            Just t2 | t2.tokenType == TokOperator, t2.value == "::" -> true
            _ -> false
        _ -> false
  log $ "Is type sig: " <> show isTypeSig
  
  log ""
  log "skipToNextLine:"
  let skipped = skipToNextLine tokens 1
  log $ "After skip, first 10 tokens:"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> t.value)) (pure unit) (Array.take 10 skipped)
  
  log ""
  log "parseFunctionDeclarationRaw on skipped:"
  case P.parseFunctionDeclarationRaw skipped of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> fun.name
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

