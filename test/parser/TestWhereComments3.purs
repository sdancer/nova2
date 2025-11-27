module Test.Parser.TestWhereComments3 where

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
  log "=== Where Comments Test 3 ==="
  
  let input = """splitTypeAndRest tokens name = go tokens []
  where
    go :: Array Token -> Array Token -> Tuple (Array Token) (Array Token)
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
  log $ "Tokens: " <> show (Array.length tokens)
  
  -- Manual tracing
  log ""
  log "Manual trace:"
  
  -- Parse name
  case P.parseIdentifierName tokens of
    Right (Tuple name rest) -> do
      log $ "Name: " <> name
      
      -- Parse params
      case P.parseMany P.parseSimplePattern rest of
        Right (Tuple params rest') -> do
          log $ "Params: " <> show (Array.length params)
          log $ "Next tokens after params:"
          void $ Array.foldl (\acc t -> acc *> log ("  " <> t.value <> " L" <> show t.line <> "C" <> show t.column)) (pure unit) (Array.take 5 rest')
        Left err -> log $ "Params failed: " <> err
    Left err -> log $ "Name failed: " <> err

