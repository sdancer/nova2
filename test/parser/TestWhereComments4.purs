module Test.Parser.TestWhereComments4 where

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
  log "=== Where Comments Test 4 ==="
  
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
  
  -- Skip to after = 
  let afterEq = Array.dropWhile (\t -> not (t.tokenType == TokOperator && t.value == "=")) tokens
  let bodyTokens = Array.drop 1 afterEq -- drop the =
  
  log $ "Body tokens start:"
  void $ Array.foldl (\acc t -> acc *> log ("  " <> t.value <> " L" <> show t.line <> "C" <> show t.column)) (pure unit) (Array.take 10 bodyTokens)
  
  log ""
  log "parseExpression:"
  case P.parseExpression bodyTokens of
    Right (Tuple expr rest) -> do
      log "✓ parsed"
      log $ "Rest after expression:"
      void $ Array.foldl (\acc t -> acc *> log ("  " <> t.value <> " L" <> show t.line <> "C" <> show t.column)) (pure unit) (Array.take 10 rest)
    Left err -> log $ "✗ " <> err

