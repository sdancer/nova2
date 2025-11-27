module Test.Parser.TestWhereComments5 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast as Ast

main :: Effect Unit
main = do
  log "=== Where Comments Test 5 ==="
  
  -- Just the where clause
  let input = """where
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
  
  log $ "Tokens count: " <> show (Array.length tokens)
  
  log ""
  log "maybeParseWhere with dummy body (ExprVar \"x\"):"
  case P.maybeParseWhere tokens 1 (Ast.ExprVar "x") of
    Right (Tuple body rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
    Left err -> log $ "✗ " <> err

