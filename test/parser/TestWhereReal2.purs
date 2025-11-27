module Test.Parser.TestWhereReal2 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast as Ast

main :: Effect Unit
main = do
  log "=== Where Real File Test 2 ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let allTokens = tokenize content
  
  -- Find where clause for splitTypeAndRest (line 1498 is 'where')
  let tokens = Array.dropWhile (\t -> not (t.value == "where" && t.line == 1498)) allTokens
  
  log $ "First 20 tokens after 'where':"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> t.value)) (pure unit) (Array.take 20 tokens)
  
  log ""
  log "maybeParseWhere:"
  case P.maybeParseWhere tokens 1 (Ast.ExprVar "x") of
    Right (Tuple body rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
      log $ "Rest first 5:"
      void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> t.value)) (pure unit) (Array.take 5 rest')
    Left err -> log $ "✗ " <> err

