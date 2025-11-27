module Test.Parser.TestWhereReal where

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
  log "=== Where Real File Test ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let allTokens = tokenize content
  
  -- Find where clause for splitTypeAndRest (line 1497 is 'where')
  let tokens = Array.dropWhile (\t -> not (t.value == "where" && t.line == 1497)) allTokens
  
  log $ "Found 'where' at:"
  case Array.head tokens of
    Just t -> log $ "  L" <> show t.line <> "C" <> show t.column <> " " <> t.value
    Nothing -> log "  Not found"
  
  log ""
  log "maybeParseWhere:"
  case P.maybeParseWhere tokens 1 (Ast.ExprVar "x") of
    Right (Tuple body rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
    Left err -> log $ "✗ " <> err

