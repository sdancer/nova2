module Test.Parser.TestTokenColumns where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array as Array
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))

main :: Effect Unit
main = do
  log "=== Token Columns Test ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let tokens = tokenize content
  
  -- Find all splitTypeAndRest tokens and show their columns
  let matching = Array.filter (\t -> t.value == "splitTypeAndRest") tokens
  log $ "Found " <> show (Array.length matching) <> " occurrences"
  
  log ""
  log "Details:"
  void $ Array.foldl (\acc t -> acc *> logTok t) (pure unit) matching

logTok :: Token -> Effect Unit
logTok t = log $ "  Line " <> show t.line <> ", Col " <> show t.column <> ": " <> t.value

