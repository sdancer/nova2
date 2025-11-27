module Test.Parser.TestFindWhere where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array as Array
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))

main :: Effect Unit
main = do
  log "=== Find Where Test ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let allTokens = tokenize content
  
  -- Find splitTypeAndRest at column 1
  let splitTokens = Array.filter (\t -> t.value == "splitTypeAndRest" && t.column == 1) allTokens
  
  log $ "Found " <> show (Array.length splitTokens) <> " splitTypeAndRest at column 1"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line)) (pure unit) splitTokens
  
  -- Find 'where' tokens after line 1490
  let whereTokens = Array.filter (\t -> t.value == "where" && t.line > 1490 && t.line < 1510) allTokens
  log ""
  log $ "Found " <> show (Array.length whereTokens) <> " 'where' near line 1500"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column)) (pure unit) whereTokens

