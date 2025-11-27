module Test.Parser.TestLineCount where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Data.String as String

main :: Effect Unit
main = do
  log "=== Line Count Test ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let lines = String.split (String.Pattern "\n") content
  log $ "Total lines in file: " <> show (Array.length lines)
  
  let tokens = tokenize content
  log $ "Total tokens: " <> show (Array.length tokens)
  
  -- Find max line number in tokens
  let maxLine = Array.foldl (\acc t -> if t.line > acc then t.line else acc) 0 tokens
  log $ "Max line number in tokens: " <> show maxLine
  
  -- Look for specific line content
  log ""
  log "Line 1221 from file:"
  case Array.index lines 1220 of
    Just l -> log $ "  " <> l
    Nothing -> log "  (not found)"
  
  log "Line 1334 from file:"
  case Array.index lines 1333 of
    Just l -> log $ "  " <> l
    Nothing -> log "  (not found)"

