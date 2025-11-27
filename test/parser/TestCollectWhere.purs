module Test.Parser.TestCollectWhere where

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
  log "=== Collect Where Test ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let allTokens = tokenize content
  
  -- Start at 'go' after 'where' (line 1499)
  let tokens = Array.dropWhile (\t -> not (t.value == "go" && t.line == 1499 && t.column == 5)) allTokens
  
  log $ "Starting tokens (where binding content):"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> t.value)) (pure unit) (Array.take 10 tokens)
  
  let whereCol = 3
  
  log ""
  log $ "whereCol = " <> show whereCol
  log $ "First token column = 5"
  log $ "5 > 3 = true, so should parse binding"
  
  log ""
  log "isTypeSignatureLine:"
  case Array.head tokens of
    Just t1 | t1.tokenType == TokIdentifier ->
      case Array.head (Array.drop 1 tokens) of
        Just t2 | t2.tokenType == TokOperator, t2.value == "::" -> log "Yes - it's a type sig"
        _ -> log "No - not a type sig"
    _ -> log "No - no identifier"
  
  log ""
  log "skipToNextLine:"
  let skipped = skipToNextLine tokens whereCol
  log $ "After skip:"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> t.value)) (pure unit) (Array.take 10 skipped)
  
  log ""
  log "parseFunctionDeclarationRaw on skipped:"
  case P.parseFunctionDeclarationRaw skipped of
    Right (Tuple fun rest) -> do
      log $ "✓ " <> fun.name
    Left err -> log $ "✗ " <> err

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

