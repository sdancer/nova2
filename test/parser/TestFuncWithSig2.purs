module Test.Parser.TestFuncWithSig2 where

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

main :: Effect Unit
main = do
  log "=== Function With Type Sig 2 ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let allTokens = tokenize content
  
  -- Extract tokens starting from splitTypeAndRest declaration (line 1495)
  let tokens = Array.dropWhile (\t -> not (t.value == "splitTypeAndRest" && t.line == 1495)) allTokens
  
  log $ "First 10 tokens:"
  void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> show t.value)) (pure unit) (Array.take 10 tokens)
  
  log ""
  log "parseFunctionWithTypeSignature:"
  case P.parseFunctionWithTypeSignature tokens of
    Right (Tuple decl rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
    Left err -> log $ "✗ " <> err
  
  log ""
  log "parseTypeSignatureDecl:"
  case P.parseTypeSignatureDecl tokens of
    Right (Tuple decl rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
      log $ "First few:"
      void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> show t.value)) (pure unit) (Array.take 5 rest')
    Left err -> log $ "✗ " <> err

