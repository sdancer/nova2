module Test.Parser.TestFuncWithSig3 where

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
  log "=== Function With Type Sig 3 ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let allTokens = tokenize content
  
  -- Extract tokens starting from splitTypeAndRest declaration (line 1495)
  let tokens = Array.dropWhile (\t -> not (t.value == "splitTypeAndRest" && t.line == 1495)) allTokens
  
  -- Manually trace parseFunctionWithTypeSignature
  let tokens' = P.skipNewlines tokens
  case Array.head tokens' of
    Just t | t.tokenType == TokIdentifier -> do
      let name = t.value
      log $ "Name: " <> name
      case P.expectOperator (Array.drop 1 tokens') "::" of
        Right (Tuple _ rest) -> do
          log "Found ::"
          let split = P.splitTypeAndRest rest name
          case split of
            Tuple typeTokens rest' -> do
              log $ "Type tokens: " <> show (Array.length typeTokens)
              log $ "Rest (function def) starts with:"
              void $ Array.foldl (\acc t -> acc *> log ("  L" <> show t.line <> "C" <> show t.column <> " " <> show t.value)) (pure unit) (Array.take 5 rest')
              
              log ""
              log "Trying to parse type:"
              case P.parseType (P.stripNewlines typeTokens) of
                Right (Tuple ty _) -> do
                  log "Type parsed OK"
                  log ""
                  log "Trying parseFunctionDeclarationRaw on rest:"
                  case P.parseFunctionDeclarationRaw rest' of
                    Right (Tuple fun rest'') -> do
                      log $ "✓ Function: " <> fun.name
                    Left err -> log $ "✗ " <> err
                Left err -> log $ "Type parse failed: " <> err
        Left err -> log $ "No '::': " <> err
    _ -> log "No identifier"

