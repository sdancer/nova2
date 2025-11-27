module Test.Parser.TestParserModule where

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
  log "=== Parser.purs Declarations Test ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let tokens = tokenize content
  
  -- First parse module header
  case P.parseDeclaration tokens of
    Right (Tuple _ rest) -> do
      log "Module header OK"
      parseAllDecls (P.skipNewlines rest) 1
    Left err -> log $ "Module header failed: " <> err

parseAllDecls :: Array Token -> Int -> Effect Unit
parseAllDecls tokens n = do
  let tokens' = P.skipNewlines tokens
  case Array.head tokens' of
    Nothing -> log $ "\nDone! All " <> show (n-1) <> " declarations parsed."
    _ -> case P.parseDeclaration tokens' of
      Right (Tuple decl rest) -> do
        when (n <= 5 || n > 80) $ log $ show n <> ". OK"
        parseAllDecls rest (n + 1)
      Left err -> do
        log $ show n <> ". FAILED: " <> err
        log "Tokens at failure:"
        logTokens (Array.take 10 tokens')

logTokens :: Array Token -> Effect Unit
logTokens tokens = do
  let fmt t = t.value <> ":" <> show t.line <> ":" <> showTokType t.tokenType
  log $ "  " <> show (map fmt tokens)

showTokType :: TokenType -> String
showTokType TokKeyword = "K"
showTokType TokIdentifier = "I"
showTokType TokOperator = "O"
showTokType TokDelimiter = "D"
showTokType TokNewline = "NL"
showTokType TokNumber = "N"
showTokType TokString = "S"
showTokType TokChar = "C"
showTokType TokUnrecognized = "?"

