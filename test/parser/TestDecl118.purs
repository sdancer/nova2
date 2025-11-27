module Test.Parser.TestDecl118 where

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
  log "=== Declaration 118 Test ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let tokens = tokenize content
  
  -- Parse through declarations to find 118
  parseN tokens 1

parseN :: Array Token -> Int -> Effect Unit
parseN tokens n = do
  let tokens' = P.skipNewlines tokens
  case Array.head tokens' of
    Nothing -> log $ "Done - total " <> show (n-1)
    _ -> case P.parseDeclaration tokens' of
      Right (Tuple decl rest) -> do
        when (n >= 116) $ do
          log $ show n <> ". OK"
          -- Show first few tokens of next
          let rest' = P.skipNewlines rest
          log $ "  Next tokens: " <> show (map showTok (Array.take 5 rest'))
        parseN rest (n + 1)
      Left err -> do
        log $ show n <> ". FAILED: " <> err
        log $ "Tokens at failure:"
        log $ show (map showTok (Array.take 10 tokens'))

showTok :: Token -> String
showTok t = t.value <> ":" <> showTokType t.tokenType

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

