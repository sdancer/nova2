module Test.Parser.TestRealFile where

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
  log "=== Real File Test ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let tokens = tokenize content
  
  -- Skip to declaration 117
  parseToN tokens 1 117

parseToN :: Array Token -> Int -> Int -> Effect Unit
parseToN tokens n target = do
  let tokens' = P.skipNewlines tokens
  case Array.head tokens' of
    Nothing -> log "End of file"
    _ -> case P.parseDeclaration tokens' of
      Right (Tuple decl rest) -> do
        if n >= target then do
          log $ show n <> ". OK"
          let rest' = P.skipNewlines rest
          log $ "  Next 15 tokens:"
          void $ Array.foldl (\acc t -> acc *> log ("    L" <> show t.line <> "C" <> show t.column <> " " <> show t.value)) (pure unit) (Array.take 15 rest')
          if n < target + 3 then parseToN rest (n + 1) target
          else pure unit
        else parseToN rest (n + 1) target
      Left err -> log $ show n <> ". FAILED: " <> err

