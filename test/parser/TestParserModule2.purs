module Test.Parser.TestParserModule2 where

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
  log "=== Parser.purs Line Check ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let tokens = tokenize content
  
  -- Find skipSuperclassConstraints token
  let matchingTokens = Array.filter (\t -> t.value == "skipSuperclassConstraints") tokens
  log $ "Found " <> show (Array.length matchingTokens) <> " occurrences"
  
  log "Occurrences:"
  flip traverse_ matchingTokens $ \t -> do
    log $ "  Line " <> show t.line <> ", Col " <> show t.column <> ": " <> t.value

traverse_ :: forall a m. Applicative m => (a -> m Unit) -> Array a -> m Unit
traverse_ f arr = case Array.head arr of
  Nothing -> pure unit
  Just x -> f x *> traverse_ f (Array.drop 1 arr)

