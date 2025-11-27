module Test.Parser.TestDecl119 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Declaration 119 Test ==="
  
  -- Exact input that would be at decl 119 position
  let input = """splitTypeAndRest :: Array Token -> String -> Tuple (Array Token) (Array Token)
splitTypeAndRest tokens name = go tokens []
  where
    go :: Array Token -> Array Token -> Tuple (Array Token) (Array Token)
    go toks acc = Tuple acc toks"""
  let tokens = tokenize input
  log $ "Tokens: " <> show (Array.length tokens)
  log $ "First few: " <> show (map showTok (Array.take 5 tokens))
  
  log ""
  log "parseDeclaration:"
  case P.parseDeclaration tokens of
    Right (Tuple decl rest) -> do
      log "✓ parsed"
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (Array.length rest') <> " tokens"
      log $ "Rest first few: " <> show (map showTok (Array.take 10 rest'))
    Left err -> log $ "✗ " <> err

showTok :: Token -> String
showTok t = t.value <> ":L" <> show t.line <> "C" <> show t.column <> ":" <> showTokType t.tokenType

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

