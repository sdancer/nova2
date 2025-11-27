module Test.Parser.TestImportAs where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Import 'as' Test ==="
  
  let input = "import Data.Array as Array"
  
  let tokens = tokenize input
  log $ "Tokens: " <> show (map showTok tokens)
  
  log ""
  log "parseImport:"
  case P.parseDeclaration tokens of
    Right (Tuple decl rest) -> do
      log "✓ parsed"
      log $ "Rest: " <> show (map showTok (Array.take 5 rest))
    Left err -> log $ "✗ " <> err

showTok :: Token -> String
showTok t = t.value <> ":L" <> show t.line <> ":" <> showTokType t.tokenType

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

