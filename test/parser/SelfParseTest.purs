module Test.Parser.SelfParseTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P

main :: Effect Unit
main = do
  log "=== Self-Parse Tests ==="
  log ""

  -- Test parsing each source file
  testFile "Types.purs" "src/Nova/Compiler/Types.purs"
  testFile "Ast.purs" "src/Nova/Compiler/Ast.purs"
  testFile "Tokenizer.purs" "src/Nova/Compiler/Tokenizer.purs"
  testFile "Parser.purs" "src/Nova/Compiler/Parser.purs"

  log ""
  log "=== Tests Complete ==="

testFile :: String -> String -> Effect Unit
testFile name path = do
  content <- readTextFile UTF8 path
  let tokens = tokenize content
  let result = P.parseModule tokens
  case result of
    Right (Tuple m rest) -> do
      let restNoNewlines = Array.filter (\t -> t.tokenType /= TokNewline) rest
      if Array.length restNoNewlines == 0 then do
        log $ "✓ " <> name
        log $ "  Module: " <> m.name
        log $ "  Declarations: " <> show (Array.length m.declarations)
      else do
        log $ "✗ " <> name <> " (leftover tokens)"
        log $ "  Module: " <> m.name
        log $ "  Declarations: " <> show (Array.length m.declarations)
        log $ "  Remaining tokens: " <> show (Array.length restNoNewlines)
        showTokens "  First remaining:" (Array.take 10 restNoNewlines)
    Left err -> do
      log $ "✗ " <> name
      log $ "  Error: " <> err
      -- Try parsing declarations directly after module header
      case P.parseDeclaration tokens of
        Right (Tuple _ rest) -> do
          log $ "  First decl parsed, then trying second..."
          case P.parseDeclaration (P.skipNewlines rest) of
            Right (Tuple _ rest2) -> do
              log $ "  Second decl OK, checking third..."
              showTokens "  Next tokens:" (Array.take 5 (P.skipNewlines rest2))
            Left err2 -> do
              log $ "  Second decl failed: " <> err2
              showTokens "  At tokens:" (Array.take 5 (P.skipNewlines rest))
        Left err1 -> log $ "  First decl failed: " <> err1

showTokens :: String -> Array Token -> Effect Unit
showTokens prefix tokens = do
  log prefix
  traverse_ showToken tokens
  where
    showToken t = log $ "    " <> showTokType t.tokenType <> " " <> show t.value <> " (line " <> show t.line <> ")"

showTokType :: TokenType -> String
showTokType TokKeyword = "KW"
showTokType TokIdentifier = "ID"
showTokType TokNumber = "NUM"
showTokType TokString = "STR"
showTokType TokChar = "CHR"
showTokType TokOperator = "OP"
showTokType TokDelimiter = "DL"
showTokType TokNewline = "NL"
showTokType TokUnrecognized = "??"
