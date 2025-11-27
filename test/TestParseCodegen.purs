module TestParseCodegen where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array (length, take, last)
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Nova.Compiler.Tokenizer (tokenize, Token)
import Nova.Compiler.Parser (parseModule, parseModuleHeader, parseDeclarations, skipNewlines)
import Nova.Compiler.Ast as Ast

main :: Effect Unit
main = do
  log "Testing CodeGen.purs parsing..."
  content <- readTextFile UTF8 "src/Nova/Compiler/CodeGen.purs"
  let tokens = tokenize content
  log $ "Tokenized: " <> show (length tokens) <> " tokens"
  -- Try parsing the module header
  case parseModuleHeader tokens of
    Left e -> log $ "Header parse error: " <> e
    Right (Tuple header rest1) -> do
      log $ "Header parsed, " <> show (length rest1) <> " tokens remaining"
      -- Try parsing declarations
      case parseDeclarations rest1 of
        Left e -> log $ "Declarations parse error: " <> e
        Right (Tuple decls rest2) -> do
          log $ "Parsed " <> show (length decls) <> " declarations"
          -- Log last parsed declaration name
          case getLastDeclName decls of
            Just n -> log $ "Last parsed declaration: " <> n
            Nothing -> log "No declarations"
          let rest3 = skipNewlines rest2
          log $ "After skipNewlines: " <> show (length rest3) <> " tokens remaining"
          -- Show the first few remaining tokens
          case take 10 rest3 of
            toks -> do
              log $ "First remaining tokens:"
              log $ showTokens toks

getLastDeclName :: Array Ast.Declaration -> Maybe String
getLastDeclName decls = case last decls of
  Just (Ast.DeclFunction f) -> Just f.name
  Just (Ast.DeclTypeSig s) -> Just s.name
  _ -> Nothing

showTokens :: Array Token -> String
showTokens toks = show (map (\t -> "(" <> t.value <> ":" <> show t.line <> ")") toks)
