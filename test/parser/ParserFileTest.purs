module Test.Parser.ParserFileTest where

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
import Nova.Compiler.Ast as Ast

main :: Effect Unit
main = do
  log "=== Parser.purs Trace ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let allTokens = tokenize content
  log $ "Total tokens: " <> show (Array.length allTokens)
  
  -- Parse declarations one by one
  parseAllDecls allTokens 1
  
parseAllDecls :: Array Token -> Int -> Effect Unit
parseAllDecls tokens n = do
  let tokens' = P.skipNewlines tokens
  case Array.head tokens' of
    Nothing -> log "Done"
    _ -> case P.parseDeclaration tokens' of
      Right (Tuple decl rest) -> do
        log $ show n <> ": ✓ " <> showDecl decl
        if n >= 1 && n <= 15 then do
          let rest' = P.skipNewlines rest
          log $ "   Remaining first 10: " <> show (map showTok (Array.take 10 rest'))
        else pure unit
        parseAllDecls rest (n + 1)
      Left err -> do
        log $ show n <> ": ✗ " <> err
        log $ "   At: " <> show (map showTok (Array.take 20 tokens'))

showDecl :: Ast.Declaration -> String
showDecl (Ast.DeclModule m) = "module " <> m.name
showDecl (Ast.DeclImport i) = "import " <> i.moduleName <> case i.alias of
  Just a -> " as " <> a
  Nothing -> ""
showDecl (Ast.DeclFunction f) = "function " <> f.name
showDecl (Ast.DeclDataType d) = "data " <> d.name
showDecl (Ast.DeclTypeAlias a) = "type " <> a.name
showDecl (Ast.DeclTypeClass c) = "class " <> c.name
showDecl (Ast.DeclTypeClassInstance _) = "instance"
showDecl (Ast.DeclForeignImport f) = "foreign " <> f.functionName
showDecl (Ast.DeclTypeSig s) = "sig " <> s.name
showDecl (Ast.DeclType t) = "type decl " <> t.name

showTok :: Token -> String
showTok t = t.value <> "@L" <> show t.line <> "C" <> show t.column <> ":" <> showTokType t.tokenType

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
