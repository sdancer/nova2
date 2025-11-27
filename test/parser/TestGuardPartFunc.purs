module Test.Parser.TestGuardPartFunc where

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
  log "=== parseGuardPart function test ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let allTokens = tokenize content
  
  -- Skip to declaration 83 (parseGuardParts) and then try to parse what follows
  parseAllDecls allTokens 1

parseAllDecls :: Array Token -> Int -> Effect Unit
parseAllDecls tokens n = do
  let tokens' = P.skipNewlines tokens
  case Array.head tokens' of
    Nothing -> log "Done"
    _ -> case P.parseDeclaration tokens' of
      Right (Tuple decl rest) -> do
        if n >= 82 && n <= 86 then do
          log $ show n <> ": ✓ " <> showDecl decl
          let rest' = P.skipNewlines rest
          log $ "   Rest first tokens: " <> show (map showTok (Array.take 5 rest'))
          case n of
            83 -> do
              -- Try parseFunctionWithTypeSignature on the rest
              log ""
              log "   Trying parseFunctionWithTypeSignature on rest:"
              case P.parseFunctionWithTypeSignature rest' of
                Right (Tuple decl2 rest2) -> log $ "   ✓ " <> showDecl decl2
                Left err -> log $ "   ✗ " <> err
            _ -> pure unit
        else pure unit
        if n < 90 then
          parseAllDecls rest (n + 1)
        else 
          log "Stopping"
      Left err -> do
        log $ show n <> ": ✗ " <> err
        log $ "   At: " <> show (map showTok (Array.take 15 tokens'))

showDecl :: Ast.Declaration -> String
showDecl (Ast.DeclModule m) = "module " <> m.name
showDecl (Ast.DeclImport i) = "import " <> i.moduleName
showDecl (Ast.DeclFunction f) = "function " <> f.name <> 
  case f.typeSignature of
    Just _ -> " (with type sig)"
    Nothing -> " (no type sig)"
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
