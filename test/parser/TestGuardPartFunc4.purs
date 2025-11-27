module Test.Parser.TestGuardPartFunc4 where

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
  log "=== parseGuardPart function test 4 ==="
  
  content <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  let allTokens = tokenize content
  
  parseAllDecls allTokens 1

parseAllDecls :: Array Token -> Int -> Effect Unit
parseAllDecls tokens n = do
  let tokens' = P.skipNewlines tokens
  case Array.head tokens' of
    Nothing -> log "Done"
    _ -> case P.parseDeclaration tokens' of
      Right (Tuple decl rest) -> do
        if n == 83 then do
          log $ show n <> ": ✓ " <> showDecl decl
          
          -- Manually trace the full parseFunctionWithTypeSignature logic
          let toks = P.dropNewlines rest
          log $ "Trying parseFunctionWithTypeSignature manually..."
          case Array.head toks of
            Just t | t.tokenType == TokIdentifier -> do
              log $ "✓ Got identifier: " <> t.value
              let name = t.value
              log $ "Trying expectOperator :: ..."
              case P.expectOperator (Array.drop 1 toks) "::" of
                Right (Tuple _ rest2) -> do
                  log "✓ Got ::"
                  log $ "Rest2: " <> show (map showTok (Array.take 10 rest2))
                  -- Try splitTypeAndRest
                  log $ "Trying splitTypeAndRest for name: " <> name
                  let Tuple typeTokens rest3 = P.splitTypeAndRest rest2 name
                  log $ "typeTokens: " <> show (map showTok typeTokens)
                  log $ "rest3 (should be function definition): " <> show (map showTok (Array.take 10 rest3))
                Left err -> log $ "✗ expectOperator failed: " <> err
            Just t -> log $ "✗ First token not identifier: " <> showTokType t.tokenType
            Nothing -> log "✗ No tokens"
        else pure unit
        if n < 90 then
          parseAllDecls rest (n + 1)
        else 
          log "Stopping"
      Left err -> do
        log $ show n <> ": ✗ " <> err

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
