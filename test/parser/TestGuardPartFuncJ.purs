module Test.Parser.TestGuardPartFuncJ where

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
  log "=== parseGuardPart function test J ==="
  
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
        if n == 84 then do
          log $ show n <> ": ✓ " <> showDecl decl
          let rest' = P.skipNewlines rest
          
          case Array.head rest' of
            Just t | t.tokenType == TokIdentifier -> do
              case P.expectOperator (Array.drop 1 rest') "::" of
                Right (Tuple _ rest2) -> do
                  let Tuple _ rest3 = P.splitTypeAndRest rest2 t.value
                  log $ "rest3: " <> show (map showTok (Array.take 10 rest3))
                  
                  log ""
                  log "Step 1: parseIdentifierName on rest3"
                  case P.parseIdentifierName rest3 of
                    Right (Tuple name1 rest4) -> do
                      log $ "  ✓ name1 = " <> name1
                      log $ "  rest4: " <> show (map showTok (Array.take 5 rest4))
                      
                      log "Step 2: parseMany parseSimplePattern on rest4"
                      case P.parseMany P.parseSimplePattern rest4 of
                        Right (Tuple params rest5) -> do
                          log $ "  ✓ params = " <> show (Array.length params)
                          log $ "  rest5: " <> show (map showTok (Array.take 5 rest5))
                          
                          log "Step 3: expectOperator = on rest5"
                          case P.expectOperator rest5 "=" of
                            Right _ -> log "  ✓ found ="
                            Left err -> log $ "  ✗ " <> err
                        Left err -> log $ "  ✗ " <> err
                    Left err -> log $ "  ✗ " <> err
                Left err -> log $ "expectOperator: " <> err
            _ -> log "Not identifier"
        else pure unit
        if n < 100 then
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
