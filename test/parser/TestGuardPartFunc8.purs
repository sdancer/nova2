module Test.Parser.TestGuardPartFunc8 where

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
  log "=== parseGuardPart function test 8 ==="
  
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
          
          let toks = P.dropNewlines rest
          let Tuple _ funcToks = P.splitTypeAndRest (Array.drop 2 toks) "parseGuardPart"
          
          case P.parseIdentifierName funcToks of
            Right (Tuple _ rest1) -> do
              case P.parseMany P.parseSimplePattern rest1 of
                Right (Tuple _ rest2) -> do
                  case P.expectOperator rest2 "=" of
                    Right (Tuple _ rest3) -> do
                      case Array.head rest3 of
                        Just firstTok -> do
                          case P.parseExpression rest3 of
                            Right (Tuple body rest4) -> do
                              log $ "Expression parsed. rest4: " <> show (map showTok (Array.take 5 rest4))
                              log $ "firstTok.column = " <> show firstTok.column
                              
                              log "Step 5: maybeParseWhere"
                              case P.maybeParseWhere rest4 firstTok.column body of
                                Right (Tuple _ rest5) -> do
                                  log "  ✓ where parsed"
                                  log $ "  rest5: " <> show (map showTok (Array.take 10 rest5))
                                Left err -> log $ "  ✗ maybeParseWhere: " <> err
                            Left err -> log $ "parseExpression: " <> err
                        Nothing -> log "No firstTok"
                    Left err -> log $ "expectOperator: " <> err
                Left err -> log $ "parseMany: " <> err
            Left err -> log $ "parseIdentifierName: " <> err
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
