module Test.Parser.TestGuardPartFunc9 where

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
  log "=== parseGuardPart where binding test ==="
  
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
            Right (Tuple _ rest1) ->
              case P.parseMany P.parseSimplePattern rest1 of
                Right (Tuple _ rest2) ->
                  case P.expectOperator rest2 "=" of
                    Right (Tuple _ rest3) ->
                      case P.parseExpression rest3 of
                        Right (Tuple _ rest4) -> do
                          -- rest4 should start with "where"
                          log $ "After expression, rest4: " <> show (map showTok (Array.take 10 rest4))
                          let rest4' = P.skipNewlines rest4
                          case Array.head rest4' of
                            Just t | t.tokenType == TokKeyword, t.value == "where" -> do
                              log "Found where!"
                              let whereCol = t.column
                              log $ "whereCol = " <> show whereCol
                              let restAfterWhere = P.skipNewlines (Array.drop 1 rest4')
                              log $ "Rest after where: " <> show (map showTok (Array.take 10 restAfterWhere))
                              
                              -- Try parsing the first binding
                              log ""
                              log "Trying parseFunctionDeclarationRaw on first binding:"
                              case P.parseFunctionDeclarationRaw restAfterWhere of
                                Right (Tuple fun rest5) -> do
                                  log $ "✓ Got function: " <> fun.name
                                  log $ "Rest5: " <> show (map showTok (Array.take 5 rest5))
                                Left err -> log $ "✗ " <> err
                            _ -> log "No where found"
                        Left err -> log $ "parseExpression: " <> err
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
