module Test.Parser.TypeSigFuncTest2 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Nova.Compiler.Tokenizer (tokenize, Token, TokenType(..))
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast as Ast

main :: Effect Unit
main = do
  log "=== Type Sig + Function with Comment Test ==="
  
  -- Test case: function, then comment, then type sig + function
  let input = """maybeParseGuard tokens =
  case Array.head tokens of
    _ -> Tuple Nothing tokens

-- | Parse guard expressions
parseGuardExpression :: Array Token -> ParseResult Ast.Expr
parseGuardExpression tokens = do
  pure unit"""
  log $ "Input: " <> input
  log ""
  let tokens = tokenize input
  log $ "First 20 tokens: " <> show (map showTok (Array.take 20 tokens))
  
  log ""
  log "Parsing declarations:"
  parseAllDecls tokens 1

parseAllDecls :: Array Token -> Int -> Effect Unit
parseAllDecls tokens n = do
  let tokens' = P.skipNewlines tokens
  case Array.head tokens' of
    Nothing -> log "Done"
    _ -> case P.parseDeclaration tokens' of
      Right (Tuple decl rest) -> do
        log $ show n <> ": ✓ " <> showDecl decl
        let rest' = P.skipNewlines rest
        log $ "   Rest starts with: " <> show (map showTok (Array.take 10 rest'))
        parseAllDecls rest (n + 1)
      Left err -> do
        log $ show n <> ": ✗ " <> err
        log $ "   At: " <> show (map showTok (Array.take 20 tokens'))

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
