module Test.Parser.TestParseFuncBody where

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
  log "=== Parse Function Body Test ==="
  
  -- This is what's left after the type signature is parsed
  let input = """parseGuardExpression tokens = do
  Tuple guards rest <- parseGuardParts tokens []
  case Array.length guards of
    0 -> failure "Expected guard expression"
    1 -> case Array.head guards of
      Just g -> success g rest
      Nothing -> failure "No guard"
    _ -> success (foldGuards guards) rest
  where
    foldGuards gs = case Array.uncons gs of
      Just { head, tail } -> Array.foldl foo head tail
      Nothing -> Ast.ExprLit (Ast.LitBool true)"""
  
  log $ "Input:\n" <> input
  log ""
  
  let tokens = tokenize input
  log $ "First 30 tokens: " <> show (map showTok (Array.take 30 tokens))
  
  log ""
  log "Trying parseFunctionDeclaration:"
  case P.parseDeclaration tokens of
    Right (Tuple decl rest) -> do
      log $ "✓ " <> showDecl decl
      let rest' = P.skipNewlines rest
      log $ "Rest: " <> show (map showTok (Array.take 10 rest'))
    Left err -> log $ "✗ " <> err

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
