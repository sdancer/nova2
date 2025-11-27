module Test.Parser.TypeSigTest where

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
  log "=== Types.purs parseModule Trace ==="

  content <- readTextFile UTF8 "src/Nova/Compiler/Tokenizer.purs"
  let allTokens = tokenize content

  -- Test parseModule
  case P.parseModule allTokens of
    Right (Tuple m rest) -> do
      log $ "✓ parseModule succeeded"
      log $ "  Module: " <> m.name
      log $ "  Declarations: " <> show (Array.length m.declarations)
      log $ "  Remaining tokens: " <> show (Array.length rest)
    Left err -> do
      log $ "✗ parseModule failed: " <> err
      -- Try parseDeclarations directly after module header
      log "Trying parseDeclarations after header..."
      case P.parseModuleHeader allTokens of
        Right (Tuple _ rest) -> do
          log $ "  Module header parsed, rest starts with:"
          log $ "  " <> show (map showTokCol (Array.take 10 (P.skipNewlines rest)))
          case P.parseDeclarations rest of
            Right (Tuple decls rest') -> do
              log $ "  parseDeclarations returned " <> show (Array.length decls) <> " declarations"
              log $ "  Remaining: " <> show (map showTokCol (Array.take 10 (P.skipNewlines rest')))
            Left e -> log $ "  parseDeclarations failed: " <> e
        Left e -> log $ "  Module header failed: " <> e

  log ""
  log "All declarations one by one:"
  parseAllDecls allTokens 1

parseAllDecls :: Array Token -> Int -> Effect Unit
parseAllDecls tokens n = do
  let tokens' = P.skipNewlines tokens
  case Array.head tokens' of
    Nothing -> log "Done"
    _ -> case P.parseDeclaration tokens' of
      Right (Tuple decl rest) -> do
        log $ show n <> ": ✓ " <> showDecl decl
        if n >= 20 && n <= 25 then do
          let rest' = P.skipNewlines rest
          log $ "   After decl, first 10 tokens:"
          log $ "   " <> show (map showTokCol (Array.take 10 rest'))
        else pure unit
        parseAllDecls rest (n + 1)
      Left err -> do
        log $ show n <> ": ✗ " <> err
        log $ "   At: " <> show (map showTokCol (Array.take 15 tokens'))
        -- Try parsing the let expression directly
        log "   Trying parseExpression on tokens after '='..."
        let afterEq = Array.drop 3 tokens'  -- Skip "tokenizeString state ="
        log $ "   afterEq: " <> show (map showTokCol (Array.take 60 afterEq))
        case P.parseExpression afterEq of
          Right (Tuple expr rest) -> do
            log $ "   ✓ parseExpression succeeded"
            log $ "   remaining: " <> show (Array.length rest)
          Left e -> do
            log $ "   ✗ parseExpression: " <> e
            -- Try parsing just the bindings
            log "   Trying parseMany parseBinding..."
            let afterLet = Array.drop 2 afterEq  -- Skip newline and "let"
            log $ "   afterLet: " <> show (map showTokCol (Array.take 10 afterLet))
            case P.parseMany P.parseBinding afterLet of
              Right (Tuple bindings rest) -> do
                log $ "   ✓ parseMany parseBinding succeeded with " <> show (Array.length bindings) <> " bindings"
                log $ "   remaining: " <> show (map showTokCol (Array.take 10 rest))
              Left e2 -> log $ "   ✗ parseMany parseBinding: " <> e2

showDecl :: Ast.Declaration -> String
showDecl (Ast.DeclModule m) = "module " <> m.name
showDecl (Ast.DeclImport i) = "import " <> i.moduleName
showDecl (Ast.DeclFunction f) = "function " <> f.name
showDecl (Ast.DeclDataType d) = "data " <> d.name
showDecl (Ast.DeclTypeAlias a) = "type " <> a.name
showDecl (Ast.DeclTypeClass c) = "class " <> c.name
showDecl (Ast.DeclTypeClassInstance _) = "instance"
showDecl (Ast.DeclForeignImport f) = "foreign " <> f.functionName
showDecl (Ast.DeclTypeSig s) = "sig " <> s.name
showDecl (Ast.DeclType t) = "type decl " <> t.name

showTokCol :: Token -> String
showTokCol t = t.value <> "@" <> show t.column <> ":" <> showTokType t.tokenType

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
