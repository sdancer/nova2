module Test.Parser.DeriveInstanceTest where

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
  log "=== Derive Instance Test ==="

  -- Test 1: Simple derive instance
  let input1 = "derive instance eqTokenType :: Eq TokenType"
  log $ "Input 1: " <> input1
  testParse input1

  -- Test 2: Actual Tokenizer.purs content
  log "\nLoading actual Tokenizer.purs file..."
  content <- readTextFile UTF8 "src/Nova/Compiler/Tokenizer.purs"
  let tokens2 = tokenize content
  log $ "Total tokens: " <> show (Array.length tokens2)

  -- Extract just the derive instance tokens
  log "\nTrying with extracted tokens..."
  let deriveTokens = Array.take 6 (P.skipNewlines (Array.drop (countTokensUntilDerive tokens2 0) tokens2))
  log $ "Derive tokens: " <> show (map (\t -> { v: t.value, col: t.column, typ: showType t.tokenType }) deriveTokens)
  case P.parseTypeClassInstance deriveTokens of
    Right _ -> log "  ✓ parseTypeClassInstance succeeded on extracted tokens"
    Left err -> log $ "  ✗ parseTypeClassInstance failed: " <> err

  -- Parse all declarations to find where it fails
  log "\nParsing all declarations..."
  parseAllDecls tokens2 1

countTokensUntilDerive :: Array Token -> Int -> Int
countTokensUntilDerive toks n = case Array.head toks of
  Just t | t.value == "derive" -> n
  Just _ -> countTokensUntilDerive (Array.drop 1 toks) (n + 1)
  Nothing -> n

parseAllDecls :: Array Token -> Int -> Effect Unit
parseAllDecls toks n = do
  let toks' = P.skipNewlines toks
  case Array.head toks' of
    Nothing -> log "Done - all tokens consumed"
    _ -> case P.parseDeclaration toks' of
      Right (Tuple decl rest) -> do
        log $ show n <> ": ✓ " <> showDecl decl
        parseAllDecls rest (n + 1)
      Left err -> do
        log $ show n <> ": ✗ " <> err
        log $ "   Tokens: " <> show (map (\t -> { v: t.value, col: t.column }) (Array.take 15 toks'))
        -- Try each parser individually
        log "   Trying parseTypeClassInstance directly..."
        case P.parseTypeClassInstance toks' of
          Right _ -> log "     ✓ parseTypeClassInstance succeeded"
          Left err2 -> do
            log $ "     ✗ parseTypeClassInstance: " <> err2
            -- Try parseType on the type portion
            let typeTokens = Array.drop 4 toks' -- Skip "derive instance eqTokenType ::"
            log $ "     Type tokens: " <> show (map (\t -> { v: t.value, col: t.column }) (Array.take 10 typeTokens))
            case P.parseType typeTokens of
              Right (Tuple ty rest) -> do
                log $ "     ✓ parseType succeeded"
                log $ "     Remaining: " <> show (Array.length rest)
                log $ "     Remaining tokens: " <> show (map (\t -> { v: t.value, col: t.column }) (Array.take 10 rest))
              Left err3 -> log $ "     ✗ parseType: " <> err3

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

testParse :: String -> Effect Unit
testParse input = do
  let tokens = tokenize input
  log $ "Tokens: " <> show (map (\t -> { v: t.value, typ: showType t.tokenType }) tokens)
  case P.parseDeclaration tokens of
    Right (Tuple _ rest) -> log $ "  ✓ Parsed, remaining: " <> show (Array.length rest)
    Left err -> log $ "  ✗ Error: " <> err

showType :: TokenType -> String
showType TokKeyword = "K"
showType TokIdentifier = "I"
showType TokOperator = "O"
showType TokNumber = "N"
showType TokString = "S"
showType TokChar = "C"
showType TokDelimiter = "D"
showType TokNewline = "NL"
showType TokUnrecognized = "?"
