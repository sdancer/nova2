module Test.Parser.DebugParse where

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
  log "=== Debug Parse ==="

  -- Test the standalone function that supposedly works
  log "Test standalone tTuple function:"
  let testInput = """tTuple ts = TyCon { name: "Tuple" <> show (length ts), args: ts }
  where
    length arr = 0"""
  let testTokens = tokenize testInput
  log $ "Total tokens: " <> show (Array.length testTokens)
  log "With parseFunctionDeclaration:"
  case P.parseFunctionDeclaration testTokens of
    Right _ -> log "  ✓ Parsed OK"
    Left err -> log $ "  ✗ Error: " <> err
  log "With parseDeclaration:"
  case P.parseDeclaration testTokens of
    Right _ -> log "  ✓ Parsed OK"
    Left err -> log $ "  ✗ Error: " <> err
  log ""

  log "--- Types.purs ---"
  typesContent <- readTextFile UTF8 "src/Nova/Compiler/Types.purs"
  parseFile "Types.purs" (tokenize typesContent)

  log "\n--- Ast.purs ---"
  astContent <- readTextFile UTF8 "src/Nova/Compiler/Ast.purs"
  parseFile "Ast.purs" (tokenize astContent)

  log "\n--- Tokenizer.purs ---"
  tokContent <- readTextFile UTF8 "src/Nova/Compiler/Tokenizer.purs"
  parseFile "Tokenizer.purs" (tokenize tokContent)

  log "\n--- Parser.purs ---"
  parserContent <- readTextFile UTF8 "src/Nova/Compiler/Parser.purs"
  parseFile "Parser.purs" (tokenize parserContent)

parseFile :: String -> Array Token -> Effect Unit
parseFile name tokens = do
  let result = parseAllDeclsQuiet tokens 0
  case result of
    Right count -> log $ "  ✓ " <> name <> ": " <> show count <> " declarations parsed"
    Left { count, err, tokens: toks } -> do
      log $ "  ✗ " <> name <> ": Failed at declaration " <> show (count + 1)
      log $ "    Error: " <> err
      let relevant = Array.take 15 $ Array.filter (\t -> t.tokenType /= TokNewline) toks
      log $ "    At: " <> show (map _.value relevant)

parseAllDeclsQuiet :: Array Token -> Int -> Either { count :: Int, err :: String, tokens :: Array Token } Int
parseAllDeclsQuiet tokens count = do
  let tokens' = P.skipNewlines tokens
  case Array.head tokens' of
    Nothing -> Right count
    _ -> case P.parseDeclaration tokens' of
      Right (Tuple _ rest) -> parseAllDeclsQuiet rest (count + 1)
      Left err -> Left { count, err, tokens: tokens' }

parseAllDecls :: Array Token -> Int -> Effect Unit
parseAllDecls tokens n = do
  let tokens' = P.skipNewlines tokens
  case Array.head tokens' of
    Nothing -> log "Done - all tokens consumed"
    _ -> case P.parseDeclaration tokens' of
      Right (Tuple decl rest) -> do
        log $ show n <> ": ✓ " <> showDecl decl
        parseAllDecls rest (n + 1)
      Left err -> do
        log $ show n <> ": ✗ " <> err
        let relevant = Array.take 15 $ Array.filter (\t -> t.tokenType /= TokNewline) tokens'
        log $ "   At: " <> show (map _.value relevant)
        -- Show actual tokens with types
        log $ "   Actual first 30: " <> show (map (\t -> { v: t.value, col: t.column }) (Array.take 30 tokens'))
        -- Try parseFunctionDeclaration directly
        case P.parseFunctionDeclarationRaw tokens' of
          Right (Tuple _ _) -> log "   parseFunctionDeclarationRaw WOULD work"
          Left err2 -> do
            log $ "   parseFunctionDeclarationRaw: " <> err2
            -- Check if parseDeclaration tried other parsers that consumed tokens
            log "   Trying parseFunctionWithTypeSignature..."
            case P.parseFunctionWithTypeSignature tokens' of
              Right _ -> log "   parseFunctionWithTypeSignature succeeded"
              Left err3 -> log $ "   parseFunctionWithTypeSignature: " <> err3
            -- Create a small reproduction - binary search to find the problematic token count
            let test n = do
                  let repro = Array.take n tokens'
                  case P.parseFunctionDeclarationRaw repro of
                    Right _ -> log $ "   n=" <> show n <> ": ✓"
                    Left err3 -> log $ "   n=" <> show n <> ": ✗ " <> err3
            log $ "   Binary search for problematic token count:"
            test 20
            test 21
            test 22
            test 23
            test 24
            test 25
            -- Show tokens 20-25
            log $ "   Tokens 20-25: " <> show (map (\t -> { v: t.value, col: t.column }) (Array.slice 19 25 tokens'))
            -- Trace with exactly 23 tokens
            let tokens23 = Array.take 23 tokens'
            log "   Detailed trace with 23 tokens:"
            -- Do EXACTLY what parseFunctionDeclarationRaw does
            case do
              Tuple name rest <- P.parseIdentifierName tokens23
              Tuple params rest' <- P.parseMany P.parseSimplePattern rest
              Tuple _ rest'' <- P.expectOperator rest' "="
              pure rest''
            of
              Right rest'' -> log $ "   Manual do-block: ✓ got past =, remaining: " <> show (Array.length rest'')
              Left err3 -> log $ "   Manual do-block: ✗ " <> err3
            -- Compare with actual function
            case P.parseFunctionDeclarationRaw tokens23 of
              Right _ -> log "   parseFunctionDeclarationRaw: ✓"
              Left err3 -> log $ "   parseFunctionDeclarationRaw: ✗ " <> err3

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
