module Test.CodeGenElixir.CompileSelfTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.String as String
import Data.Traversable (traverse)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser (parseModule)
import Nova.Compiler.TypeChecker (checkModule)
import Nova.Compiler.Types (emptyEnv)
import Nova.Compiler.CodeGen (genModule)
import Nova.Compiler.Ast (Declaration)

main :: Effect Unit
main = do
  log "=== Compile to Elixir Test ==="
  log ""

  -- Modules with no dependencies
  compileFile "Unify" "src/Nova/Compiler/Unify.purs" []
  compileFile "Types" "src/Nova/Compiler/Types.purs" []
  compileFile "Ast" "src/Nova/Compiler/Ast.purs" []
  compileFile "Tokenizer" "src/Nova/Compiler/Tokenizer.purs" []
  compileFile "CodeGen" "src/Nova/Compiler/CodeGen.purs" []
  compileFile "TypeChecker" "src/Nova/Compiler/TypeChecker.purs" []

  -- Parser depends on Types, Ast, Tokenizer
  compileFile "Parser" "src/Nova/Compiler/Parser.purs"
    ["src/Nova/Compiler/Types.purs", "src/Nova/Compiler/Ast.purs", "src/Nova/Compiler/Tokenizer.purs"]

  log ""
  log "=== Done ==="

-- | Parse a file and return its declarations
parseFile :: String -> Effect (Array Declaration)
parseFile path = do
  content <- readTextFile UTF8 path
  let tokens = tokenize content
  case parseModule tokens of
    Left _ -> pure []
    Right (Tuple m _) -> pure m.declarations

compileFile :: String -> String -> Array String -> Effect Unit
compileFile name path deps = do
  log $ "Compiling " <> name <> "..."
  -- Load dependency declarations
  depDecls <- map Array.concat $ traverse parseFile deps
  -- Load main file
  content <- readTextFile UTF8 path
  let tokens = tokenize content
  case parseModule tokens of
    Left parseErr -> log $ "  Parse error: " <> parseErr
    Right (Tuple mod _) -> do
      log $ "  Parsed " <> show (Array.length mod.declarations) <> " declarations"
      -- Combine dep declarations with main for type checking
      let allDecls = depDecls <> mod.declarations
      case checkModule emptyEnv allDecls of
        Left tcErr -> log $ "  Type error: " <> show tcErr
        Right _env -> do
          log $ "  Type check passed"
          -- Generate code only for the main module
          let elixirCode = genModule mod
          let outPath = "output/" <> name <> ".ex"
          let lines = String.split (String.Pattern "\n") elixirCode
          log $ "  Generated " <> show (Array.length lines) <> " lines of Elixir"
          writeTextFile UTF8 outPath elixirCode
          log $ "  Written to " <> outPath
