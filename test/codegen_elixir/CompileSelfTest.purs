module Test.CodeGenElixir.CompileSelfTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Array as Array
import Data.String as String
import Data.Traversable (traverse)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Nova.Compiler.CstPipeline (parseModuleCst)
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
  compileFile "CodeGen" "src/Nova/Compiler/CodeGen.purs" []
  compileFile "TypeChecker" "src/Nova/Compiler/TypeChecker.purs" []

  -- CstToAst depends on Types, Ast
  compileFile "CstToAst" "src/Nova/Compiler/CstToAst.purs"
    ["src/Nova/Compiler/Types.purs", "src/Nova/Compiler/Ast.purs"]

  log ""
  log "=== Done ==="

-- | Parse a file and return its declarations
parseFile :: String -> Effect (Array Declaration)
parseFile path = do
  content <- readTextFile UTF8 path
  case parseModuleCst content of
    Left _ -> pure []
    Right m -> pure (Array.fromFoldable m.declarations)

compileFile :: String -> String -> Array String -> Effect Unit
compileFile name path deps = do
  log $ "Compiling " <> name <> "..."
  -- Load dependency declarations
  depDecls <- map Array.concat $ traverse parseFile deps
  -- Load main file
  content <- readTextFile UTF8 path
  case parseModuleCst content of
    Left parseErr -> log $ "  Parse error: " <> parseErr
    Right mod -> do
      let modDecls = Array.fromFoldable mod.declarations
      log $ "  Parsed " <> show (Array.length modDecls) <> " declarations"
      -- Combine dep declarations with main for type checking
      let allDecls = depDecls <> modDecls
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
