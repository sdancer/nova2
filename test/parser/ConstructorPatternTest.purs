-- | Tests for constructor pattern parsing in function definitions
module Test.Parser.ConstructorPatternTest where

import Prelude

import Data.Either (Either(..), isRight)
import Data.List (List(..), length)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Nova.Compiler.Ast as Ast
import Nova.Compiler.CstParser (parseModule, runParser)
import Nova.Compiler.CstLexer (lexModule)
import Nova.Compiler.CstToAst (convertModule)

-- | Parse a module string to AST
parseToAst :: String -> Either String Ast.Module
parseToAst source = do
  let tokens = lexModule source
  Tuple cstMod _ <- runParser parseModule tokens
  convertModule cstMod

-- | Test that nullary constructors in function parameters are parsed as separate arguments
-- | Example: `foo Nil x` should have 2 parameters (Nil and x), not 1 parameter (Nil x)
testNullaryConstructorSeparateFromFollowingArgs :: Effect Boolean
testNullaryConstructorSeparateFromFollowingArgs = do
  let source = """module Test where
instantiateGo schemeTy e Nil sub = { ty: schemeTy, env: e }"""

  case parseToAst source of
    Left err -> do
      log $ "Parse failed: " <> err
      pure false
    Right mod -> case mod.declarations of
      Cons (Ast.DeclFunction func) Nil -> do
        let paramCount = length func.parameters
        if paramCount == 4
          then do
            log "✓ testNullaryConstructorSeparateFromFollowingArgs passed"
            pure true
          else do
            log $ "✗ Expected 4 parameters, got " <> show paramCount
            pure false
      _ -> do
        log "✗ Expected exactly one function declaration"
        pure false

-- | Test that constructor patterns with arguments still work in parentheses
testConstructorWithArgsInParens :: Effect Boolean
testConstructorWithArgsInParens = do
  let source = """module Test where
length (Cons x xs) = 1 + length xs"""

  case parseToAst source of
    Left err -> do
      log $ "Parse failed: " <> err
      pure false
    Right mod -> case mod.declarations of
      Cons (Ast.DeclFunction func) Nil -> do
        let paramCount = length func.parameters
        if paramCount == 1
          then do
            log "✓ testConstructorWithArgsInParens passed"
            pure true
          else do
            log $ "✗ Expected 1 parameter (the parenthesized pattern), got " <> show paramCount
            pure false
      _ -> do
        log "✗ Expected exactly one function declaration"
        pure false

-- | Test multiple nullary constructors as separate parameters
testMultipleNullaryConstructors :: Effect Boolean
testMultipleNullaryConstructors = do
  let source = """module Test where
foo True False Nothing = 42"""

  case parseToAst source of
    Left err -> do
      log $ "Parse failed: " <> err
      pure false
    Right mod -> case mod.declarations of
      Cons (Ast.DeclFunction func) Nil -> do
        let paramCount = length func.parameters
        if paramCount == 3
          then do
            log "✓ testMultipleNullaryConstructors passed"
            pure true
          else do
            log $ "✗ Expected 3 parameters, got " <> show paramCount
            pure false
      _ -> do
        log "✗ Expected exactly one function declaration"
        pure false

-- | Run all constructor pattern tests
main :: Effect Unit
main = do
  log "=== Constructor Pattern Parsing Tests ==="

  r1 <- testNullaryConstructorSeparateFromFollowingArgs
  r2 <- testConstructorWithArgsInParens
  r3 <- testMultipleNullaryConstructors

  let passed = if r1 && r2 && r3 then "ALL PASSED" else "SOME FAILED"
  log $ "\n=== " <> passed <> " ==="
