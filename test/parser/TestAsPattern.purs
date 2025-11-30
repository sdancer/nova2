module Test.Parser.TestAsPattern where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast as Ast

-- Test as-patterns (name@pattern)
-- Issue: e@(ExprLambda pats body) fails to parse in function parameters

main :: Effect Unit
main = do
  log "=== As-Pattern Tests ==="
  log ""

  -- Test 1: Simple as-pattern in case
  testExpr "As-pattern in case" "case x of y@(Just z) -> y" $ \expr ->
    case expr of
      Ast.ExprCase _ clauses -> Array.length clauses == 1
      _ -> false

  -- Test 2: As-pattern in function parameter
  testDecl "As-pattern in function param" "foo x@(Just y) = y" $ \decl ->
    case decl of
      Ast.DeclFunction f -> f.name == "foo"
      _ -> false

  -- Test 3: As-pattern with constructor
  testDecl "As-pattern with constructor" "bar e@(ExprLambda pats body) = e" $ \decl ->
    case decl of
      Ast.DeclFunction f -> f.name == "bar"
      _ -> false

  -- Test 4: Multiple params with as-pattern
  testDecl "Multiple params with as-pattern" "baz ctx e@(Just x) = x" $ \decl ->
    case decl of
      Ast.DeclFunction f -> f.name == "baz" && Array.length f.parameters == 2
      _ -> false

  log ""
  log "=== Tests Complete ==="

testExpr :: String -> String -> (Ast.Expr -> Boolean) -> Effect Unit
testExpr name input check = do
  let tokens = tokenize input
  let result = P.parseExpression tokens
  case result of
    Right (Tuple expr rest) | check expr && Array.length rest == 0 -> log $ "✓ " <> name
    Right (Tuple expr rest) | check expr -> do
      log $ "✗ " <> name
      log $ "  Expression parsed but leftover tokens remain"
    Right (Tuple expr _) -> do
      log $ "✗ " <> name
      log $ "  Input: " <> show input
      log $ "  Expression parsed but check failed"
    Left err -> do
      log $ "✗ " <> name
      log $ "  Input: " <> show input
      log $ "  Error: " <> err

testDecl :: String -> String -> (Ast.Declaration -> Boolean) -> Effect Unit
testDecl name input check = do
  let tokens = tokenize input
  let result = P.parseDeclaration tokens
  case result of
    Right (Tuple decl rest) | check decl && Array.length rest == 0 -> log $ "✓ " <> name
    Right (Tuple decl rest) | check decl -> do
      log $ "✗ " <> name
      log $ "  Declaration parsed but leftover tokens remain"
    Right (Tuple decl _) -> do
      log $ "✗ " <> name
      log $ "  Input: " <> show input
      log $ "  Declaration parsed but check failed"
    Left err -> do
      log $ "✗ " <> name
      log $ "  Input: " <> show input
      log $ "  Error: " <> err
