module Test.Parser.TestParenCase where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.List as List
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast as Ast

-- Test parenthesized case expressions
-- Issue: (case x of ...) fails to parse when case spans multiple lines

main :: Effect Unit
main = do
  log "=== Parenthesized Case Expression Tests ==="
  log ""

  -- Test 1: Simple case without parens (baseline - should work)
  testExpr "Case without parens" "case x of\n  Just _ -> 1\n  Nothing -> 0" $ \expr ->
    case expr of
      Ast.ExprCase _ clauses -> List.length clauses == 2
      _ -> false

  -- Test 2: Single-line case in parens (should work)
  testExpr "Single-line paren case" "(case x of Just _ -> 1)" $ \expr ->
    case expr of
      Ast.ExprCase _ clauses -> List.length clauses == 1
      _ -> false

  -- Test 3: Multiline case in parens (the failing case)
  testExpr "Multiline paren case" "(case x of\n  Just _ -> 1\n  Nothing -> 0)" $ \expr ->
    case expr of
      Ast.ExprCase _ clauses -> List.length clauses == 2
      _ -> false

  -- Test 4: Paren case followed by operator (real-world use case)
  testExpr "Paren case with operator" "(case x of\n  Just n -> n\n  Nothing -> 0) + 1" $ \expr ->
    case expr of
      Ast.ExprBinOp "+" (Ast.ExprCase _ _) _ -> true
      _ -> false

  -- Test 5: Nested parens with case
  testExpr "Nested parens case" "((case x of\n  Just _ -> 1\n  Nothing -> 0))" $ \expr ->
    case expr of
      Ast.ExprCase _ clauses -> List.length clauses == 2
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
