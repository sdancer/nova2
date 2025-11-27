module Test.Parser.ParserTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser as P
import Nova.Compiler.Ast as Ast

main :: Effect Unit
main = do
  log "=== Parser Tests ==="
  log ""

  -- Test 1: Module header
  testCase "Module header" "module Main where" $ \result ->
    case result of
      Right (Tuple (Ast.DeclModule m) _) -> m.name == "Main"
      _ -> false

  -- Test 2: Simple import
  testCase "Simple import" "import Prelude" $ \result ->
    case result of
      Right (Tuple (Ast.DeclImport i) _) -> i.moduleName == "Prelude"
      _ -> false

  -- Test 3: Import with alias
  testCase "Import with alias" "import Data.Array as A" $ \result ->
    case result of
      Right (Tuple (Ast.DeclImport i) _) ->
        i.moduleName == "Data.Array" && i.alias == Just "A"
      _ -> false

  -- Test 4: Data type
  testCase "Data type" "data Maybe a = Nothing | Just a" $ \result ->
    case result of
      Right (Tuple (Ast.DeclDataType d) _) ->
        d.name == "Maybe" && Array.length d.constructors == 2
      _ -> false

  -- Test 5: Type alias
  testCase "Type alias" "type Pair a = { fst :: a, snd :: a }" $ \result ->
    case result of
      Right (Tuple (Ast.DeclTypeAlias a) _) -> a.name == "Pair"
      _ -> false

  -- Test 6: Simple function
  testCase "Simple function" "add x y = x + y" $ \result ->
    case result of
      Right (Tuple (Ast.DeclFunction f) _) ->
        f.name == "add" && Array.length f.parameters == 2
      _ -> false

  -- Test 7: Lambda expression
  testExpr "Lambda" "\\x -> x + 1" $ \expr ->
    case expr of
      Ast.ExprLambda params _ -> Array.length params == 1
      _ -> false

  -- Test 8: If expression
  testExpr "If expression" "if true then 1 else 0" $ \expr ->
    case expr of
      Ast.ExprIf _ _ _ -> true
      _ -> false

  -- Test 9: Let expression
  testExpr "Let expression" "let x = 1 in x + 1" $ \expr ->
    case expr of
      Ast.ExprLet bindings _ -> Array.length bindings == 1
      _ -> false

  -- Test 10: Function application
  testExpr "Function application" "f x y" $ \expr ->
    case expr of
      Ast.ExprApp (Ast.ExprApp _ _) _ -> true
      _ -> false

  -- Test 11: Binary operators
  testExpr "Binary operators" "1 + 2 * 3" $ \expr ->
    case expr of
      Ast.ExprBinOp "+" _ _ -> true
      _ -> false

  -- Test 12: List literal
  testExpr "List literal" "[1, 2, 3]" $ \expr ->
    case expr of
      Ast.ExprList elems -> Array.length elems == 3
      _ -> false

  -- Test 13: Record literal
  testExpr "Record literal" "{ x: 1, y: 2 }" $ \expr ->
    case expr of
      Ast.ExprRecord fields -> Array.length fields == 2
      _ -> false

  -- Test 14: Qualified identifier
  testExpr "Qualified identifier" "Array.length" $ \expr ->
    case expr of
      Ast.ExprQualified "Array" "length" -> true
      _ -> false

  -- Test 15: Case expression
  testExpr "Case expression" "case x of\n  Nothing -> 0\n  Just n -> n" $ \expr ->
    case expr of
      Ast.ExprCase _ clauses -> Array.length clauses == 2
      _ -> false

  -- Test 16: Type signature parsing
  testCase "Type signature" "foo :: Int -> String" $ \result ->
    case result of
      Right (Tuple (Ast.DeclTypeSig ts) _) ->
        ts.name == "foo"
      _ -> false

  -- Test 17: Function type
  testType "Function type" "Int -> String" $ \ty ->
    case ty of
      Ast.TyExprArrow _ _ -> true
      _ -> false

  -- Test 18: Parameterized type
  testType "Parameterized type" "Maybe Int" $ \ty ->
    case ty of
      Ast.TyExprApp (Ast.TyExprCon "Maybe") (Ast.TyExprCon "Int") -> true
      _ -> false

  -- Test 19: Record type
  testType "Record type" "{ name :: String, age :: Int }" $ \ty ->
    case ty of
      Ast.TyExprRecord fields _ -> Array.length fields == 2
      _ -> false

  -- Test 20: Full module
  testModule "Full module" """module Test where

import Prelude

foo :: Int -> Int
foo x = x + 1
""" $ \m ->
    m.name == "Test" && Array.length m.declarations >= 2

  log ""
  log "=== Tests Complete ==="

testCase :: String -> String -> (P.ParseResult Ast.Declaration -> Boolean) -> Effect Unit
testCase name input check = do
  let tokens = tokenize input
  let result = P.parseDeclaration tokens
  if check result
    then log $ "✓ " <> name
    else do
      log $ "✗ " <> name
      log $ "  Input: " <> show input
      case result of
        Left err -> log $ "  Error: " <> err
        Right (Tuple decl _) -> log $ "  Got declaration"

testExpr :: String -> String -> (Ast.Expr -> Boolean) -> Effect Unit
testExpr name input check = do
  let tokens = tokenize input
  let result = P.parseExpression tokens
  case result of
    Right (Tuple expr _) | check expr -> log $ "✓ " <> name
    Right (Tuple expr _) -> do
      log $ "✗ " <> name
      log $ "  Input: " <> show input
      log $ "  Expression parsed but check failed"
    Left err -> do
      log $ "✗ " <> name
      log $ "  Input: " <> show input
      log $ "  Error: " <> err

testType :: String -> String -> (Ast.TypeExpr -> Boolean) -> Effect Unit
testType name input check = do
  let tokens = tokenize input
  let result = P.parseType tokens
  case result of
    Right (Tuple ty _) | check ty -> log $ "✓ " <> name
    Right (Tuple ty _) -> do
      log $ "✗ " <> name
      log $ "  Input: " <> show input
      log $ "  Type parsed but check failed"
    Left err -> do
      log $ "✗ " <> name
      log $ "  Input: " <> show input
      log $ "  Error: " <> err

testModule :: String -> String -> (Ast.Module -> Boolean) -> Effect Unit
testModule name input check = do
  let tokens = tokenize input
  let result = P.parseModule tokens
  case result of
    Right (Tuple m _) | check m -> log $ "✓ " <> name
    Right (Tuple m _) -> do
      log $ "✗ " <> name
      log $ "  Input: " <> show input
      log $ "  Module parsed but check failed"
    Left err -> do
      log $ "✗ " <> name
      log $ "  Input: " <> show input
      log $ "  Error: " <> err
