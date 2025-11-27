module Test.TypeCheck.NegativeLitTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Nova.Compiler.Tokenizer (tokenize)
import Nova.Compiler.Parser (parseExpression)
import Nova.Compiler.Ast (Expr(..), Literal(..), Pattern(..))
import Nova.Compiler.Types (emptyEnv, tInt, tString)
import Nova.Compiler.TypeChecker (infer, TCError)

main :: Effect Unit
main = do
  log "=== Negative Literal Parsing Debug ==="

  -- Test 1: Just -1
  log "\n-- Test: -1 --"
  testParse "-1"
  testInfer "-1"

  -- Test 2: (-1)
  log "\n-- Test: (-1) --"
  testParse "(-1)"
  testInfer "(-1)"

  -- Test 3: mkTVar (-1) "a" - needs mkTVar in scope
  log "\n-- Test: mkTVar (-1) \"a\" (with mkTVar in env) --"
  testParse "mkTVar (-1) \"a\""
  testInferWithEnv "mkTVar (-1) \"a\""

  -- Test 3b: Test parts separately
  log "\n-- Test parts: mkTVar 1 \"a\" --"
  testInferWithEnv "mkTVar 1 \"a\""

  log "\n-- Test parts: mkTVar --"
  testInferWithEnv "mkTVar"

  log "\n-- Test parts: 1 --"
  testInferWithEnv "1"

  -- Test 3c: What does (-1) type to standalone?
  log "\n-- Test: just (-1) inferred --"
  testInferWithEnv "(-1)"

  -- Test 3d: Can we apply mkTVar to (-1)?
  log "\n-- Test: mkTVar (-1) --"
  testInferWithEnv "mkTVar (-1)"

  -- Test 4: function call with negative arg
  log "\n-- Test: foo (-1) --"
  testParse "foo (-1)"

  -- Test 5: literal negative integers in the source
  log "\n-- Test: (0 - 1) --"
  testParse "(0 - 1)"

  log "\n=== Done ==="

testParse :: String -> Effect Unit
testParse src = do
  let tokens = tokenize src
  case parseExpression tokens of
    Left err -> log $ "  Parse error: " <> err
    Right (Tuple expr _) -> log $ "  Parsed: " <> showExprDebug expr

testInfer :: String -> Effect Unit
testInfer src = do
  let tokens = tokenize src
  case parseExpression tokens of
    Left err -> log $ "  Parse error: " <> err
    Right (Tuple expr _) ->
      case infer emptyEnv expr of
        Left err -> log $ "  Type error: " <> show err
        Right _ -> log $ "  Type OK!"

-- Use emptyEnv which already includes builtinPrelude with mkTVar
testInferWithEnv :: String -> Effect Unit
testInferWithEnv src = do
  let tokens = tokenize src
  case parseExpression tokens of
    Left err -> log $ "  Parse error: " <> err
    Right (Tuple expr _) ->
      case infer emptyEnv expr of
        Left err -> log $ "  Type error: " <> show err
        Right _ -> log $ "  Type OK!"

-- Deep debug show for expressions
showExprDebug :: Expr -> String
showExprDebug (ExprLit (LitInt n)) = "LitInt(" <> show n <> ")"
showExprDebug (ExprLit (LitString s)) = "LitString(" <> show s <> ")"
showExprDebug (ExprVar v) = "Var(" <> v <> ")"
showExprDebug (ExprApp f a) = "App(" <> showExprDebug f <> ", " <> showExprDebug a <> ")"
showExprDebug (ExprParens e) = "Parens(" <> showExprDebug e <> ")"
showExprDebug (ExprBinOp op l r) = "BinOp(" <> op <> ", " <> showExprDebug l <> ", " <> showExprDebug r <> ")"
showExprDebug (ExprUnaryOp op e) = "UnaryOp(" <> op <> ", " <> showExprDebug e <> ")"
showExprDebug e = "<" <> showExprCon e <> ">"

showExprCon :: Expr -> String
showExprCon (ExprLit _) = "Lit"
showExprCon (ExprVar _) = "Var"
showExprCon (ExprApp _ _) = "App"
showExprCon (ExprParens _) = "Parens"
showExprCon (ExprUnaryOp _ _) = "UnaryOp"
showExprCon (ExprBinOp _ _ _) = "BinOp"
showExprCon (ExprLambda _ _) = "Lambda"
showExprCon (ExprLet _ _) = "Let"
showExprCon (ExprIf _ _ _) = "If"
showExprCon (ExprCase _ _) = "Case"
showExprCon (ExprDo _) = "Do"
showExprCon (ExprRecord _) = "Record"
showExprCon (ExprRecordAccess _ _) = "RecordAccess"
showExprCon (ExprRecordUpdate _ _) = "RecordUpdate"
showExprCon (ExprList _) = "List"
showExprCon (ExprTuple _) = "Tuple"
showExprCon (ExprQualified _ _) = "Qualified"
showExprCon (ExprTyped _ _) = "Typed"
showExprCon (ExprSection _) = "Section"
