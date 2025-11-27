module Test.TypeCheck.TypeCheckTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Nova.Compiler.Ast (Expr(..), Literal(..), Pattern(..))
import Nova.Compiler.Types (emptyEnv, tInt, tBool, tArrow)
import Nova.Compiler.TypeChecker (infer, TCError)
import Data.Maybe (Maybe(..))

main :: Effect Unit
main = do
  log "=== TypeChecker Tests ==="

  -- Test literal inference
  log "\n-- Literal tests --"
  testExpr "integer literal" (ExprLit (LitInt 42))
  testExpr "string literal" (ExprLit (LitString "hello"))
  testExpr "bool literal" (ExprLit (LitBool true))

  -- Test lambda inference
  log "\n-- Lambda tests --"
  testExpr "identity lambda" (ExprLambda [PatVar "x"] (ExprVar "x"))
  testExpr "const lambda" (ExprLambda [PatVar "x", PatVar "y"] (ExprVar "x"))

  -- Test if expression
  log "\n-- If expression tests --"
  testExpr "if true then 1 else 2"
    (ExprIf (ExprLit (LitBool true)) (ExprLit (LitInt 1)) (ExprLit (LitInt 2)))

  -- Test let expression
  log "\n-- Let expression tests --"
  testExpr "let x = 1 in x"
    (ExprLet [{ pattern: PatVar "x", value: ExprLit (LitInt 1), typeAnn: Nothing }] (ExprVar "x"))

  log "\n=== All tests completed ==="

testExpr :: String -> Expr -> Effect Unit
testExpr name expr =
  case infer emptyEnv expr of
    Left err -> log $ "FAIL: " <> name <> " - " <> show err
    Right res -> log $ "PASS: " <> name <> " => inferred type"
