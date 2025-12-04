module Nova.Compiler.RefEq where

import Prelude
import Data.List (length)
import Nova.Compiler.Ast (Expr(..))

-- Reference equality for expressions
-- In the compiled version, we use structural equality
-- This is used for lambda lifting to match expressions

refEqExpr :: Expr -> Expr -> Boolean
refEqExpr (ExprVar a) (ExprVar b) = a == b
refEqExpr (ExprLit a) (ExprLit b) = true -- Simplified: literals are equal if same constructor
refEqExpr (ExprApp f1 a1) (ExprApp f2 a2) = refEqExpr f1 f2 && refEqExpr a1 a2
refEqExpr (ExprLambda p1 b1) (ExprLambda p2 b2) =
  length p1 == length p2 && refEqExpr b1 b2
refEqExpr (ExprParens e1) (ExprParens e2) = refEqExpr e1 e2
refEqExpr (ExprParens e1) e2 = refEqExpr e1 e2
refEqExpr e1 (ExprParens e2) = refEqExpr e1 e2
refEqExpr _ _ = false
