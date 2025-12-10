module Nova.Compiler.RefEq where

import Prelude
import Data.List (List(..), length, zip)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Foldable (all)
import Nova.Compiler.Ast (Expr(..), Pattern(..), Literal(..), DoStatement(..), LetBind, CaseClause)

-- Reference equality for expressions
-- In the compiled version, we use structural equality
-- This is used for lambda lifting to match expressions

refEqExpr :: Expr -> Expr -> Boolean
refEqExpr (ExprVar a) (ExprVar b) = a == b
refEqExpr (ExprQualified ns1 n1) (ExprQualified ns2 n2) = ns1 == ns2 && n1 == n2
refEqExpr (ExprLit a) (ExprLit b) = refEqLit a b
refEqExpr (ExprApp f1 a1) (ExprApp f2 a2) = refEqExpr f1 f2 && refEqExpr a1 a2
refEqExpr (ExprLambda p1 b1) (ExprLambda p2 b2) =
  length p1 == length p2 && refEqPatternList p1 p2 && refEqExpr b1 b2
refEqExpr (ExprLet binds1 body1) (ExprLet binds2 body2) =
  length binds1 == length binds2 && refEqLetBindList binds1 binds2 && refEqExpr body1 body2
refEqExpr (ExprIf c1 t1 e1) (ExprIf c2 t2 e2) =
  refEqExpr c1 c2 && refEqExpr t1 t2 && refEqExpr e1 e2
refEqExpr (ExprCase scrut1 clauses1) (ExprCase scrut2 clauses2) =
  refEqExpr scrut1 scrut2 && length clauses1 == length clauses2 && refEqCaseClauseList clauses1 clauses2
refEqExpr (ExprDo stmts1) (ExprDo stmts2) =
  length stmts1 == length stmts2 && refEqDoStmtList stmts1 stmts2
refEqExpr (ExprBinOp op1 l1 r1) (ExprBinOp op2 l2 r2) =
  op1 == op2 && refEqExpr l1 l2 && refEqExpr r1 r2
refEqExpr (ExprUnaryOp op1 e1) (ExprUnaryOp op2 e2) =
  op1 == op2 && refEqExpr e1 e2
refEqExpr (ExprList es1) (ExprList es2) =
  length es1 == length es2 && refEqExprList es1 es2
refEqExpr (ExprTuple es1) (ExprTuple es2) =
  length es1 == length es2 && refEqExprList es1 es2
refEqExpr (ExprRecord fields1) (ExprRecord fields2) =
  length fields1 == length fields2 && refEqRecordFields fields1 fields2
refEqExpr (ExprRecordAccess e1 f1) (ExprRecordAccess e2 f2) =
  f1 == f2 && refEqExpr e1 e2
refEqExpr (ExprRecordUpdate e1 upd1) (ExprRecordUpdate e2 upd2) =
  refEqExpr e1 e2 && length upd1 == length upd2 && refEqRecordFields upd1 upd2
refEqExpr (ExprTyped e1 _) (ExprTyped e2 _) = refEqExpr e1 e2  -- Ignore type annotations for equality
refEqExpr (ExprParens e1) (ExprParens e2) = refEqExpr e1 e2
refEqExpr (ExprParens e1) e2 = refEqExpr e1 e2
refEqExpr e1 (ExprParens e2) = refEqExpr e1 e2
refEqExpr (ExprSection s1) (ExprSection s2) = s1 == s2
refEqExpr _ _ = false

-- | Compare literals
refEqLit :: Literal -> Literal -> Boolean
refEqLit (LitInt a) (LitInt b) = a == b
refEqLit (LitNumber a) (LitNumber b) = a == b
refEqLit (LitString a) (LitString b) = a == b
refEqLit (LitChar a) (LitChar b) = a == b
refEqLit (LitBool a) (LitBool b) = a == b
refEqLit _ _ = false

-- | Compare patterns
refEqPattern :: Pattern -> Pattern -> Boolean
refEqPattern (PatVar a) (PatVar b) = a == b
refEqPattern PatWildcard PatWildcard = true
refEqPattern (PatLit a) (PatLit b) = refEqLit a b
refEqPattern (PatCon n1 ps1) (PatCon n2 ps2) =
  n1 == n2 && length ps1 == length ps2 && refEqPatternList ps1 ps2
refEqPattern (PatList ps1) (PatList ps2) =
  length ps1 == length ps2 && refEqPatternList ps1 ps2
refEqPattern (PatCons h1 t1) (PatCons h2 t2) =
  refEqPattern h1 h2 && refEqPattern t1 t2
refEqPattern (PatRecord fs1) (PatRecord fs2) =
  length fs1 == length fs2 && all (\(Tuple (Tuple n1 p1) (Tuple n2 p2)) -> n1 == n2 && refEqPattern p1 p2) (zip fs1 fs2)
refEqPattern (PatParens p1) (PatParens p2) = refEqPattern p1 p2
refEqPattern (PatParens p1) p2 = refEqPattern p1 p2
refEqPattern p1 (PatParens p2) = refEqPattern p1 p2
refEqPattern (PatAs n1 p1) (PatAs n2 p2) = n1 == n2 && refEqPattern p1 p2
refEqPattern _ _ = false

-- | Compare pattern lists
refEqPatternList :: List Pattern -> List Pattern -> Boolean
refEqPatternList Nil Nil = true
refEqPatternList (Cons p1 ps1) (Cons p2 ps2) = refEqPattern p1 p2 && refEqPatternList ps1 ps2
refEqPatternList _ _ = false

-- | Compare expression lists
refEqExprList :: List Expr -> List Expr -> Boolean
refEqExprList Nil Nil = true
refEqExprList (Cons e1 es1) (Cons e2 es2) = refEqExpr e1 e2 && refEqExprList es1 es2
refEqExprList _ _ = false

-- | Compare let binding lists
refEqLetBindList :: List LetBind -> List LetBind -> Boolean
refEqLetBindList Nil Nil = true
refEqLetBindList (Cons b1 bs1) (Cons b2 bs2) =
  refEqPattern b1.pattern b2.pattern && refEqExpr b1.value b2.value && refEqLetBindList bs1 bs2
refEqLetBindList _ _ = false

-- | Compare case clause lists
refEqCaseClauseList :: List CaseClause -> List CaseClause -> Boolean
refEqCaseClauseList Nil Nil = true
refEqCaseClauseList (Cons c1 cs1) (Cons c2 cs2) =
  refEqPattern c1.pattern c2.pattern &&
  refEqMaybeExpr c1.guard c2.guard &&
  refEqExpr c1.body c2.body &&
  refEqCaseClauseList cs1 cs2
refEqCaseClauseList _ _ = false

-- | Compare Maybe Expr
refEqMaybeExpr :: Maybe Expr -> Maybe Expr -> Boolean
refEqMaybeExpr Nothing Nothing = true
refEqMaybeExpr (Just e1) (Just e2) = refEqExpr e1 e2
refEqMaybeExpr _ _ = false

-- | Compare do statement lists
refEqDoStmtList :: List DoStatement -> List DoStatement -> Boolean
refEqDoStmtList Nil Nil = true
refEqDoStmtList (Cons s1 ss1) (Cons s2 ss2) =
  refEqDoStmt s1 s2 && refEqDoStmtList ss1 ss2
refEqDoStmtList _ _ = false

-- | Compare do statements
refEqDoStmt :: DoStatement -> DoStatement -> Boolean
refEqDoStmt (DoLet binds1) (DoLet binds2) = refEqLetBindList binds1 binds2
refEqDoStmt (DoBind p1 e1) (DoBind p2 e2) = refEqPattern p1 p2 && refEqExpr e1 e2
refEqDoStmt (DoExpr e1) (DoExpr e2) = refEqExpr e1 e2
refEqDoStmt _ _ = false

-- | Compare record fields
refEqRecordFields :: List (Tuple String Expr) -> List (Tuple String Expr) -> Boolean
refEqRecordFields Nil Nil = true
refEqRecordFields (Cons (Tuple n1 e1) fs1) (Cons (Tuple n2 e2) fs2) =
  n1 == n2 && refEqExpr e1 e2 && refEqRecordFields fs1 fs2
refEqRecordFields _ _ = false
