module Nova.Compiler.RefEq where

import Nova.Compiler.Ast (Expr)

-- Reference equality check via FFI (monomorphic to avoid generalization issues)
foreign import refEqExpr :: Expr -> Expr -> Boolean
