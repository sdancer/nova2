module Test.CodeGenElixir.CodeGenTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Nova.Compiler.Ast (Module, Declaration(..), FunctionDeclaration, Expr(..), Literal(..), Pattern(..))
import Nova.Compiler.CodeGen (genModule, genFunction, genExpr', emptyCtx)

main :: Effect Unit
main = do
  log "=== CodeGen Tests ==="

  let ctx = emptyCtx

  -- Test literal generation
  log "\n-- Literal generation --"
  log $ "Int 42: " <> genExpr' ctx 0 (ExprLit (LitInt 42))
  log $ "String \"hello\": " <> genExpr' ctx 0 (ExprLit (LitString "hello"))
  log $ "Bool true: " <> genExpr' ctx 0 (ExprLit (LitBool true))
  log $ "Char 'a': " <> genExpr' ctx 0 (ExprLit (LitChar 'a'))

  -- Test lambda generation
  log "\n-- Lambda generation --"
  log $ "identity: " <> genExpr' ctx 0 (ExprLambda [PatVar "x"] (ExprVar "x"))
  log $ "const: " <> genExpr' ctx 0 (ExprLambda [PatVar "x", PatVar "y"] (ExprVar "x"))

  -- Test if generation
  log "\n-- If generation --"
  log $ "if: " <> genExpr' ctx 0 (ExprIf (ExprLit (LitBool true)) (ExprLit (LitInt 1)) (ExprLit (LitInt 2)))

  -- Test list generation
  log "\n-- List generation --"
  log $ "list: " <> genExpr' ctx 0 (ExprList [ExprLit (LitInt 1), ExprLit (LitInt 2), ExprLit (LitInt 3)])

  -- Test record generation
  log "\n-- Record generation --"
  log $ "record: " <> genExpr' ctx 0 (ExprRecord [Tuple "x" (ExprLit (LitInt 1)), Tuple "y" (ExprLit (LitInt 2))])

  -- Test function generation
  log "\n-- Function generation --"
  let addFunc :: FunctionDeclaration
      addFunc = { name: "add"
                , parameters: [PatVar "x", PatVar "y"]
                , body: ExprBinOp "+" (ExprVar "x") (ExprVar "y")
                , guards: []
                , typeSignature: Nothing
                }
  log $ "function:\n" <> genFunction ctx addFunc

  -- Test module generation
  log "\n-- Module generation --"
  let testModule :: Module
      testModule = { name: "Test.Example"
                   , declarations: [DeclFunction addFunc]
                   }
  log $ "module:\n" <> genModule testModule

  log "\n=== All CodeGen tests completed ==="
