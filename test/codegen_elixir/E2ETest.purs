module Test.CodeGenElixir.E2ETest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.String as String
import Nova.Compiler.Ast (Module, Declaration(..), Expr(..), Literal(..), Pattern(..), DoStatement(..), LetBind)
import Nova.Compiler.CodeGen (genModule)

foreign import runElixirCode :: String -> Effect String

main :: Effect Unit
main = do
  log "=== E2E CodeGen Tests ==="
  log "Testing generated Elixir code execution\n"

  -- Test 1: Simple arithmetic
  testElixir "arithmetic"
    { name: "Test.Arithmetic"
    , declarations:
        [ DeclFunction
            { name: "add"
            , parameters: [PatVar "x", PatVar "y"]
            , body: ExprBinOp "+" (ExprVar "x") (ExprVar "y")
            , guards: []
            , typeSignature: Nothing
            }
        , DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprApp (ExprApp (ExprVar "add") (ExprLit (LitInt 2))) (ExprLit (LitInt 3))
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.Arithmetic.main()"
    "5"

  -- Test 2: Identity function
  testElixir "identity"
    { name: "Test.Identity"
    , declarations:
        [ DeclFunction
            { name: "id"
            , parameters: [PatVar "x"]
            , body: ExprVar "x"
            , guards: []
            , typeSignature: Nothing
            }
        , DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprApp (ExprVar "id") (ExprLit (LitInt 42))
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.Identity.main()"
    "42"

  -- Test 3: Boolean if
  testElixir "boolean_if"
    { name: "Test.Boolean"
    , declarations:
        [ DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprIf (ExprLit (LitBool true)) (ExprLit (LitInt 1)) (ExprLit (LitInt 0))
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.Boolean.main()"
    "1"

  -- Test 4: List
  testElixir "list"
    { name: "Test.ListTest"
    , declarations:
        [ DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprList [ExprLit (LitInt 1), ExprLit (LitInt 2), ExprLit (LitInt 3)]
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.ListTest.main()"
    "[1, 2, 3]"

  -- Test 5: Record
  testElixir "record"
    { name: "Test.RecordTest"
    , declarations:
        [ DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprRecord [Tuple "x" (ExprLit (LitInt 10)), Tuple "y" (ExprLit (LitInt 20))]
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.RecordTest.main()"
    "%{x: 10, y: 20}"

  -- Test 6: Lambda application
  testElixir "lambda"
    { name: "Test.LambdaTest"
    , declarations:
        [ DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprApp
                (ExprLambda [PatVar "x"] (ExprBinOp "*" (ExprVar "x") (ExprLit (LitInt 2))))
                (ExprLit (LitInt 21))
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.LambdaTest.main()"
    "42"

  -- Test 7: Comparison operators
  testElixir "comparison"
    { name: "Test.Compare"
    , declarations:
        [ DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprIf
                (ExprBinOp ">" (ExprLit (LitInt 5)) (ExprLit (LitInt 3)))
                (ExprLit (LitInt 1))
                (ExprLit (LitInt 0))
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.Compare.main()"
    "1"

  -- Test 8: Boolean operators
  testElixir "boolean_and"
    { name: "Test.BoolAnd"
    , declarations:
        [ DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprBinOp "&&" (ExprLit (LitBool true)) (ExprLit (LitBool false))
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.BoolAnd.main()"
    "false"

  -- Test 9: List pattern matching (cons)
  testElixir "cons_pattern"
    { name: "Test.ConsPat"
    , declarations:
        [ DeclFunction
            { name: "first"
            , parameters: [PatCons (PatVar "h") (PatVar "t")]
            , body: ExprVar "h"
            , guards: []
            , typeSignature: Nothing
            }
        , DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprApp (ExprVar "first") (ExprList [ExprLit (LitInt 42), ExprLit (LitInt 1), ExprLit (LitInt 2)])
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.ConsPat.main()"
    "42"

  -- Test 10: Record pattern matching
  testElixir "record_pattern"
    { name: "Test.RecPat"
    , declarations:
        [ DeclFunction
            { name: "getX"
            , parameters: [PatRecord [Tuple "x" (PatVar "x")]]
            , body: ExprVar "x"
            , guards: []
            , typeSignature: Nothing
            }
        , DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprApp (ExprVar "getX") (ExprRecord [Tuple "x" (ExprLit (LitInt 99)), Tuple "y" (ExprLit (LitInt 1))])
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.RecPat.main()"
    "99"

  -- Test 11: Case expression with patterns
  testElixir "case_cons"
    { name: "Test.CaseCons"
    , declarations:
        [ DeclFunction
            { name: "sumList"
            , parameters: [PatVar "xs"]
            , body: ExprCase (ExprVar "xs")
                [ { pattern: PatList [], guard: Nothing, body: ExprLit (LitInt 0) }
                , { pattern: PatCons (PatVar "h") (PatVar "t"), guard: Nothing
                  , body: ExprBinOp "+" (ExprVar "h") (ExprApp (ExprVar "sumList") (ExprVar "t"))
                  }
                ]
            , guards: []
            , typeSignature: Nothing
            }
        , DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprApp (ExprVar "sumList") (ExprList [ExprLit (LitInt 1), ExprLit (LitInt 2), ExprLit (LitInt 3), ExprLit (LitInt 4), ExprLit (LitInt 5)])
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.CaseCons.main()"
    "15"

  -- Test 12: Recursive factorial
  testElixir "factorial"
    { name: "Test.Factorial"
    , declarations:
        [ DeclFunction
            { name: "fact"
            , parameters: [PatVar "n"]
            , body: ExprIf
                (ExprBinOp "==" (ExprVar "n") (ExprLit (LitInt 0)))
                (ExprLit (LitInt 1))
                (ExprBinOp "*" (ExprVar "n") (ExprApp (ExprVar "fact") (ExprBinOp "-" (ExprVar "n") (ExprLit (LitInt 1)))))
            , guards: []
            , typeSignature: Nothing
            }
        , DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprApp (ExprVar "fact") (ExprLit (LitInt 5))
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.Factorial.main()"
    "120"

  -- Test 13: Record update
  testElixir "record_update"
    { name: "Test.RecUpdate"
    , declarations:
        [ DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprRecordAccess
                (ExprRecordUpdate
                  (ExprRecord [Tuple "x" (ExprLit (LitInt 1)), Tuple "y" (ExprLit (LitInt 2))])
                  [Tuple "x" (ExprLit (LitInt 100))])
                "x"
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.RecUpdate.main()"
    "100"

  -- Test 14: Unary negation
  testElixir "unary_neg"
    { name: "Test.UnaryNeg"
    , declarations:
        [ DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprUnaryOp "-" (ExprLit (LitInt 42))
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.UnaryNeg.main()"
    "-42"

  -- Test 15: Do notation (simple sequence)
  testElixir "do_simple"
    { name: "Test.DoSimple"
    , declarations:
        [ DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprDo
                [ DoExpr (ExprLit (LitInt 1))
                , DoExpr (ExprLit (LitInt 2))
                , DoExpr (ExprLit (LitInt 42))
                ]
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.DoSimple.main()"
    "42"

  -- Test 16: Do notation with let
  testElixir "do_let"
    { name: "Test.DoLet"
    , declarations:
        [ DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprDo
                [ DoLet [{ pattern: PatVar "x", value: ExprLit (LitInt 10), typeAnn: Nothing }]
                , DoLet [{ pattern: PatVar "y", value: ExprLit (LitInt 20), typeAnn: Nothing }]
                , DoExpr (ExprBinOp "+" (ExprVar "x") (ExprVar "y"))
                ]
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.DoLet.main()"
    "30"

  -- Test 17: String concatenation
  testElixir "string_concat"
    { name: "Test.StringConcat"
    , declarations:
        [ DeclFunction
            { name: "main"
            , parameters: []
            , body: ExprBinOp "<>" (ExprLit (LitString "Hello, ")) (ExprLit (LitString "World!"))
            , guards: []
            , typeSignature: Nothing
            }
        ]
    }
    "Test.StringConcat.main()"
    "\"Hello, World!\""

  log "\n=== E2E Tests Complete ==="

testElixir :: String -> Module -> String -> String -> Effect Unit
testElixir name mod expr expected = do
  let code = genModule mod
  let fullCode = code <> "\nIO.inspect(" <> expr <> ")"
  output <- runElixirCode fullCode
  let trimmed = String.trim output
  if trimmed == expected
    then log $ "PASS: " <> name
    else log $ "FAIL: " <> name <> "\n  expected: " <> expected <> "\n  got: " <> trimmed <> "\n  code:\n" <> code
