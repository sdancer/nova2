# Sprint 1: Foundation - Pattern Inference & Prelude

## Goal
Complete pattern type inference and add core operators to enable basic functional programs.

---

## Task 1: Pattern Type Inference Completion

### 1.1 PatRecord
**File**: `src/Nova/Compiler/TypeChecker.purs`

```purescript
inferPat env (PatRecord fields) ty = do
  -- For each field (label, pattern):
  -- 1. Create fresh type var for field
  -- 2. Infer pattern with that type
  -- 3. Build expected record type
  -- 4. Unify with ty
```

**Test case**:
```purescript
-- Input
let { x, y } = { x: 1, y: 2 } in x + y

-- Should infer:
-- { x, y } :: { x :: Int, y :: Int }
-- result :: Int
```

### 1.2 PatList
**File**: `src/Nova/Compiler/TypeChecker.purs`

```purescript
inferPat env (PatList pats) ty = do
  -- 1. Create fresh type var for element
  -- 2. Unify ty with Array elemTy
  -- 3. Infer each pattern with elemTy
```

**Test case**:
```purescript
-- Input
case xs of
  [a, b, c] -> a + b + c
  _ -> 0

-- Should infer:
-- [a, b, c] :: Array Int
```

### 1.3 PatCons
**File**: `src/Nova/Compiler/TypeChecker.purs`

```purescript
inferPat env (PatCons head tail) ty = do
  -- 1. Create fresh type var for element
  -- 2. Unify ty with Array elemTy
  -- 3. Infer head with elemTy
  -- 4. Infer tail with Array elemTy
```

**Test case**:
```purescript
-- Input
case xs of
  (h : t) -> h
  [] -> 0

-- Should infer:
-- (h : t) :: Array Int
-- h :: Int, t :: Array Int
```

### 1.4 PatAs
**File**: `src/Nova/Compiler/TypeChecker.purs`

```purescript
inferPat env (PatAs name pat) ty = do
  -- 1. Infer inner pattern with ty
  -- 2. Bind name to ty in environment
```

**Test case**:
```purescript
-- Input
case mx of
  x@(Just y) -> x
  Nothing -> Nothing

-- Should infer:
-- x :: Maybe a
-- y :: a
```

---

## Task 2: Add Prelude Operators

### 2.1 Update builtinPrelude in Types.purs

**Current** (line 145-158):
```purescript
builtinPrelude :: Map String Scheme
builtinPrelude = Map.fromFoldable
  [ Tuple "Int" (mkScheme [] tInt)
  , Tuple "String" (mkScheme [] tString)
  -- ... types only
  ]
```

**Add operators**:
```purescript
builtinPrelude :: Map String Scheme
builtinPrelude = Map.fromFoldable
  -- Types
  [ Tuple "Int" (mkScheme [] tInt)
  , Tuple "String" (mkScheme [] tString)
  , Tuple "Char" (mkScheme [] tChar)
  , Tuple "Bool" (mkScheme [] tBool)
  -- Arithmetic
  , Tuple "+" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "-" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "*" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  , Tuple "/" (mkScheme [] (tArrow tInt (tArrow tInt tInt)))
  -- Comparison
  , Tuple "==" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  , Tuple "/=" (mkScheme [a] (tArrow (TyVar a) (tArrow (TyVar a) tBool)))
  , Tuple "<" (mkScheme [] (tArrow tInt (tArrow tInt tBool)))
  , Tuple ">" (mkScheme [] (tArrow tInt (tArrow tInt tBool)))
  , Tuple "<=" (mkScheme [] (tArrow tInt (tArrow tInt tBool)))
  , Tuple ">=" (mkScheme [] (tArrow tInt (tArrow tInt tBool)))
  -- Boolean
  , Tuple "&&" (mkScheme [] (tArrow tBool (tArrow tBool tBool)))
  , Tuple "||" (mkScheme [] (tArrow tBool (tArrow tBool tBool)))
  , Tuple "not" (mkScheme [] (tArrow tBool tBool))
  -- String
  , Tuple "<>" (mkScheme [] (tArrow tString (tArrow tString tString)))
  -- List
  , Tuple ":" (mkScheme [a] (tArrow (TyVar a) (tArrow (tArray (TyVar a)) (tArray (TyVar a)))))
  -- Function
  , Tuple "$" (mkScheme [a, b] (tArrow (tArrow (TyVar a) (TyVar b)) (tArrow (TyVar a) (TyVar b))))
  ]
  where
    a = mkTVar (-1) "a"
    b = mkTVar (-2) "b"
```

---

## Task 3: E2E Tests

### 3.1 Pattern Matching Tests

Add to `test/codegen_elixir/E2ETest.purs`:

```purescript
-- Test: List pattern
testElixir "list_pattern"
  { name: "Test.ListPat"
  , declarations:
      [ DeclFunction
          { name: "sum3"
          , parameters: [PatList [PatVar "a", PatVar "b", PatVar "c"]]
          , body: ExprBinOp "+" (ExprBinOp "+" (ExprVar "a") (ExprVar "b")) (ExprVar "c")
          , typeSignature: Nothing
          }
      , DeclFunction
          { name: "main"
          , parameters: []
          , body: ExprApp (ExprVar "sum3") (ExprList [ExprLit (LitInt 1), ExprLit (LitInt 2), ExprLit (LitInt 3)])
          , typeSignature: Nothing
          }
      ]
  }
  "Test.ListPat.main()"
  "6"

-- Test: Cons pattern
testElixir "cons_pattern"
  { name: "Test.ConsPat"
  , declarations:
      [ DeclFunction
          { name: "head"
          , parameters: [PatCons (PatVar "h") (PatVar "_t")]
          , body: ExprVar "h"
          , typeSignature: Nothing
          }
      , DeclFunction
          { name: "main"
          , parameters: []
          , body: ExprApp (ExprVar "head") (ExprList [ExprLit (LitInt 42), ExprLit (LitInt 0)])
          , typeSignature: Nothing
          }
      ]
  }
  "Test.ConsPat.main()"
  "42"

-- Test: Record pattern
testElixir "record_pattern"
  { name: "Test.RecPat"
  , declarations:
      [ DeclFunction
          { name: "getX"
          , parameters: [PatRecord [Tuple "x" (PatVar "x")]]
          , body: ExprVar "x"
          , typeSignature: Nothing
          }
      , DeclFunction
          { name: "main"
          , parameters: []
          , body: ExprApp (ExprVar "getX") (ExprRecord [Tuple "x" (ExprLit (LitInt 99)), Tuple "y" (ExprLit (LitInt 1))])
          , typeSignature: Nothing
          }
      ]
  }
  "Test.RecPat.main()"
  "99"
```

### 3.2 Operator Tests

```purescript
-- Test: Comparison operators
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
          , typeSignature: Nothing
          }
      ]
  }
  "Test.Compare.main()"
  "1"

-- Test: Boolean operators
testElixir "boolean_ops"
  { name: "Test.BoolOps"
  , declarations:
      [ DeclFunction
          { name: "main"
          , parameters: []
          , body: ExprBinOp "&&" (ExprLit (LitBool true)) (ExprLit (LitBool false))
          , typeSignature: Nothing
          }
      ]
  }
  "Test.BoolOps.main()"
  "false"
```

---

## Task 4: Fix CodeGen for Patterns

### 4.1 Cons Pattern Generation

Update `genPattern` in `CodeGen.purs`:

```purescript
genPattern (PatCons head tail) =
  "[" <> genPattern head <> " | " <> genPattern tail <> "]"
```

### 4.2 Record Pattern Generation

```purescript
genPattern (PatRecord fields) =
  "%{" <> intercalate ", " (map genFieldPat fields) <> "}"
  where
    genFieldPat (Tuple label pat) =
      snakeCase label <> ": " <> genPattern pat
```

---

## Checklist

- [ ] Implement `inferPat` for `PatRecord`
- [ ] Implement `inferPat` for `PatList`
- [ ] Implement `inferPat` for `PatCons`
- [ ] Implement `inferPat` for `PatAs`
- [ ] Add arithmetic operators to prelude
- [ ] Add comparison operators to prelude
- [ ] Add boolean operators to prelude
- [ ] Add list operators to prelude
- [ ] Add E2E test for list pattern
- [ ] Add E2E test for cons pattern
- [ ] Add E2E test for record pattern
- [ ] Add E2E test for comparison ops
- [ ] Add E2E test for boolean ops
- [ ] Verify all existing tests still pass

---

## Success Criteria

After Sprint 1, this program should compile and run:

```purescript
module Main where

sum :: Array Int -> Int
sum xs = case xs of
  [] -> 0
  (h : t) -> h + sum t

main :: Int
main = sum [1, 2, 3, 4, 5]
-- Output: 15
```

Generated Elixir:
```elixir
defmodule Main do
  def sum(xs) do
    case xs do
      [] -> 0
      [h | t] -> h + sum(t)
    end
  end

  def main() do
    sum([1, 2, 3, 4, 5])
  end
end

IO.inspect(Main.main())
# Output: 15
```
