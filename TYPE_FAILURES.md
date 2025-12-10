# Self Type-Check Failures

Current status: **8 PASS / 6 FAIL** out of 14 compiler source files.

## Summary

| File | Error Type | Root Cause |
|------|------------|------------|
| CodeGenCoreErlang.purs | Unification | Array vs String in genModule |
| CstParser.purs | Unification | Record literal vs LetBinding ADT |
| CstPipeline.purs | Missing Import | CstParser.runParser not exported |
| CstToAst.purs | Unification | Number vs Int in negate operation |
| Dependencies.purs | Unification | String vs Declaration type mismatch |
| TypeChecker.purs | Unification | Record literal vs Set ADT |

---

## Detailed Analysis

### 1. CodeGenCoreErlang.purs

**Error:**
```
Unification error in function 'genModule' -> checkFunctionBodies:
Type mismatch: Array(1 args) vs String(0 args)
```

**Location:** `src/Nova/Compiler/CodeGenCoreErlang.purs:208-212`
```purescript
genModule :: Module -> String
genModule m =
  let modName = translateModuleName m.name
      allFuncs = Array.mapMaybe getFunc (Array.fromFoldable m.declarations)
```

**Root Cause:** Type checker cannot properly infer the intermediate types in the let binding chain. The `Array.mapMaybe` returns `Array`, but somewhere the inference expects `String`. Likely issue with `Module` type not being resolved from Ast.purs registry.

**Category:** CST/AST type resolution

---

### 2. CstParser.purs

**Error:**
```
Unification error in function 'parseLet' -> checkFunctionBodies:
Type mismatch: {leadingComments, range, trailingComments, value} vs LetBinding(1 args)
```

**Location:** `src/Nova/Compiler/CstParser.purs:643-649`
```purescript
parseLet :: Parser (Cst.Expr Void)
parseLet = Control.Lazy.defer \_ -> do
  kw <- tokKeyword "let"
  bindings <- layoutBlock parseLetBinding
  inKw <- tokKeyword "in"
  body <- parseExpr
  pure (Cst.ExprLet { keyword: kw, bindings, "in": inKw, body })
```

**Root Cause:** `layoutBlock` returns something with `SourceToken` wrapper fields (`leadingComments`, `range`, `trailingComments`, `value`) but the type checker expects raw `LetBinding`. The CST uses wrapped tokens extensively, and the type checker doesn't understand the `Wrapped`/`Delimited` newtypes.

**Category:** CST type wrappers not understood

---

### 3. CstPipeline.purs

**Error:**
```
Undefined qualified import: CstParser.runParser
```

**Location:** `src/Nova/Compiler/CstPipeline.purs:18-19`
```purescript
cstResult <- CstParser.runParser CstParser.parseModule tokens
```

**Root Cause:** `CstParser.runParser` is not being exported by CstParser.purs, or the registry isn't picking it up. The function exists but isn't visible to the type checker's cross-module resolution.

**Fix Required:** Ensure `runParser` is in CstParser.purs exports and that `extractExports` captures it.

**Category:** Export/import resolution

---

### 4. CstToAst.purs

**Error:**
```
Unification error in function 'convertBinder' -> checkFunctionBodies:
Type mismatch: Number(0 args) vs Int(0 args)
```

**Location:** `src/Nova/Compiler/CstToAst.purs:972-976`
```purescript
Cst.BinderNumber neg _ num -> do
  let n' = case neg of
        Nothing -> num
        Just _ -> -num  -- <-- negate applied to Number
  pure $ Ast.PatLit (Ast.LitNumber n')
```

**Root Cause:** The `negate` operator (used via `-num`) is typed as `forall a. a -> a` in builtinPrelude. When inferring the case expression, one branch returns `Number` (num) and the other returns `Int` (from `negate` being constrained elsewhere). The polymorphic `negate` doesn't unify properly across numeric types.

**Category:** Numeric type inference / Ring typeclass

---

### 5. Dependencies.purs

**Error:**
```
Unification error in function 'getDependencies' -> checkFunctionBodies:
Type mismatch: String(0 args) vs Declaration(0 args)
```

**Location:** `src/Nova/Compiler/Dependencies.purs:22-28`
```purescript
getDependencies :: Declaration -> Set String
getDependencies decl = case decl of
  DeclFunction f ->
    let bodyDeps = getExprDeps f.body
        guardDeps = foldl (\acc g -> Set.union acc (getGuardedExprDeps g)) Set.empty f.guards
        paramNames = foldl (\acc p -> Set.union acc (getBoundNames p)) Set.empty f.parameters
    in Set.difference (Set.union bodyDeps guardDeps) paramNames
```

**Root Cause:** Pattern matching on `Declaration` ADT constructors (like `DeclFunction`) isn't properly resolved. The type checker sees `decl` as `Declaration` but can't determine that `f` in `DeclFunction f` has record type `FunctionDeclaration`. This is a fundamental issue with ADT pattern matching inference.

**Category:** ADT constructor pattern type inference

---

### 6. TypeChecker.purs

**Error:**
```
Unification error in function 'collectTypeNames' -> checkFunctionBodies:
Type mismatch: {ty, vars} vs Set(1 args)
```

**Location:** `src/Nova/Compiler/TypeChecker.purs:1163-1171`
```purescript
collectTypeNames :: Type -> Set.Set String
collectTypeNames (TyVar _) = Set.empty
collectTypeNames (TyCon tc) =
  let argsNames = Array.foldl (\s t -> Set.union s (collectTypeNames t)) Set.empty tc.args
  in Set.insert tc.name argsNames
collectTypeNames (TyRecord r) =
  let fieldNames = Map.values r.fields
      fieldsSet = Array.foldl (\s t -> Set.union s (collectTypeNames t)) Set.empty (Array.fromFoldable fieldNames)
  in fieldsSet
```

**Root Cause:** Pattern matching on `Type` ADT with `TyCon tc` extracts a record, but the type checker sees the return type annotation as `Set.Set String` and can't reconcile the intermediate `{ty, vars}` record with `Set`. The `tc` variable should have type `TCon` (which has `{name, args}` fields, not `{ty, vars}`), suggesting the TCon type definition isn't being resolved correctly.

**Category:** ADT pattern type extraction

---

## Common Themes

### 1. ADT Pattern Matching (3 failures)
Files: Dependencies.purs, TypeChecker.purs, CstParser.purs

The type checker struggles to:
- Extract record types from ADT constructor patterns
- Properly resolve field types after destructuring
- Handle polymorphic wrapper types (Wrapped, Delimited)

### 2. Cross-Module Type Resolution (1 failure)
File: CstPipeline.purs

Qualified imports aren't always resolved, even when the function exists in the source module.

### 3. Numeric Type Inference (1 failure)
File: CstToAst.purs

The polymorphic `negate` function doesn't properly constrain to `Number` or `Int` based on usage context.

### 4. Complex Expression Inference (1 failure)
File: CodeGenCoreErlang.purs

Deep let-binding chains with multiple function applications lose type information.

---

## Suggested Fixes (Priority Order)

1. **Export resolution** (CstPipeline.purs) - Ensure `runParser` is exported and captured by `extractExports`

2. **ADT constructor inference** - Improve `inferPat` in TypeChecker to properly extract record types from ADT constructor patterns

3. **Numeric type class** - Consider making `negate` Int-specific or implementing proper Ring/Num type class inference

4. **CST wrapper types** - Add understanding of Wrapped/Delimited newtypes to type inference

---

## Passing Files (8)

- Ast.purs (78 declarations)
- Cst.purs (67 declarations)
- CstLayout.purs
- CstLexer.purs
- ImportProcessor.purs (68 declarations)
- RefEq.purs
- Types.purs (244 declarations)
- Unify.purs
