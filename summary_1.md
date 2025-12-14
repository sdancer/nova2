# CodeGen Performance Fixes Summary

## Issue
The `node scripts/regenerate-purs.js` command was hanging indefinitely when compiling `Nova.Compiler.CodeGenCoreErlang.purs`. The `genModule` function, which has 18 function bindings and 24 value bindings, was causing the hang.

## Root Causes and Fixes

### 1. Exponential Evaluation Bug in `genValueBindsWithBodyStrSorted`

**Location**: `src/Nova/Compiler/CodeGenCoreErlang.purs:1267-1281`

**Problem**: The function defined both `continuation` and `continuationComplex` in a `let` block:

```purescript
let continuation = genValueBindsWithBodyStrSorted ctx rest bodyStr
    continuationComplex = genValueBindsWithBodyStrSorted nextCtx rest bodyStr
in if isSimplePattern bind.pattern
   then ... continuation
   else ... continuationComplex
```

Since PureScript is **strict**, BOTH recursive calls were evaluated even though only one was used. With 24 let bindings in `genModule`, this caused **2^24 (~16 million)** recursive calls, resulting in the hang.

**Fix**: Moved recursive calls inside the `if-then-else` branches so only the needed one is evaluated:

```purescript
if isSimplePattern bind.pattern
then "let <" <> pat <> "> = " <> val <> "\n      in " <> genValueBindsWithBodyStrSorted ctx rest bodyStr
else let tmpVar = "_Let" <> show ctx.varCounter
         nextCtx = ctx { varCounter = ctx.varCounter + 1 }
     in "let <" <> tmpVar <> "> = " <> val <> "\n      in case " <> tmpVar <> " of\n        <" <> pat <> "> when 'true' -> " <> genValueBindsWithBodyStrSorted nextCtx rest bodyStr <> "\n      end"
```

### 2. Native Cons Operator for BEAM Self-Hosting

**Location**: `src/Nova/Compiler/CodeGenCoreErlang.purs`

**Problem**: When `(:)` is used as a first-class function (e.g., `Array.foldr (:) [] arr`), the CodeGen generated:

```erlang
call 'Data.List':':'()
```

This doesn't exist on the BEAM target because our `lib/Data/List.purs` doesn't export a function named `:`.

**Fix**: Added special cases to generate native Erlang cons for:

1. **ExprVar** (imported `:` from Data.List):
   ```purescript
   if (srcMod == "Data.List" || srcMod == "Data.List.Types") && name == ":"
   then "fun (X, Xs) -> [X|Xs]"
   ```

2. **ExprQualified** (qualified reference like `Data.List.(:)`):
   ```purescript
   if (fullModName == "Data.List" || fullModName == "Data.List.Types") && funcName == ":"
   then "fun (X, Xs) -> [X|Xs]"
   ```

3. **ExprApp** (applied cons with arguments):
   ```purescript
   if (fullModName == "Data.List" || fullModName == "Data.List.Types") && funcName == ":"
   then case args of
     [h, t] -> "[" <> genExpr ctx h <> " | " <> genExpr ctx t <> "]"
     [h] -> "fun (Xs) -> [" <> genExpr ctx h <> " | Xs]"
     _ -> "fun (X, Xs) -> [X|Xs]"
   ```

Note: The lambda is **uncurried** (arity 2) because `Data.Array.foldr` expects a 2-argument function.

## Other Changes

### lib/Data/List.purs
- Added infix declaration: `infixr 6 cons as :`

### lib/Data/Set.purs
- Added `Eq` instance for Set (for BEAM target)

### src/Nova/Compiler/CstLexer.purs
- (Earlier fix) Changed O(n²) list concatenation to O(n) cons-with-reverse pattern

## Results

| Metric | Before | After |
|--------|--------|-------|
| Regeneration time | Hanging indefinitely | ~3.4s |
| Self-hosting test | Failed (undefined `Data.List.":"`) | All 15 modules pass |
| Spago tests | N/A | All 28 tests pass |

## Files Modified

- `src/Nova/Compiler/CodeGenCoreErlang.purs` - Main fixes
- `src/Nova/Compiler/CstLexer.purs` - O(n) lexer fix (earlier)
- `lib/Data/List.purs` - Infix operator declaration
- `lib/Data/Set.purs` - Eq instance
- `nova_lang/priv/core/*.core` - Regenerated Core Erlang files
