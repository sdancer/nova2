# Nova Compiler Backlog

## Self-Hosting Issues Resolved

These issues were fixed to enable the self-hosted Nova compiler to type-check and compile its own source code:

### Forward Reference Issues (Completed)
The self-hosted type checker processes declarations sequentially, so functions defined in `where` clauses must be moved to top-level before they are used.

1. **Unify.purs** - Moved `unifyStep`, `unifyField` out of where clauses
2. **CstLayout.purs** - Moved `isIndented` before `currentIndentGo`
3. **CstLexer.purs** - Moved layout functions before `lexModule`, added local `isIndented`/`lytToken`
4. **TypeChecker.purs** - Refactored multiple `go` functions to top-level:
   - `instantiateGo`
   - `inferManyGo`
   - `inferElemsGo`
   - `inferFieldsGo`
   - `inferRecordPatGo`
   - `inferConPatsGo`
   - `inferClausesGo`, `inferGuard`, `inferGuardExpr`, `exprToPattern`
   - `buildConstructorTypeGo`, `buildConstructorTypeWithAliasesGo`
   - `checkFunctionBodiesGo`
5. **CodeGenWasmSimple.purs** - Moved `collectDoStmtRefs` before `collectExternalRefs`

### Type Annotation Issues (Completed)
1. **Unify.purs** - Changed `row :: _` to `row :: Maybe TVar` (self-hosted doesn't support type wildcards)

### Parser Issues (Completed)
1. **CstParser.purs** - Added `parseImportOp` for operator imports like `(:)`
2. **CstParser.purs** - Added `tokColon` for single colon in record literals `{ foo: 1 }`

## Outstanding Issues

### Parameterized Type Alias Expansion (In Progress)
The self-hosted type checker doesn't expand parameterized type aliases like:
- `Wrapped a = { open :: SourceToken, value :: a, close :: SourceToken }`
- `Labeled a b = { label :: a, separator :: SourceToken, value :: b }`
- `Separated a = { head :: a, tail :: Array (Tuple SourceToken a) }`

**Affected files:**
- CstToAst.purs - Partially fixed by manually expanding type signatures
- CstPipeline.purs - Depends on CstToAst

**Current approach:** Manually expand type aliases in function signatures.
**Long-term fix:** Add parameterized type alias expansion to TypeChecker.purs

### Modules Not Yet Compiling
- CstToAst (Wrapped/Labeled type alias issue)
- CstPipeline (depends on CstToAst)

### Modules Successfully Compiling
- Ast (297 lines)
- Types (14 lines)
- Cst (282 lines)
- CstLayout (34 lines)
- CstLexer (18 lines)
- CstParser (9 lines)
- Tokenizer (41 lines)
- Parser (32 lines)
- Unify (93 lines)
- TypeChecker (15 lines)
- CodeGen (12 lines)
- CodeGenCoreErlang (84 lines)
- CodeGenWasmSimple (115 lines)
- Dependencies (4 lines)

## Feature Tests

The following tests verify functionality needed for self-hosting:

- `test/typecheck/TypeAliasTest.purs` - Parameterized type alias expansion (PASS in PureScript, needed in self-hosted)

## Future Work

### Self-Hosted Type Checker Enhancements
- [ ] Support parameterized type alias expansion in self-hosted type checker
- [ ] Support type wildcards (`_`) in annotations for self-hosted
- [ ] Better error messages for unification failures

### Parser Enhancements
- [ ] Handle more complex import patterns
- [ ] Better error recovery

### Code Generation
- [ ] Complete WASM code generation
- [ ] Optimize generated Elixir code
