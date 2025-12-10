# Source Structure

## Directories

```
src/Nova/Compiler/    # PureScript source (edit here)
nova_lang/            # Generated Elixir (never edit directly)
test/                 # PureScript tests
test/parser/          # Parser-specific tests
```

## Compiler Modules

| Module | Purpose |
|--------|---------|
| `Tokenizer.purs` | Lexical analysis → tokens with positions |
| `Parser.purs` | Recursive descent → AST |
| `Ast.purs` | AST types: Declaration, Expr, Pattern, TypeExpr |
| `Types.purs` | Internal types: TVar, TCon, TyRecord, substitutions |
| `TypeChecker.purs` | Hindley-Milner inference (Algorithm W) |
| `Unify.purs` | Robinson unification |
| `CodeGen.purs` | Typed AST → Elixir |
| `Dependencies.purs` | Dependency graph |
| `Regenerate.purs` | Self-compilation orchestration |

## Key Types

- `TypeExpr` = surface syntax (what user writes)
- `Type` = internal representation (type checker uses)
- `Declaration` = top-level (functions, data, newtypes, imports, infix)
- `Expr` = expressions (literals, vars, app, lambda, case, do, let)
- `Pattern` = pattern matching (vars, constructors, records, lists)
