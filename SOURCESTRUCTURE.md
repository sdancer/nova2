# Source Structure

## Directories

```
src/Nova/Compiler/    # PureScript source (edit here)
nova_lang/            # Generated Elixir (never edit directly)
test/                 # PureScript tests
```

## Compiler Modules

### Parsing Pipeline (CST-based)

| Module | Purpose |
|--------|---------|
| `CstLexer.purs` | Lexical analysis with layout handling |
| `CstLayout.purs` | Layout rule insertion (offside rule) |
| `CstParser.purs` | Layout-aware parser → CST |
| `CstToAst.purs` | CST → AST conversion |
| `CstPipeline.purs` | Orchestrates parsing: source → AST |
| `Cst.purs` | Concrete Syntax Tree types |

### Core Compiler

| Module | Purpose |
|--------|---------|
| `Ast.purs` | AST types: Declaration, Expr, Pattern, TypeExpr |
| `Types.purs` | Internal types: TVar, TCon, TyRecord, Scheme, Env |
| `TypeChecker.purs` | Hindley-Milner inference (Algorithm W) |
| `Unify.purs` | Robinson unification |
| `ImportProcessor.purs` | Module import resolution |

### Code Generation

| Module | Purpose |
|--------|---------|
| `CodeGen.purs` | Typed AST → Elixir |
| `CodeGenCoreErlang.purs` | Typed AST → Core Erlang |

### Infrastructure

| Module | Purpose |
|--------|---------|
| `Dependencies.purs` | Dependency graph for incremental compilation |
| `Regenerate.purs` | Self-compilation orchestration |
| `RefEq.purs` | Reference equality for expressions |

## Key Types

- `TypeExpr` = surface syntax (what user writes)
- `Type` = internal representation (type checker uses)
- `Declaration` = top-level (functions, data, newtypes, imports, infix)
- `Expr` = expressions (literals, vars, app, lambda, case, do, let)
- `Pattern` = pattern matching (vars, constructors, records, lists)
