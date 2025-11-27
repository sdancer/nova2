# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Nova Lang is a compiler written in PureScript that compiles a PureScript-like language to Elixir. The project implements a full compiler pipeline: tokenization → parsing → type checking → code generation.

**Bootstrapping Goal:** We are bootstrapping a self-hosted version of the compiler that will run on the BEAM VM after transcompiling to Elixir. The current PureScript implementation serves as the bootstrap compiler.

## Build Commands

```bash
npx spago build       # Build the project
npx spago test        # Run tests
npx spago repl        # Start interactive REPL
```

## Architecture

The compiler lives in `src/Nova/Compiler/` with these core modules:

- **Tokenizer.purs** - Lexical analysis producing tokens with position info
- **Parser.purs** - Recursive descent parser producing AST from tokens
- **Ast.purs** - AST type definitions (Module, Declaration, Expr, Pattern, TypeExpr)
- **Types.purs** - Internal type representation for type checking (TVar, TCon, TyRecord, substitutions)
- **TypeChecker.purs** - Hindley-Milner type inference (Algorithm W) with constraints
- **Unify.purs** - Robinson unification algorithm for types
- **CodeGen.purs** - Elixir code generation from typed AST

### Key Types

The AST distinguishes between:
- `Declaration` - Top-level items (functions, data types, type classes, imports)
- `Expr` - Expressions (literals, variables, application, lambdas, case, do, let)
- `Pattern` - Pattern matching (variables, constructors, records, lists)
- `TypeExpr` - Surface syntax types (what user writes)
- `Type` - Internal type representation (used by type checker)

### Parser Design

The parser uses explicit token manipulation with helpers like `skipNewlines`, `expectKeyword`, `expectDelimiter`. Parse results are `Either String (Tuple a (Array Token))`.

## Test Structure

Tests are in `test/` with many parser test files in `test/parser/`. The main test entry point is `test/Main.purs`.
