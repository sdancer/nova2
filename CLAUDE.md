# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Nova Lang is a self-hosted compiler that compiles a PureScript-like language to Elixir (BEAM VM). The project implements a full compiler pipeline: tokenization → parsing → type checking → code generation.

**Bootstrap Status: COMPLETE** - The compiler successfully compiles itself. Stage 1 (PureScript→JS) and Stage 2 (Elixir) produce identical output for all 9 core modules.

**Primary Goal**: Any changes must compile successfully through Stage 2. Always verify with `node scripts/bootstrap-compare.js` (must show 9/9 MATCH).

## Directory Structure

- `src/Nova/Compiler/` - **Source**: The PureScript compiler source code (bootstrap compiler)
- `nova_lang/lib/nova/compiler/` - **Compiled Output**: Elixir compiler (self-hosted)
- `nova_lang/test/purs/` - Test programs demonstrating working features
- `scripts/regenerate.js` - Recompile PureScript source to Elixir
- `scripts/bootstrap-compare.js` - Verify Stage 1 and Stage 2 produce identical output
- `scripts/test-core-erlang.js` - Test Core Erlang backend generation

## Build Commands

```bash
npx spago build                    # Build PureScript compiler (Stage 1)
node scripts/regenerate.js         # Recompile source to Elixir
node scripts/bootstrap-compare.js  # Verify bootstrap (should show 8/8 MATCH)
cd nova_lang && mix test           # Run Elixir tests
```

## Compiler Architecture

### Core Modules (~8,700 lines total)

| Module | Lines | Purpose |
|--------|-------|---------|
| **Ast.purs** | 294 | AST type definitions |
| **Tokenizer.purs** | 316 | Lexical analysis with position tracking |
| **Parser.purs** | 3,158 | Recursive descent parser |
| **Types.purs** | 440 | Internal type representation |
| **TypeChecker.purs** | 1,414 | Hindley-Milner type inference |
| **Unify.purs** | 110 | Robinson unification algorithm |
| **CodeGen.purs** | 2,279 | Elixir code generation |
| **CodeGenCoreErlang.purs** | 370 | Core Erlang code generation (alternative backend) |
| **Dependencies.purs** | 281 | Module dependency resolution |

### Pipeline Flow

```
Source Code → Tokenizer → Parser → TypeChecker → CodeGen → Elixir
                ↓            ↓           ↓            ↓
            [Token]       [AST]    [Typed AST]    [String]
```

## Language Features

### Expressions (25+ types)
- **Literals**: Int, String, Char, Bool, Number
- **Functions**: Lambda `\x -> x`, Application `f x`, Composition
- **Bindings**: Let `let x = 1 in x`, Where clauses
- **Control**: If-then-else, Case expressions with pattern matching
- **Collections**: Lists `[1,2,3]`, Tuples `(a,b)`, Records `{x: 1, y: 2}`
- **Records**: Access `r.field`, Update `{r | field = val}`
- **Operators**: Binary, Unary, Sections `(+)`, `(+1)`, `(1+)`
- **Monadic**: Do-notation with bind/let

### Patterns (10 types)
- Variable `x`, Wildcard `_`, Literal `42`
- Constructor `Just x`, Record `{x, y}`
- List `[a,b,c]`, Cons `(h:t)`
- As-pattern `x@(Just v)`

### Declarations
- Functions with type signatures
- Data types `data Maybe a = Nothing | Just a`
- Newtypes `newtype UserId = UserId Int`
- Type aliases `type Name = String`
- Type classes and instances
- Imports with selective/hiding
- Infix declarations `infixl 6 +`
- Foreign imports (FFI)

### Type System
- Hindley-Milner inference (Algorithm W)
- Polymorphism with `forall`
- Records with row types
- Type class constraints (parsing complete, resolution partial)

## Working Test Programs

```purescript
-- nova_lang/test/purs/SumTest.purs
module Test.SumTest where

sum :: Array Int -> Int
sum xs = case xs of
  [] -> 0
  (h : t) -> h + sum t

main :: Int
main = sum [1, 2, 3, 4, 5]  -- Output: 15
```

Other test files: `SimpleTest.purs`, `ParserFeatures.purs`, `EffectTest.purs`, `SectionTest.purs`

## Development Workflow

After making changes to PureScript source:

1. Build: `npx spago build`
2. Regenerate: `node scripts/regenerate.js`
3. **Verify bootstrap: `node scripts/bootstrap-compare.js` (MUST show 8/8 MATCH)**

**CRITICAL**: The primary goal is successful compilation through Stage 2. Every change must pass bootstrap verification. If Stage 1 ≠ Stage 2, the change breaks self-hosting and must be fixed before proceeding.

## Known Limitations

- Type class instance method dispatch incomplete
- Higher-rank types beyond basic `forall`
- GADTs not supported
- No optimization passes

## Key Implementation Details

### Parser Design
- Recursive descent with explicit token manipulation
- Parse results: `Either String (Tuple a (Array Token))`
- Helpers: `skipNewlines`, `expectKeyword`, `expectDelimiter`, `parseAny`, `parseSeparated`

- `test/` - PureScript tests (parser tests in `test/parser/`)
- `nova_lang/test/` - Elixir tests for compiled output (namespace service, code generation)

### Type Checker
- Fresh variable generation with naming hints
- Substitution-based unification
- Environment tracks: bindings, counter, namespace

### Code Generation
- Context tracks: local vars, module functions, arities
- Special handling for: constructors, Array functions, FFI
- Generates curried functions with partial application support

## MCP Interface (AI Agent Dev Environment)

The Nova MCP server (`nova mcp`) exposes the compiler as a service for AI agents to programmatically write, validate, and compile Nova code. This is NOT an LSP - it's a development environment designed for AI agents.

**Key Concepts:**
- **Namespaces**: In-memory workspaces for organizing code (not tied to files)
- **Declarations**: Functions, types, aliases added incrementally to namespaces
- **Immediate feedback**: Type check after each change, get structured errors
- **On-demand compilation**: Generate Elixir when ready

**Workflow for AI agents:**
1. Create a namespace: `create_namespace {name: "MyApp"}`
2. Add declarations one at a time: `add_declaration {namespace: "MyApp", source: "add x y = x + y"}`
3. Validate to check types: `validate_namespace {namespace: "MyApp"}`
4. Fix any errors based on structured feedback
5. Compile when ready: `compile_namespace {namespace: "MyApp"}`

**Compiler core operations:**
- `load_compiler_core` - Load all 8 compiler modules into namespaces
- `compile_compiler` - Compile all modules with dependencies
- `validate_compiler` - Type check all compiler modules

**Available MCP tools:** create_namespace, delete_namespace, list_namespaces, add_declaration, update_declaration, remove_declaration, list_declarations, get_declaration, validate_namespace, get_type, get_diagnostics, get_completions, add_import, list_imports, load_file, compile_file, compile_namespace, load_compiler_core, compile_compiler, validate_compiler
