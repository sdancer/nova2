# Nova Compiler Architecture

This document describes the architecture of the Nova compiler, a self-hosted PureScript-like language that compiles to Elixir/BEAM.

## Directory Structure

```
nova2/
├── src/Nova/Compiler/     # PureScript source code (the compiler)
├── nova_lang/             # Compiled Elixir output (generated)
├── lib/                   # Library modules (e.g., Data.List)
├── scripts/               # Build and regeneration scripts
└── test/                  # PureScript tests
```

## Compiler Pipeline

```
Source Code (.purs)
       │
       ▼
┌─────────────────┐
│   Tokenizer     │  → Lexical analysis, produces tokens
└─────────────────┘
       │
       ▼
┌─────────────────┐
│   Parser        │  → Recursive descent, produces AST
└─────────────────┘
       │
       ▼
┌─────────────────┐
│  TypeChecker    │  → Hindley-Milner type inference (Algorithm W)
└─────────────────┘
       │
       ▼
┌─────────────────┐
│   CodeGen       │  → Generates Elixir code
└─────────────────┘
       │
       ▼
   Elixir Code (.ex)
```

## Core Modules

### AST & Types (`src/Nova/Compiler/`)

- **Ast.purs** - AST type definitions (Module, Declaration, Expr, Pattern, TypeExpr)
- **Types.purs** - Internal type representation (TVar, TCon, TyRecord, Scheme, Env, Subst)
- **Cst.purs** - Concrete Syntax Tree types (for CST-based parser)

### Parsing

- **Tokenizer.purs** - Lexical analysis, produces tokens with position info
- **Parser.purs** - Recursive descent parser, produces AST from tokens
- **CstLexer.purs** - CST-based lexer with layout handling
- **CstParser.purs** - CST-based parser
- **CstToAst.purs** - Converts CST to AST
- **CstPipeline.purs** - Combines CST lexer, parser, and conversion

### Type System

- **Unify.purs** - Robinson unification algorithm
- **TypeChecker.purs** - Hindley-Milner type inference with:
  - Type class support
  - Module registry for imports
  - Export extraction

### Code Generation

- **CodeGen.purs** - Primary Elixir code generator
- **CodeGenCoreErlang.purs** - Alternative Core Erlang backend
- **CodeGenWasm.purs** / **CodeGenWasmSimple.purs** - Experimental WASM backends

### Utilities

- **Dependencies.purs** - Dependency extraction and graph management

## Module System

### Import Processing

The compiler supports qualified imports with aliases:

```purescript
import Data.List as List
-- Allows using: List.reverse, List.null, etc.
```

Key functions in `TypeChecker.purs`:
- `processImportDecl` - Processes import declarations, resolves modules from registry
- `mergeExportsToEnvWithPrefix` - Adds qualified bindings (e.g., `"List.reverse"`) to environment

### Module Registry

The `ModuleRegistry` type (in `Types.purs`) stores compiled module exports:
- Type definitions
- Type aliases
- Constructor schemes
- Value schemes (functions)

### Export Extraction

`extractExports` in `TypeChecker.purs` extracts exports from declarations:
- `DeclDataType` - Extracts type info and constructor schemes
- `DeclTypeAlias` - Extracts type alias info
- `DeclForeignImport` - Extracts foreign import type signatures
- `DeclFunction` - Added after type checking via `addValuesToExports`

## Build System

### regenerate.js

The main build script (`scripts/regenerate.js`) handles:

1. **Library module compilation** - Compiles modules in `lib/` first (e.g., `Data.List`)
2. **Registry building** - Registers library exports for use by compiler modules
3. **Compiler module compilation** - Compiles all compiler modules with dependency ordering

Key features:
- Converts PureScript List to JS Array for export extraction
- Handles both Array and List declaration formats
- Builds dependency graph for proper compilation order

### Module Dependencies

```javascript
const moduleDeps = {
  'Ast.purs': [],
  'Types.purs': ['Ast.purs'],
  'TypeChecker.purs': ['Ast.purs', 'Types.purs', 'Unify.purs'],
  // ... etc
};
```

## Self-Compilation Status

The compiler can compile itself. Current status: **12/18 modules** compile successfully.

### Working Modules
- Ast, Types, Tokenizer, Parser, Unify, TypeChecker
- CodeGen, CodeGenCoreErlang, Dependencies
- Cst, CstPipeline, RefEq

### Modules with Issues
- **CstLayout, CstLexer, CstParser, CstToAst** - Parser doesn't support certain syntax
- **CodeGenWasm, CodeGenWasmSimple** - Type mismatches (experimental backends)

## Runtime

The compiled Elixir code uses runtime support modules in `nova_lang/lib/nova/`:

- **Nova.Runtime** - Core runtime functions (append, map, fold, etc.)
- **Nova.Map** - Map operations
- **Nova.Set** - Set operations
- **Nova.Array** - Array operations
- **Nova.List** - List operations (backing for Data.List)

## Type System Details

### Type Representation

```purescript
data Type
  = TyVar TVar           -- Type variable: a, b, etc.
  | TyCon TCon           -- Type constructor: Int, List a, etc.
  | TyRecord TRecord     -- Record type: { name :: String, age :: Int }

type TVar = { id :: Int, name :: String }
type TCon = { name :: String, args :: Array Type }
```

### Type Schemes

```purescript
type Scheme = { vars :: Array TVar, ty :: Type }
```

Schemes represent polymorphic types. `vars` contains the quantified type variables.

### Substitutions

Type substitutions (`Subst`) map type variable IDs to types. Key operations:
- `applySubst` - Apply substitution to a type
- `composeSubst` - Compose two substitutions
- `unify` - Unify two types, producing a substitution

### Environment

```purescript
type Env =
  { bindings :: Map String Scheme     -- Variable bindings
  , nextVar :: Int                     -- Fresh variable counter
  , typeAliases :: Map String Type     -- Type alias expansions
  }
```

## Library Modules

Library modules live in `lib/` and provide standard functionality. These are compiled first by `regenerate.js` and their exports are registered in the module registry for use by compiler modules.

### Available Library Modules

| Module | Exports | Description |
|--------|---------|-------------|
| `Data.Maybe` | 6 | Optional values (`Just`, `Nothing`, `fromMaybe`, `maybe`, `isJust`, `isNothing`) |
| `Data.Tuple` | 3 | Pairs (`Tuple`, `fst`, `snd`) |
| `Data.Either` | 3 | Error handling (`Left`, `Right`, `either`) |
| `Data.Char` | 22 | Character operations (`isAlpha`, `isDigit`, `toUpper`, `toLower`, etc.) |
| `Data.List` | 27 | List operations (`reverse`, `null`, `head`, `tail`, `map`, `filter`, etc.) |
| `Data.Array` | 53 | Array operations (`head`, `tail`, `map`, `filter`, `foldl`, `zip`, etc.) |
| `Data.String` | 28 | String operations (`length`, `take`, `drop`, `split`, `trim`, etc.) |
| `Data.Map` | 31 | Key-value maps (`lookup`, `insert`, `delete`, `union`, `keys`, etc.) |
| `Data.Set` | 24 | Unique collections (`member`, `insert`, `delete`, `union`, etc.) |
| `Data.Foldable` | 23 | Generic fold operations (`foldl`, `foldr`, `foldM`, `any`, `all`, etc.) |

### Foreign Imports

Library modules use foreign imports to map to Elixir runtime functions:

```purescript
foreign import reverse :: forall a. List a -> List a
foreign import null :: forall a. List a -> Boolean
```

The `extractExports` function in `TypeChecker.purs` extracts type signatures from `DeclForeignImport` declarations.
